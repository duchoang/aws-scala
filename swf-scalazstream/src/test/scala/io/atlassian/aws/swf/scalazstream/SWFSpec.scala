package io.atlassian.aws.swf
package scalazstream

import java.util.UUID
import java.util.concurrent.{ TimeUnit, CountDownLatch, Executors }

import com.amazonaws.regions.Region
import com.amazonaws.{ ResponseMetadata, AmazonWebServiceRequest }
import com.amazonaws.services.simpleworkflow.{ AmazonSimpleWorkflowClient, model, AmazonSimpleWorkflow }
import io.atlassian.aws.swf.Decision.CompleteWorkflowExecution
import io.atlassian.aws.swf.WorkflowEvent.{ ActivityScheduled, ActivityCompleted, WorkflowExecutionStarted }
import io.atlassian.aws.{ swf, AmazonRegion, AmazonClient }
import io.atlassian.aws.spec.ScalaCheckSpec
import io.atlassian.aws.swf.{ Decision, WorkflowDefinition, SWF, DecisionInstance }
import kadai.concurrent.ThreadFactories
import kadai.log.Logging
import org.junit.runner.RunWith
import org.specs2.execute.{ Success, Failure }
import org.specs2.main.Arguments

import scala.collection.mutable
import scala.concurrent.duration._
import scalaz.concurrent.Task
import scalaz.{ \/-, -\/, Monad, @@ }
import scalaz.syntax.id._
import scalaz.syntax.std.option._

/**
 * Test for SWF.  Note that this test will connect to AWS using credentials obtained from environment variables, and will
 * register a test workflow, and exercise it.  The workflow is left enabled at the end of the test - the assumption being
 * that the test will be re-run, and/or other tests run that also exercise this workflow.
 *
 * To be run via (nb. the " -- " is important):
 *   sbt "test:testOnly *SWFSpec -- aws-integration"
 */
@RunWith(classOf[org.specs2.runner.JUnitRunner])
class SWFSpec(val arguments: Arguments) extends ScalaCheckSpec with Logging {
  import Logging._

  val IS_LOCAL = !arguments.commandLine.contains("aws-integration")
  val REGION = arguments.commandLine.value("region").getOrElse(Option(System.getenv("AWS_REGION")).getOrElse("ap-southeast-2"))

  implicit val CLIENT = AmazonClient.default[AmazonSimpleWorkflowClient] <| { _.setRegion(AmazonRegion.orDefault(REGION)) }

  def is = skipAllIf(IS_LOCAL) ^ sequential ^ stopOnFail ^ s2"""
     This specification test SWF functionality

     Create the test workflow (if required)                            ${step(createTestWorkflow)}
     Start activity pollers                                            ${step(startActivityPollers())}
     Start deciders                                                    ${step(startDeciders())}

     Post to the workflow, crash the decider                           ${postToWorkflowTriggersDeciderCrash()}
     Post to the workflow, crash the activityPoller                    ${postToWorkflowTriggersActivityPollerCrash()}
     Post to the workflow and with a failed activity                   ${postToWorkflowAndActivityFails()}
     Post to the workflow and verify activity completion               ${postToWorkflowHappyPath()}

     Shutdown threads                                                  ${step(shutdownThreads)}
   """

  // constants for communicating to our test activities what we are intending to test....
  val DeciderCrash = "deciderCrash"
  val ActivityCrash = "activityCrash"
  val ActivitySuccess = "activitySuccess"
  val ActivityFail = "activityFail"

  // nb. for testing, we have to collect activity execution results into a mutable map...
  val activityResultMap = mutable.Map[WorkflowId, Option[String]]()
  val activityExecutionLatches = mutable.Map[WorkflowId, CountDownLatch]()

  val activity1 = Activity("testActivity1", "1.0")
  def activity1Fn[F[_]: Monad]: ActivityFunction[F] = {
    activityInstance =>
      activityResultMap.put(activityInstance.workflow.workflowId, activityInstance.input)
      activityExecutionLatches(activityInstance.workflow.workflowId).countDown()

      activityInstance.input match {
        case Some(ActivityCrash)   => throw new RuntimeException("Activity crash!")
        case Some(ActivitySuccess) => ActivityResult.success("blah")
        case Some(ActivityFail)    => ActivityResult.failed("blah", "someone asked me to fail")
        case _                     => ActivityResult.failed("blah", "noone told me what to do!")
      }
  }

  // the main workflow decision function...
  def decisionFunction: DecisionFunction = {
    decisionInstance =>

      val pf: PartialFunction[WorkflowEvent, List[Decision]] = {
        case WorkflowExecutionStarted(_, _, details) if details.input.contains(DeciderCrash) =>
          activityExecutionLatches(decisionInstance.workflowInstance.workflowId).countDown()
          throw new RuntimeException("Decider crash!")

        case WorkflowExecutionStarted(_, _, details) =>
          Decision.ScheduleActivity(activity1, ActivityId("testActivity1Id"), details.input) :: Nil

        case ActivityCompleted(Some(ActivityScheduled(_, _, _, details)), _, _, _, _) =>
          Decision.CompleteWorkflowExecution(details.input) :: Nil
      }

      decisionInstance.events.notUnknown.lastOption.map(pf).getOrElse(Nil)
  }

  // warning, there are a limited total number of domains per account.  Don't go changing this repeatedly.
  val testDomain = Domain("testingDomain")
  val taskList = TaskList("taskList")
  val testWorkflow = Workflow("testWorkflow", "1.0")

  val workflowDef = new WorkflowDefinition() {
    def domain: Domain = testDomain
    def workflowConfig: WorkflowConfig = WorkflowConfig("test", taskList, defaultExecutionStartToCloseTimeout = 1.minute)
    def domainConfig: DomainConfig = DomainConfig("test", 1.hour)
    def activityTaskList: TaskList = taskList
    def workflow: Workflow = testWorkflow
    def decisionEngine: DecisionFunction = decisionFunction
    def activities[F[_]: Monad]: List[ActivityDefinition[F]] = {
      ActivityDefinition(activity1, ActivityConfig("testActivity1", taskList), activity1Fn[F]) :: Nil
    }
  }

  def createTestWorkflow = {
    val action = SWF.registerWorkflow(workflowDef)
    action.run(CLIENT).run match {
      case -\/(e) =>
        error(s"Error registering test workflow: $e")
        Failure(s"Error registering test workflow: $e")
      case \/-(task) =>
        info("Registered test workflow")
        Success
    }
  }

  // thread pools as required for SWF Decider / ActivityPollers
  val activityPollerExecutorService = Executors.newFixedThreadPool(8, ThreadFactories.named("swfActivityPoller").build)
  val activityPollerHeartbeatScheduledExecutorService = Executors.newScheduledThreadPool(8, ThreadFactories.named("swfActivityPollerSES").build)
  val deciderExecutorService = Executors.newFixedThreadPool(4, ThreadFactories.named("swfDecider").build)

  def startActivityPollers() = {
    new ActivityPoller(CLIENT, workflowDef.domain, SWFIdentity("activityPoller"), workflowDef.activityTaskList, workflowDef.activities, activityPollerExecutorService, activityPollerHeartbeatScheduledExecutorService, 5.minutes).poller(maxConcurrentActivityExecutions = 8) runAsync {
      case -\/(throwable) => error(s"Activity poller error: $throwable")
      case \/-(_)         => ()
    }
  }

  def startDeciders() = {
    val task: Task[Unit] = new Decider(CLIENT, workflowDef, SWFIdentity("decider"), deciderExecutorService).decider
    task runAsync {
      case -\/(throwable) => error(s"Decider error: $throwable")
      case \/-(_)         => ()
    }
  }

  def postToWorkflowTriggersDeciderCrash() = {
    val workflowId = WorkflowId(UUID.randomUUID().toString)
    val latch = addLatch(workflowId)

    SWF.startWorkflow(testDomain, testWorkflow, workflowId, DeciderCrash).run(CLIENT).run

    // wait for map to be updated...
    latch.await(1, TimeUnit.MINUTES)
    (latch.getCount must_=== 0L) and
      (activityResultMap.get(workflowId) must beNone)
  }

  def postToWorkflowTriggersActivityPollerCrash() = {
    val workflowId = WorkflowId(UUID.randomUUID().toString)
    val latch: CountDownLatch = addLatch(workflowId)

    SWF.startWorkflow(testDomain, testWorkflow, workflowId, ActivityCrash).run(CLIENT).run

    // wait for map to be updated...
    latch.await(1, TimeUnit.MINUTES)
    // nb. the current testing code triggers the failure AFTER putting values into the activityResultMap...
    (latch.getCount must_=== 0L) and
      (activityResultMap(workflowId) must beSome(ActivityCrash))
  }

  def postToWorkflowHappyPath() = {
    val workflowId = WorkflowId(UUID.randomUUID().toString)
    val latch = addLatch(workflowId)
    SWF.startWorkflow(testDomain, testWorkflow, workflowId, ActivitySuccess).run(CLIENT).run

    // wait for map to be updated...
    latch.await(1, TimeUnit.MINUTES)
    (latch.getCount must_=== 0L) and
      (activityResultMap(workflowId) must beSome(ActivitySuccess))
  }

  def postToWorkflowAndActivityFails() = {
    val workflowId = WorkflowId(UUID.randomUUID().toString)
    val latch = addLatch(workflowId)
    SWF.startWorkflow(testDomain, testWorkflow, workflowId, ActivityFail).run(CLIENT).run

    // wait for map to be updated...
    latch.await(1, TimeUnit.MINUTES)
    (latch.getCount must_=== 0L) and
      (activityResultMap(workflowId) must beSome(ActivityFail))
  }

  def addLatch(workflowId: WorkflowId): CountDownLatch = {
    new CountDownLatch(1) <| { activityExecutionLatches.put(workflowId, _) }
  }

  def shutdownThreads = {
    deciderExecutorService.shutdownNow()
    activityPollerExecutorService.shutdownNow()
    activityPollerHeartbeatScheduledExecutorService.shutdownNow()
  }
}
