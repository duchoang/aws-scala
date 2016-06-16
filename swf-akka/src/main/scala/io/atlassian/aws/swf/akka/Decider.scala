package io.atlassian.aws
package swf
package akka

import _root_.akka.actor.{ Props, Actor, PoisonPill }
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import kadai.log.json.JsonLogging
import SWFAction._

import scala.concurrent.duration._

case class DeciderConfig(pollDelay: FiniteDuration = 2.seconds,
                         workflow: WorkflowDefinition,
                         identity: SWFIdentity)

object Decider {
  object Protocol {
    case object Poll
  }

  def props(config: DeciderConfig, swf: AmazonSimpleWorkflow): Props =
    Props(classOf[Decider], config, swf)
}

class Decider(config: DeciderConfig, swf: AmazonSimpleWorkflow) extends Actor with JsonLogging {
  import context.dispatcher
  import io.atlassian.aws.swf.akka.Decider.Protocol._
  import kadai.log.json.JsonLogging._

  def receive = {
    case PoisonPill =>
      context.stop(self)
    case Poll =>
      poll.flatMap { od =>
        od.fold(Attempt.ok(())) { d =>
          complete(d.taskToken, "", config.workflow.decisionEngine(d))
        }
      }.run.fold(
        { i => error(i) },
        { identity }
      )
      triggerPoll
      ()
    case m =>
      println(s"Unknown message $m")
      triggerPoll
      ()
  }

  private def poll: Attempt[Option[DecisionInstance]] =
    SWF.poll(DecisionQuery(config.workflow.domain, config.workflow.workflowConfig.defaultTaskList, config.identity)).unsafePerform(swf)

  private def complete(taskToken: TaskToken, context: String, decisions: List[Decision]): Attempt[Unit] =
    SWF.completeDecision(taskToken, context, decisions).unsafePerform(swf)

  private def triggerPoll =
    context.system.scheduler.scheduleOnce(config.pollDelay, self, Poll)
}
