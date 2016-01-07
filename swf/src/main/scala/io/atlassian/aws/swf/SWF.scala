package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.model._
import kadai.Invalid

import scalaz.std.list._
import scalaz.syntax.all._
import scala.collection.JavaConverters._
import com.amazonaws.services.simpleworkflow.model.{TaskList => AWSTaskList}

object SWF {
  import SWFAction._

  def register(domain: Domain, config: DomainConfig): SWFAction[Domain] =
    SWFAction.withClient {
      _.registerDomain(
        new RegisterDomainRequest()
          .withName(domain.unwrap)
          .withDescription(config.description)
          .withWorkflowExecutionRetentionPeriodInDays(
            s"${config.retentionPeriod.toDays}"
          )) |> { _ => domain }
    }.handle {
      case Invalid.Err(_: DomainAlreadyExistsException) =>
        // See if the domain is deprecated. If so, then we need to error because the domain is actually not available.
        status(domain).flatMap {
          case RegistrationStatus.DEPRECATED =>
            SWFAction.fail(new DomainDeprecatedException(domain.unwrap))
          case _ =>
            SWFAction.ok(domain)
        }
    }

  private def status(domain: Domain): SWFAction[RegistrationStatus] =
    SWFAction.withClient {
      _.describeDomain(
        new DescribeDomainRequest().withName(domain.unwrap)).getDomainInfo.getStatus |> RegistrationStatus.fromValue
    }

  private def status(domain: Domain, workflow: Workflow): SWFAction[RegistrationStatus] =
    SWFAction.withClient {
      _.describeWorkflowType(
        new DescribeWorkflowTypeRequest().withDomain(domain.unwrap).withWorkflowType(workflow.aws))
        .getTypeInfo.getStatus |> RegistrationStatus.fromValue
    }

  private def status(domain: Domain, activity: Activity): SWFAction[RegistrationStatus] =
    SWFAction.withClient {
      _.describeActivityType(
        new DescribeActivityTypeRequest().withDomain(domain.unwrap).withActivityType(activity.aws))
        .getTypeInfo.getStatus |> RegistrationStatus.fromValue
    }

  def registerWorkflow(workflow: WorkflowDefinition): SWFAction[WorkflowDefinition] =
    for {
      _ <- SWF.register(workflow.domain, workflow.domainConfig)
      _ <- SWF.register(workflow.domain, workflow.workflow, workflow.workflowConfig)
      _ <- workflow.activities[SWFAction].traverseU { a => SWF.register(workflow.domain, a.activity, a.definition) }
    } yield workflow

  def register(domain: Domain, workflow: Workflow, config: WorkflowConfig): SWFAction[Workflow] =
    SWFAction.withClient { w =>
      val req = new RegisterWorkflowTypeRequest().withDescription(config.description).withName(workflow.name)
        .withDefaultExecutionStartToCloseTimeout(config.defaultExecutionStartToCloseTimeout.secAws)
        .withDefaultTaskList(config.defaultTaskList.aws)
        .withDomain(domain.unwrap)
        .withVersion(workflow.version)
        .withDefaultTaskPriority(config.defaultTaskPriority.toString)

      config.defaultTaskStartToCloseTimeout.foreach { d => req.setDefaultTaskStartToCloseTimeout(d.secAws) }
      config.childPolicy.foreach { p => req.setDefaultChildPolicy(p.aws) }

      w.registerWorkflowType(req)
      workflow
    }.handle {
      case Invalid.Err(_: TypeAlreadyExistsException) =>
        // See if the workflow is deprecated. If so, then we need to error because the domain is actually not available.
        status(domain, workflow).flatMap {
          case RegistrationStatus.DEPRECATED =>
            SWFAction.fail(new TypeDeprecatedException(s"Workflow deprecated ${workflow.name}: ${workflow.version}"))
          case _ =>
            SWFAction.ok(workflow)
        }
    }

  def register(domain: Domain, activity: Activity, config: ActivityConfig): SWFAction[Activity] =
    SWFAction.withClient { w =>
      val req = new RegisterActivityTypeRequest().withDescription(config.description).withName(activity.name)
        .withDefaultTaskList(config.defaultTaskList.aws)
        .withDefaultTaskPriority(config.defaultTaskPriority.toString)
        .withDomain(domain.unwrap)
        .withVersion(activity.version)

      config.defaultTaskHeartbeatTimeout.foreach { d => req.setDefaultTaskHeartbeatTimeout(d.secAws) }
      config.defaultTaskScheduleToStart.foreach { d => req.setDefaultTaskScheduleToStartTimeout(d.secAws) }
      config.defaultTaskScheduleToClose.foreach { d => req.setDefaultTaskScheduleToCloseTimeout(d.secAws) }
      config.defaultTaskStartToCloseTimeout.foreach { d => req.setDefaultTaskStartToCloseTimeout(d.secAws) }
      w.registerActivityType(req)
      activity
    }.handle {
      case Invalid.Err(_: TypeAlreadyExistsException) =>
        // See if the activity is deprecated. If so, then we need to error because the domain is actually not available.
        status(domain, activity).flatMap {
          case RegistrationStatus.DEPRECATED =>
            SWFAction.fail(new TypeDeprecatedException(s"Activity deprecated ${activity.name}: ${activity.version}"))
          case _ =>
            SWFAction.ok(activity)
        }
    }

  def poll(query: DecisionQuery): SWFAction[Option[DecisionInstance]] =
    withClient { _.pollForDecisionTask(query.aws) |> DecisionInstance.unapply }

  def poll(query: ActivityQuery): SWFAction[Option[ActivityInstance]] =
    withClient { _.pollForActivityTask(query.aws) |> ActivityInstance.unapply }

  def startWorkflow(domain: Domain, workflow: Workflow, id: WorkflowId, input: String, taskList: Option[TaskList] = None): SWFAction[RunId] = {
    val request: StartWorkflowExecutionRequest =
      new StartWorkflowExecutionRequest().withDomain(domain.unwrap).withWorkflowId(id.unwrap).withWorkflowType(workflow.aws).withInput(input)

    // If taskList is not specified, SWF will use the defaultTaskList defined during workflow registration.
    taskList.foreach(tl => request.withTaskList(new AWSTaskList().withName(tl.unwrap)))

    withClient {
      _.startWorkflowExecution(request).getRunId |> RunId.apply
    }
  }


  def completeDecision(taskToken: TaskToken, context: String, decisions: List[Decision]): SWFAction[Unit] =
    withClient {
      _.respondDecisionTaskCompleted(
        new RespondDecisionTaskCompletedRequest().withTaskToken(taskToken.unwrap).withExecutionContext(context).withDecisions(decisions.map { _.aws }.asJava)
      )
    }

  def heartbeat(taskToken: TaskToken): SWFAction[ActivityTaskStatus] =
    withClient { _.recordActivityTaskHeartbeat(new RecordActivityTaskHeartbeatRequest().withTaskToken(taskToken.unwrap)) }

  def completeActivity(taskToken: TaskToken, result: String): SWFAction[Unit] =
    withClient {
      _.respondActivityTaskCompleted(new RespondActivityTaskCompletedRequest().withTaskToken(taskToken.unwrap).withResult(result))
    }

  def failActivity(taskToken: TaskToken, reason: String, detail: String): SWFAction[Unit] =
    withClient {
      _.respondActivityTaskFailed(new RespondActivityTaskFailedRequest().withTaskToken(taskToken.unwrap).withReason(reason.shortReason).withDetails(detail))
    }

  def cancelActivity(taskToken: TaskToken): SWFAction[Unit] =
    withClient {
      _.respondActivityTaskCanceled(new RespondActivityTaskCanceledRequest().withTaskToken(taskToken.unwrap))
    }
}
