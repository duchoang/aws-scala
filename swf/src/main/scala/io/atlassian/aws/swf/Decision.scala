package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.model.{ Decision => ADecision, _ }

import scala.concurrent.duration.{ FiniteDuration, Duration }
import scalaz.syntax.id._
import scala.collection.JavaConverters._

sealed trait Decision {
  def aws: ADecision
}

object Decision {
  case class ScheduleActivity(activity: Activity, id: ActivityId, input: Option[String], control: Option[String] = None,
                              scheduleToCloseTimeout: Option[Duration] = None, taskList: Option[TaskList] = None,
                              taskPriority: Option[Int] = None, scheduleToStartTimeout: Option[Duration] = None,
                              startToCloseTimeout: Option[Duration] = None, heartbeatTimeout: Option[Duration] = None) extends Decision {
    lazy val aws =
      new ADecision().withDecisionType(DecisionType.ScheduleActivityTask).withScheduleActivityTaskDecisionAttributes {
        val att = new ScheduleActivityTaskDecisionAttributes().withActivityId(id.unwrap).withActivityType(activity.aws)
        input.foreach { att.setInput }
        control.foreach { att.setControl }
        taskList.foreach { t => att.setTaskList(t.aws) }
        taskPriority.foreach { p => att.setTaskPriority(p.toString) }
        scheduleToCloseTimeout.foreach { d => att.setScheduleToCloseTimeout(d.secAws) }
        scheduleToStartTimeout.foreach { d => att.setScheduleToStartTimeout(d.secAws) }
        startToCloseTimeout.foreach { d => att.setStartToCloseTimeout(d.secAws) }
        heartbeatTimeout.foreach { d => att.setHeartbeatTimeout(d.secAws) }
        att
      }
  }
  case class RequestCancelActivity(id: ActivityId) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.RequestCancelActivityTask).withRequestCancelActivityTaskDecisionAttributes(
      new RequestCancelActivityTaskDecisionAttributes().withActivityId(id.unwrap)
    )
  }

  case class CompleteWorkflowExecution(result: Option[String]) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.CompleteWorkflowExecution).withCompleteWorkflowExecutionDecisionAttributes {
      val attr = new CompleteWorkflowExecutionDecisionAttributes()
      result.foreach { attr.setResult }
      attr
    }
  }

  case class FailWorkflowExecution(reason: String, detail: String) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.FailWorkflowExecution).withFailWorkflowExecutionDecisionAttributes(
      new FailWorkflowExecutionDecisionAttributes().withDetails(detail).withReason(reason)
    )
  }

  case class CancelWorkflowExecution(detail: String) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.CancelWorkflowExecution).withCancelWorkflowExecutionDecisionAttributes(
      new CancelWorkflowExecutionDecisionAttributes().withDetails(detail)
    )
  }

  case class ContinueAsNewWorkflowExecution(input: String, executionStartToCloseTimeout: Option[Duration] = None,
                                            taskList: TaskList, taskPriority: Option[Int] = None,
                                            taskStartToCloseTimeout: Option[Duration] = None,
                                            childPolicy: Option[ChildPolicy] = None, tags: List[String] = Nil,
                                            workflowTypeVersion: Option[String] = None) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.ContinueAsNewWorkflowExecution).withContinueAsNewWorkflowExecutionDecisionAttributes {
      val att = new ContinueAsNewWorkflowExecutionDecisionAttributes().withInput(input).withTaskList(taskList.aws).withTagList(tags.asJava)
      executionStartToCloseTimeout.foreach { d => att.setExecutionStartToCloseTimeout(d.secAws) }
      taskPriority.foreach { p => att.setTaskPriority(p.toString) }
      taskStartToCloseTimeout.foreach { d => att.setTaskStartToCloseTimeout(d.secAws) }
      childPolicy.foreach { p => att.setChildPolicy(p.aws) }
      workflowTypeVersion.foreach { att.setWorkflowTypeVersion }
      att
    }
  }

  case class RecordMarker(name: String, detail: String) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.RecordMarker).withRecordMarkerDecisionAttributes(
      new RecordMarkerDecisionAttributes().withDetails(detail).withMarkerName(name)
    )
  }

  case class StartTimer(id: TimerId, startToFire: FiniteDuration, control: Option[String] = None) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.StartTimer).withStartTimerDecisionAttributes {
      val att = new StartTimerDecisionAttributes().withTimerId(id.unwrap).withStartToFireTimeout(startToFire.secAws)
      control.foreach { att.setControl }
      att
    }
  }

  case class CancelTimer(id: TimerId) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.CancelTimer).withCancelTimerDecisionAttributes(
      new CancelTimerDecisionAttributes().withTimerId(id.unwrap)
    )
  }
  case class SignalExternalWorkflowExecution(id: WorkflowId, run: RunId, name: String, input: Option[String] = None,
                                             control: Option[String] = None) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.SignalExternalWorkflowExecution).withSignalExternalWorkflowExecutionDecisionAttributes {
      val att = new SignalExternalWorkflowExecutionDecisionAttributes().withRunId(run.unwrap).withWorkflowId(id.unwrap).withSignalName(name)
      input.foreach { att.setInput }
      control.foreach { att.setControl}
      att
    }
  }

  case class RequestCancelExternalWorkflowExecution(id: WorkflowId, run: RunId, control: Option[String] = None) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.RequestCancelExternalWorkflowExecution).withRequestCancelExternalWorkflowExecutionDecisionAttributes {
      val att = new RequestCancelExternalWorkflowExecutionDecisionAttributes().withWorkflowId(id.unwrap).withRunId(run.unwrap)
      control.foreach { att.setControl }
      att
    }
  }
  case class StartChildWorkflowExecution(id: WorkflowId, workflow: Workflow, input: String,
                                         control: Option[String] = None,
                                         executionStartToCloseTimeout: Option[Duration] = None,
                                         taskList: Option[TaskList] = None, taskPriority: Option[Int] = None,
                                         taskStartToCloseTimeout: Option[Duration] = None,
                                         childPolicy: Option[ChildPolicy] = None, tags: List[String] = Nil) extends Decision {
    lazy val aws = new ADecision().withDecisionType(DecisionType.StartChildWorkflowExecution).withStartChildWorkflowExecutionDecisionAttributes {
      val att =  new StartChildWorkflowExecutionDecisionAttributes().withWorkflowId(id.unwrap).withWorkflowType(workflow.aws)
        .withInput(input).withTagList(tags.asJava)
      control.foreach { att.setControl }
      executionStartToCloseTimeout.foreach { d => att.setExecutionStartToCloseTimeout(d.secAws) }
      taskList.foreach { t => att.setTaskList(t.aws) }
      taskPriority.foreach { p => att.setTaskPriority(p.toString) }
      taskStartToCloseTimeout.foreach { d => att.setTaskStartToCloseTimeout(d.secAws) }
      childPolicy.foreach { p => att.setChildPolicy(p.aws) }
      att
    }
  }

}
