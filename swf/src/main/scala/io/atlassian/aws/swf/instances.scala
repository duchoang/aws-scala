package io.atlassian.aws.swf

import com.amazonaws.services.simpleworkflow.model.{ DecisionTask, ActivityTask, WorkflowExecution }

import scalaz.std.list._
import scala.collection.JavaConverters._

case class WorkflowInstance(workflowId: WorkflowId, runId: String)

object WorkflowInstance {
  def apply(w: WorkflowExecution): WorkflowInstance =
    WorkflowInstance(WorkflowId(w.getWorkflowId), w.getRunId)
}

case class ActivityInstance(activity: Activity, taskToken: TaskToken, id: String, input: Option[String], startedEventId: Long, workflow: WorkflowInstance)

object ActivityInstance {
  def unapply(a: ActivityTask): Option[ActivityInstance] =
    Option(a.getTaskToken) map { t =>
      ActivityInstance(Activity(a.getActivityType), TaskToken(t), a.getActivityId, Option(a.getInput), a.getStartedEventId, WorkflowInstance(a.getWorkflowExecution))
    }
}

case class DecisionInstance(taskToken: TaskToken, startedEventId: EventId, workflowInstance: WorkflowInstance, workflow: Workflow, previousStartedEventId: EventId, nextPageToken: Option[EventPageToken], events: List[WorkflowEvent])

object DecisionInstance {
  def unapply(d: DecisionTask): Option[DecisionInstance] =
    Option(d.getTaskToken) map { t =>
      // order list of events in ascending order
      val sortedEvents = d.getEvents.asScala.sortBy { _.getEventId }.map { WorkflowEvent.apply }
      val transformedList = sortedEvents.foldLeft(nil[WorkflowEvent]) { (acc, event) =>
        acc :+ WorkflowEvent(event, acc)
      }
      DecisionInstance(TaskToken(t), d.getStartedEventId, WorkflowInstance(d.getWorkflowExecution), Workflow(d.getWorkflowType), d.getPreviousStartedEventId,
        Option(d.getNextPageToken).map { EventPageToken.apply }, transformedList)
    }
}