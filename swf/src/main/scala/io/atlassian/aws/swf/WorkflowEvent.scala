package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.model.{ EventType, HistoryEvent }
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.collection.JavaConverters._

import scalaz.syntax.std.option._

sealed trait WorkflowEvent {
  def timestamp: DateTime
  def id: EventId
}

object WorkflowEvent {
  case class WorkflowExecutionStarted(timestamp: DateTime, id: EventId, details: WorkflowExecutionStarted.Details) extends WorkflowEvent
  object WorkflowExecutionStarted {
    case class Details(childPolicy: ChildPolicy, taskList: TaskList,
                       workflow: Workflow, input: Option[String], continuedExecutionRunId: Option[RunId],
                       executionStartToCloseTimeout: Option[Duration], parentInitiatedEventId: Option[EventId],
                       parentWorkflowExecution: Option[WorkflowInstance], tags: List[String],
                       taskPriority: Option[Int], taskStartToCloseTimeout: Option[Duration])
  }

  case class ActivityScheduled(activity: Activity, timestamp: DateTime, id: EventId, details: ActivityScheduled.Details) extends WorkflowEvent
  object ActivityScheduled {
    case class Details(activityId: ActivityId, taskList: TaskList, taskPriority: Option[Int], control: Option[String],
                       input: Option[String], triggeringDecision: EventId, heartbeatTimeout: Option[Duration],
                       scheduleToCloseTimeout: Option[Duration], scheduleToStartTimeout: Option[Duration],
                       startToCloseTimeout: Option[Duration])
  }

  case class ActivityStarted(timestamp: DateTime, id: EventId, scheduledEventId: EventId, scheduledEvent: Option[ActivityScheduled]) extends WorkflowEvent
  case class ActivityCompleted(scheduledEvent: Option[ActivityScheduled], result: Option[String], timestamp: DateTime, id: EventId, scheduledEventId: EventId) extends WorkflowEvent
  case class ActivityFailed(timestamp: DateTime, id: EventId, reason: Option[String], detail: Option[String], scheduledEventId: EventId, scheduledEvent: Option[ActivityScheduled]) extends WorkflowEvent
  case class TimerStarted(timestamp: DateTime, id: EventId, timerId: TimerId, timeout: FiniteDuration, triggeringDecision: EventId, control: Option[String]) extends WorkflowEvent
  case class TimerFired(startedEvent: Option[TimerStarted], timestamp: DateTime, id: EventId, timerId: TimerId, startedEventId: EventId) extends WorkflowEvent

  // TODO - finish mapping all events and get rid of this
  case class UnknownEvent(timestamp: DateTime, id: EventId, event: HistoryEvent) extends WorkflowEvent

  def apply(h: HistoryEvent): WorkflowEvent =
    EventType.fromValue(h.getEventType) match {
      case EventType.WorkflowExecutionStarted =>
        val attr = h.getWorkflowExecutionStartedEventAttributes
        WorkflowExecutionStarted(h.getEventTimestamp.dateTime, h.getEventId, WorkflowExecutionStarted.Details(
          ChildPolicy.unapply(attr.getChildPolicy) | ChildPolicy.Terminate, TaskList(attr.getTaskList),
          Workflow(attr.getWorkflowType), Option(attr.getInput), Option(attr.getContinuedExecutionRunId).map { RunId.apply },
          attr.getExecutionStartToCloseTimeout.safeSecs, Option(attr.getParentInitiatedEventId).map { _.toLong }, Option(attr.getParentWorkflowExecution).map { WorkflowInstance.apply },
          Option(attr.getTagList).map { _.asScala.toList } | Nil, attr.getTaskPriority.safeInt, attr.getTaskStartToCloseTimeout.safeSecs))

      case EventType.ActivityTaskScheduled =>
        val attr = h.getActivityTaskScheduledEventAttributes
        ActivityScheduled(Activity(attr.getActivityType), h.getEventTimestamp.dateTime, h.getEventId,
          ActivityScheduled.Details(ActivityId(attr.getActivityId), TaskList(attr.getTaskList),
            attr.getTaskPriority.safeInt, Option(attr.getControl), Option(attr.getInput),
            attr.getDecisionTaskCompletedEventId, attr.getHeartbeatTimeout.safeSecs,
            attr.getScheduleToCloseTimeout.safeSecs, attr.getScheduleToStartTimeout.safeSecs, attr.getStartToCloseTimeout.safeSecs))
      case EventType.ActivityTaskStarted =>
        val attr = h.getActivityTaskStartedEventAttributes
        ActivityStarted(h.getEventTimestamp.dateTime, h.getEventId, attr.getScheduledEventId, None)
      case EventType.ActivityTaskCompleted =>
        val attr = h.getActivityTaskCompletedEventAttributes
        ActivityCompleted(None, Option(attr.getResult), h.getEventTimestamp.dateTime, h.getEventId, attr.getScheduledEventId)
      case EventType.ActivityTaskFailed =>
        val attr = h.getActivityTaskFailedEventAttributes
        ActivityFailed(h.getEventTimestamp.dateTime, h.getEventId, Option(attr.getReason), Option(attr.getDetails), attr.getScheduledEventId, None)
      case EventType.TimerStarted =>
        val attr = h.getTimerStartedEventAttributes
        TimerStarted(h.getEventTimestamp.dateTime, h.getEventId, TimerId(attr.getTimerId), attr.getStartToFireTimeout.secsOr(0 seconds), attr.getDecisionTaskCompletedEventId, Option(attr.getControl))

      case EventType.TimerFired =>
        val attr = h.getTimerFiredEventAttributes
        TimerFired(None, h.getEventTimestamp.dateTime, h.getEventId, TimerId(attr.getTimerId), attr.getStartedEventId)

      case _ => UnknownEvent(h.getEventTimestamp.dateTime, h.getEventId, h)
    }

  def apply(e: WorkflowEvent, others: List[WorkflowEvent]): WorkflowEvent = {

    def find[A <: WorkflowEvent](id: EventId, constructor: PartialFunction[WorkflowEvent, A]): WorkflowEvent =
      others.find { _.id == id }.collect { constructor }.map { identity[WorkflowEvent] } | e

    e match {
      case a @ ActivityStarted(_, _, scheduledEvent, None) =>
        find(scheduledEvent, {
          case e @ ActivityScheduled(_, _, _, _) => a.copy(scheduledEvent = e.some)
        })
      case a @ ActivityCompleted(None, _, _, _, scheduledEvent) =>
        find(scheduledEvent, {
          case e @ ActivityScheduled(_, _, _, _) => a.copy(scheduledEvent = e.some)
        })
      case a @ ActivityFailed(_, _, _, _, scheduledEvent, None) =>
        find(scheduledEvent, {
          case e @ ActivityScheduled(_, _, _, _) => a.copy(scheduledEvent = e.some)
        })
      case t @ TimerFired(None, _, _, _, startedEvent) =>
        find(startedEvent, {
          case e @ TimerStarted(_, _, _, _, _, _) => t.copy(startedEvent = e.some)
        })
      case _ => e
    }
  }
}

