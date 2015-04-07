package io.atlassian.aws

import java.util.Date
import java.util.concurrent.TimeUnit

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow
import com.amazonaws.services.simpleworkflow.model.{ TaskList => AWSTaskList }
import org.joda.time.DateTime

import scala.concurrent.duration.{ FiniteDuration, Duration }
import scalaz.syntax.std.option._

package object swf extends swf.Types with ActivityTypes {
  type SWFAction[A] = AwsAction[AmazonSimpleWorkflow, A]

  type DecisionFunction = DecisionInstance => List[Decision]

  implicit class TaskListSyntax(val t: TaskList) extends AnyVal {
    def aws: AWSTaskList =
      new AWSTaskList().withName(t.unwrap)
  }

  private[swf] implicit class DurationSyntax(val d: Duration) extends AnyVal {
    def secAws: String = if (!d.isFinite) "NONE" else d.toSeconds.toString
  }

  implicit class DateSyntax(val date: Date) extends AnyVal {
    def dateTime: DateTime =
      new DateTime(date)
  }

  private[swf] implicit class StringSyntax(val s: String) extends AnyVal {
    def safeSecs: Option[Duration] =
      safeInt.map { i => Duration(i.toLong, TimeUnit.SECONDS) }

    def secs: Duration =
      safeSecs | Duration.Inf

    def secsOr(default: FiniteDuration): FiniteDuration =
      safeInt.map { i => FiniteDuration(i.toLong, TimeUnit.SECONDS) } | default

    def safeInt: Option[Int] =
      try s.toInt.some
      catch { case _: Throwable => None }

    def shortReason: String =
      s.substring(0, 256)
  }

  implicit class EventListSyntax(val l: List[WorkflowEvent]) extends AnyVal {
    def notUnknown: List[WorkflowEvent] =
      l.filterNot {
        case WorkflowEvent.UnknownEvent(_, _, _) => true
        case _                                   => false
      }
  }

  implicit class DecisionSyntax(val d: Decision) extends AnyVal {
    def list: List[Decision] =
      List(d)
  }
}
