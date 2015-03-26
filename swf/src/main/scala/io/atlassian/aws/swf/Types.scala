package io.atlassian.aws
package swf

import scalaz.@@
import com.amazonaws.services.simpleworkflow.model.{ TaskList => ATaskList }

trait Types {
  type EventId = Long

  sealed trait DomainNameMarker
  type Domain = String @@ DomainNameMarker
  object Domain extends Tagger[String, DomainNameMarker]

  sealed trait TaskListNameMarker
  type TaskList = String @@ TaskListNameMarker
  object TaskList extends Tagger[String, TaskListNameMarker] {
    def apply(t: ATaskList): TaskList =
      TaskList(t.getName)
  }

  sealed trait TaskTokenMarker
  type TaskToken = String @@ TaskTokenMarker
  object TaskToken extends Tagger[String, TaskTokenMarker]

  sealed trait IdentityMarker
  type SWFIdentity = String @@ IdentityMarker
  object SWFIdentity extends Tagger[String, IdentityMarker]

  sealed trait WorkflowIdMarker
  type WorkflowId = String @@ WorkflowIdMarker
  object WorkflowId extends Tagger[String, WorkflowIdMarker]

  sealed trait RunIdMarker
  type RunId = String @@ RunIdMarker
  object RunId extends Tagger[String, RunIdMarker]

  sealed trait PageTokenMarker
  type EventPageToken = String @@ PageTokenMarker
  object EventPageToken extends Tagger[String, PageTokenMarker]

  sealed trait ActivityIdMarker
  type ActivityId = String @@ ActivityIdMarker
  object ActivityId extends Tagger[String, ActivityIdMarker]

  sealed trait TimerIdMarker
  type TimerId = String @@ TimerIdMarker
  object TimerId extends Tagger[String, TimerIdMarker]
}
