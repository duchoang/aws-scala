package io.atlassian.aws
package swf

import scalaz.@@
import com.amazonaws.services.simpleworkflow.model.{ TaskList => ATaskList }

trait Types {
  type EventId = Long

  type Domain = String @@ Domain.Marker
  object Domain extends Tagger[String]

  type TaskList = String @@ TaskList.Marker
  object TaskList extends Tagger[String] {
    def apply(t: ATaskList): TaskList =
      TaskList(t.getName)
  }

  type TaskToken = String @@ TaskToken.Marker
  object TaskToken extends Tagger[String]

  type SWFIdentity = String @@ SWFIdentity.Marker
  object SWFIdentity extends Tagger[String]

  type WorkflowId = String @@ WorkflowId.Marker
  object WorkflowId extends Tagger[String]

  type RunId = String @@ RunId.Marker
  object RunId extends Tagger[String]

  type EventPageToken = String @@ EventPageToken.Marker
  object EventPageToken extends Tagger[String]

  type ActivityId = String @@ ActivityId.Marker
  object ActivityId extends Tagger[String]

  type TimerId = String @@ TimerId.Marker
  object TimerId extends Tagger[String]
}
