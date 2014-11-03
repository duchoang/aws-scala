package io.atlassian.aws
package cloudformation

import scalaz.{ Tag, @@ }

trait Types {
  sealed trait StackNameMarker
  type StackName = String @@ StackNameMarker

  object StackName extends Tagger[String, StackNameMarker] {
    implicit def stackNameAsString(sn: StackName): String = Tag.unwrap(sn)
  }

  sealed trait StackOperationIdMarker
  type StackOperationId = String @@ StackOperationIdMarker

  object StackOperationId extends Tagger[String, StackOperationIdMarker]

}
