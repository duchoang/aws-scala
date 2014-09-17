package io.atlassian.aws
package cloudformation

import scalaz.@@

trait Types {
  sealed trait StackNameMarker
  type StackName = String @@ StackNameMarker

  object StackName extends Tagger[String, StackNameMarker]

  sealed trait StackOperationIdMarker
  type StackOperationId = String @@ StackOperationIdMarker

  object StackOperationId extends Tagger[String, StackOperationIdMarker]
}
