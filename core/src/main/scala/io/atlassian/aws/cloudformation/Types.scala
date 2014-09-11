package io.atlassian.aws.cloudformation

import scalaz.{ Tag, @@ }

trait Types {
  sealed trait StackNameMarker
  type StackName = String @@ StackNameMarker

  object StackName {
    def apply(name: String): StackName =
      Tag(name)
  }

  sealed trait StackOperationIdMarker
  type StackOperationId = String @@ StackOperationIdMarker

  object StackOperationId {
    def apply(id: String): StackOperationId =
      Tag(id)
  }
}
