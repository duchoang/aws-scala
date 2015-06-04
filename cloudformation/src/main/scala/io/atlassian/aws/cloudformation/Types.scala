package io.atlassian.aws
package cloudformation

import scalaz.@@

trait Types {
  type StackName = String @@ StackName.Marker

  object StackName extends Tagger[String]

  type StackOperationId = String @@ StackOperationId.Marker

  object StackOperationId extends Tagger[String]
}
