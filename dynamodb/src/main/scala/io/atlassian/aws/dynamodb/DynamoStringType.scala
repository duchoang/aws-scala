package io.atlassian.aws
package dynamodb

import scalaz._
import scalaz.syntax.id._

trait DynamoStringType {
  private[dynamodb] val EMPTY_STRING_PLACEHOLDER = 0.toChar.toString

  /**
   * A DynamoString is a string that is compatible with Dynamo, specifically an empty string is encoded as a 0 char.
   */
  type DynamoString = String @@ DynamoString.Marker
  object DynamoString extends Tagger[String] {
    override def apply(s: String): DynamoString =
      Tag {
        if (s.isEmpty) EMPTY_STRING_PLACEHOLDER else s
      }

    def asString(s: DynamoString): Option[String] =
      s.unwrap |> { unwrapped =>
        if (unwrapped == null) None
        else if (unwrapped == EMPTY_STRING_PLACEHOLDER) Some("") // TODO, nasty, nasty, nasty
        else Some(unwrapped)
      }

    object syntax {
      implicit class DynamoStringSyntax(val s: DynamoString) {
        def asString: Option[String] =
          DynamoString.asString(s)
      }
    }
  }
}
