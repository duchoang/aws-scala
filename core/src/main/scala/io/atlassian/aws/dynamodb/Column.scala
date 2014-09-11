package io.atlassian.aws.dynamodb

import Marshaller._
import Unmarshaller.Operation._

/**
 * A specific field/column in a table. Has a name and an Encoder/Decoder to
 * prepare the encoded representation to the Dynamo driver, and to return
 * the de-serialized value back from the database, respectively.
 */
case class Column[A](name: String) {
  def apply(a: A)(implicit ev: Encoder[A]): Field[A] =
    set(name, a)

  def get(implicit ev: Decoder[A]): Unmarshaller.Operation[A] =
    Unmarshaller.Operation.get(name)
}
