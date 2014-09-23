package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scalaz.syntax.id._

/**
 * Type class for marshalling objects into a map suitable for passing to AWS DynamoDB client
 * @tparam A The type of the object to marshall.
 */
trait Marshaller[A] {
  /**
   * Generates a map of field (i.e. column) names to a possible AttributeValue.
   * If the value is None, we assume that the value is deleted.
   */
  def toMap(a: A): KeyValue

  def toFlattenedMap(a: A): Map[String, AttributeValue] =
    toMap(a).collect {
      case (key, Some(value)) => key -> value
    }
}

object Marshaller {
  def apply[A: Marshaller] =
    implicitly[Marshaller[A]]

  /**
   * Convenience method to marshall a specific field in the object.
   * Alternately, use a Column to encapsulate a named Field for reuse.
   *
   * @param name The name of the field. The name of the column in the DynamoDB table.
   * @param value The value to store.
   * @param encode Encoder for the field value to store.
   * @return A field mapping that can be used for marshalling the object
   */
  def set[A](name: String, value: A)(implicit encode: Encoder[A]): Field[A] =
    name -> encode(value)

  def from[A](toKeyValue: ToKeyValue[A]): Marshaller[A] =
    new Marshaller[A] {
      def toMap(a: A): KeyValue = toKeyValue(a)
    }

  def fromColumn[A: Encoder](column: Column[A]): Marshaller[A] =
    new Marshaller[A] {
      def toMap(a: A): KeyValue = Map(column(a))
    }

  def fromColumn2[A: Encoder, B: Encoder, C](ca: Column[A], cb: Column[B])(f: C => (A, B)): Marshaller[C] =
    new Marshaller[C] {
      def toMap(c: C): KeyValue = f(c) |> { case (a, b) => Map(ca(a), cb(b)) }
    }
}
