package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scalaz.Contravariant, scalaz.syntax.id._

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
  /**
   * Convenience method to marshall a specific field in the object.
   * Alternately, use a Column to encapsulate a named Field for reuse.
   *
   * @param name The name of the field. The name of the column in the DynamoDB table.
   * @param value The value to store.
   * @param encode Encoder for the field value to store.
   * @return A field mapping that can be used for marshalling the object
   */
  def set[A](name: String, value: A)(implicit ev: Encoder[A]): Field[A] =
    name -> ev.encode(value)

  def from[A](toKeyValue: A => KeyValue): Marshaller[A] =
    new Marshaller[A] {
      def toMap(a: A): KeyValue = toKeyValue(a)
    }

  implicit object MarshallerContra extends Contravariant[Marshaller] {
    def contramap[A, B](ma: Marshaller[A])(f: B => A): Marshaller[B] =
      from { b => ma.toMap(f(b)) }
  }
  // TODO remove in 2.1
  @deprecated("use column.marshaller instead", "2.0")
  def fromColumn[A](column: Column[A]): Marshaller[A] =
    column.marshaller

  // TODO remove in 2.1
  @deprecated("use Column.compose instead", "2.0")
  def fromColumn2[A, B, C](ca: Column[A], cb: Column[B])(f: C => (A, B)): Marshaller[C] =
    Column.compose2[C](ca, cb)(f)((_: A, _: B) => ???).marshaller
}
