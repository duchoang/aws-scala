package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scalaz.Contravariant, scalaz.syntax.id._

/**
 * Type class for marshalling objects into a map suitable for passing to AWS DynamoDB client
 * @tparam A The type of the object to marshall.
 */
private[dynamodb] trait Marshaller[A] {
  self =>
  /**
   * Generates a map of field (i.e. column) names to a possible AttributeValue.
   * If the value is None, we assume that the value is deleted.
   */
  def toMap(a: A): KeyValue

  def toFlattenedMap(a: A): Map[String, AttributeValue] =
    toMap(a).collect {
      case (key, Some(value)) => key -> value
    }

  def contramap[B](f: B => A): Marshaller[B] =
    Marshaller.from { b => toMap(f(b)) }

  def liftOption: Marshaller[Option[A]] =
    new Marshaller[Option[A]] {
      def toMap(a: Option[A]): KeyValue = a.fold(Map.empty[String, Value]) { self.toMap }
    }
}

private[dynamodb] object Marshaller {
  def from[A](toKeyValue: A => KeyValue): Marshaller[A] =
    new Marshaller[A] {
      def toMap(a: A): KeyValue = toKeyValue(a)
    }

  implicit object MarshallerContra extends Contravariant[Marshaller] {
    def contramap[A, B](ma: Marshaller[A])(f: B => A): Marshaller[B] = ma contramap f
  }
}
