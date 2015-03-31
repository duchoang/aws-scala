package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scalaz.Contravariant, scalaz.syntax.id._

/**
 * Marshall objects into a map suitable for passing to AWS DynamoDB client
 * @tparam A The type of the object to marshall.
 */
private[dynamodb] case class Marshaller[A](run: A => KeyValue) {
  /**
   * Generates a map of field (i.e. column) names to a possible AttributeValue.
   * If the value is None, we assume that the value is deleted.
   */
  def toMap(a: A): KeyValue =
    run(a)

  def toFlattenedMap(a: A): Map[String, AttributeValue] =
    toMap(a).collect {
      case (key, Some(value)) => key -> value
    }

  def contramap[B](f: B => A): Marshaller[B] =
    Marshaller { b => toMap(f(b)) }

  def liftOption: Marshaller[Option[A]] =
    Marshaller { _.fold(Map.empty[String, Value]) { run } }
}

private[dynamodb] object Marshaller {
  implicit object MarshallerContra extends Contravariant[Marshaller] {
    def contramap[A, B](ma: Marshaller[A])(f: B => A): Marshaller[B] = ma contramap f
  }
}
