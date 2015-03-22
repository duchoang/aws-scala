package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import kadai.Invalid
import scalaz.{ Functor, Monad }, scalaz.std.option._, scalaz.syntax.all._, scalaz.syntax.std.either._

/**
 * Type class for unmarshalling objects from a map returned from AWS DynamoDB client
 * @tparam A The type of the object to unmarshall
 */
trait Unmarshaller[A] {
  def unmarshall: Unmarshaller.Operation[A]

  def map[B](f: A => B): Unmarshaller[B] =
    Unmarshaller.from { unmarshall.map(f) }

  final def fromMap(m: Map[String, AttributeValue]): Attempt[A] =
    unmarshall(m)

  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  private[dynamodb] def option(map: java.util.Map[String, AttributeValue]): DynamoDBAction[Option[A]] = {
    import collection.JavaConverters._
    if (map == null)
      DynamoDBAction.ok(none[A])
    else
      map.asScala.toMap |> fromMap |> { _.map(some) } |> DynamoDBAction.attempt
  }

  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  private[dynamodb] def unmarshall(map: java.util.Map[String, AttributeValue]): DynamoDBAction[A] = {
    import collection.JavaConverters._
    if (map == null)
      DynamoDBAction.fail("No values to unmarshall")
    else
      map.asScala.toMap |> fromMap |> DynamoDBAction.attempt
  }
}

object Unmarshaller {
  def from[A](f: Operation[A]) =
    new Unmarshaller[A] {
      def unmarshall = f
    }

  /**
   * Represents an unmarshalling operation that tries to convert a map of field names to AttributeValues into an object of
   * type A. Use the convenience functions in the companion object to create Unmarshaller.Operations.
   * @param run The operation
   * @tparam A The object to unmarshall into.
   */
  case class Operation[A](run: Map[String, AttributeValue] => Attempt[A]) {
    def apply(m: Map[String, AttributeValue]): Attempt[A] =
      run(m)

    def map[B](f: A => B): Operation[B] =
      Operation { run(_).map(f) }

    def flatMap[B](f: A => Operation[B]): Operation[B] =
      Operation { m => run(m) >>= { f(_)(m) } }
  }

  object Operation {
    def ok[A](strict: A): Operation[A] = Operation { _ => Attempt.ok(strict) }
    def fail[A](error: Invalid): Operation[A] = Operation { _ => Attempt(error.left) }

    /**
     * Main convenience method for creating an [[Operation]].
     * @param name The name of the field to extract.
     * @param converter Implicit unmarshaller for a specific AttributeValue to the field.
     *                  See [[Decoder]] for currently supported field types.
     * @tparam A The type of the field to unmarshall.
     * @return Unmarshall operation for unmarshalling a field in the object.
     */
    def get[A: Decoder](name: String): Operation[A] =
      Operation { m =>
        (m.get(name) |> Decoder[A].decode).lift {
          _.leftMap { _ |+| Invalid.Message(s"Cannot decode field $name") }
        }
      }

    implicit def OperationMonad: Monad[Operation] =
      new Monad[Operation] {
        def point[A](v: => A) = Operation.ok(v)
        def bind[A, B](m: Operation[A])(f: A => Operation[B]) = m flatMap f
        override def map[A, B](m: Operation[A])(f: A => B) = m map f
      }
  }

  implicit object UnmarshallerFunctor extends Functor[Unmarshaller] {
    def map[A, B](fa: Unmarshaller[A])(f: A => B) = fa map f
  }
}
