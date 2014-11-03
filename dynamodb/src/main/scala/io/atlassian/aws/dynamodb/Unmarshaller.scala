package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import kadai.Invalid
import scalaz.Monad, scalaz.syntax.all._

/**
 * Type class for unmarshalling objects from a map returned from AWS DynamoDB client
 * @tparam A The type of the object to unmarshall
 */
trait Unmarshaller[A] {
  final def fromMap(m: Map[String, AttributeValue]): Attempt[A] =
    unmarshall(m)

  def unmarshall: Unmarshaller.Operation[A]
}

object Unmarshaller {
  def apply[A: Unmarshaller] =
    implicitly[Unmarshaller[A]]

  def from[A](f: Operation[A]) = new Unmarshaller[A] {
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
      flatMap(f andThen Operation.ok)

    def flatMap[B](f: A => Operation[B]): Operation[B] =
      Operation {
        m => run(m) >>= { f(_)(m) }
      }
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
    def get[A](name: String)(implicit converter: Decoder[A]): Operation[A] =
      Operation { m =>
        (m.get(name) |> converter).lift {
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
}
