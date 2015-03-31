package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import kadai.Invalid
import scalaz.{ Kleisli, Monad }
import scalaz.std.option._
import scalaz.syntax.all._

/**
 * Unmarshallers ake a given map of attribute values from AWS SDK
 * and turn that into into a value object.
 */
private[dynamodb] object Unmarshaller {
  def apply[A](f: DynamoMap => Attempt[A]): Unmarshaller[A] =
    Kleisli { f }

  /** Convenience method for creating a single column [[Unmarshaller]]. */
  def get[A: Decoder](name: String): Unmarshaller[A] =
    Unmarshaller {
      m => (m.get(name) |> Decoder[A].decode).lift { _.leftMap { _ |+| Invalid.Message(s"Cannot decode field $name") } }
    }

  def ok[A](strict: A): Unmarshaller[A] =
    Unmarshaller { _ => Attempt.ok(strict) }

  def fail[A](error: Invalid): Unmarshaller[A] =
    Unmarshaller { _ => Attempt(error.left) }

  implicit def UnmarshallerMonad: Monad[Unmarshaller] =
    new Monad[Unmarshaller] {
      def point[A](v: => A) = Unmarshaller.ok(v)
      def bind[A, B](m: Unmarshaller[A])(f: A => Unmarshaller[B]) = m flatMap f
      override def map[A, B](m: Unmarshaller[A])(f: A => B) = m map f
    }

  implicit class UnmarshallerOps[A](val unmarshaller: Unmarshaller[A]) extends AnyVal {
    def apply(m: DynamoMap) =
      unmarshaller.run(m)

    import collection.JavaConverters._

    def option(attrs: java.util.Map[String, AttributeValue]): Attempt[Option[A]] =
      if (attrs == null)
        Attempt.ok(none[A])
      else
        this(attrs.asScala.toMap).map(some)

    def liftOption: Unmarshaller[Option[A]] =
      Unmarshaller { unmarshaller.run(_).toOption.point[Attempt] }

    def unmarshall(attrs: java.util.Map[String, AttributeValue]): Attempt[A] =
      if (attrs == null)
        Attempt.fail("No values to unmarshall")
      else
        this(attrs.asScala.toMap)
  }
}
