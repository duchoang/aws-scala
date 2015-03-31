package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import kadai.Invalid
import scalaz.{ Functor, Kleisli, Monad }
import scalaz.std.option._
import scalaz.syntax.all._
import scalaz.syntax.std.either._

private[dynamodb] object Unmarshaller {
  /**
   * Convenience method for creating a aimple [[Unmarshaller]].
   */
  def get[A: Decoder](name: String): Unmarshaller[A] =
    scalaz.Kleisli { m =>
      (m.get(name) |> Decoder[A].decode).lift {
        _.leftMap { _ |+| Invalid.Message(s"Cannot decode field $name") }
      }
    }

  def ok[A](strict: A): Unmarshaller[A] =
    Kleisli { _ => Attempt.ok(strict) }

  def fail[A](error: Invalid): Unmarshaller[A] =
    Kleisli { _ => Attempt(error.left) }

  implicit def UnmarshallerMonad: Monad[Unmarshaller] =
    new Monad[Unmarshaller] {
      def point[A](v: => A) = Unmarshaller.ok(v)
      def bind[A, B](m: Unmarshaller[A])(f: A => Unmarshaller[B]) = m flatMap f
      override def map[A, B](m: Unmarshaller[A])(f: A => B) = m map f
    }

  implicit class UnmarshallerOps[A](val u: Unmarshaller[A]) extends AnyVal {
    import collection.JavaConverters._

    final def fromMap(m: Map[String, AttributeValue]): Attempt[A] =
      u.run(m)

    /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
    def option(map: java.util.Map[String, AttributeValue]): Attempt[Option[A]] =
      if (map == null)
        Attempt.ok(none[A])
      else
        map.asScala.toMap |> fromMap |> { _.map(some) }

    def liftOption: Unmarshaller[Option[A]] =
      Kleisli { u.run(_).toOption.point[Attempt] }

    def unmarshall(map: java.util.Map[String, AttributeValue]): Attempt[A] =
      if (map == null)
        Attempt.fail("No values to unmarshall")
      else
        map.asScala.toMap |> fromMap
  }
}
