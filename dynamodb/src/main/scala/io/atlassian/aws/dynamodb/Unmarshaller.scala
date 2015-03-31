package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import kadai.Invalid
import scalaz.{ Functor, Monad }, scalaz.std.option._, scalaz.syntax.all._, scalaz.syntax.std.either._

/**
 * Type class for unmarshalling objects from a map returned from AWS DynamoDB client
 * @tparam A The type of the object to unmarshall
 */
// TODO this is Reader[Map[String, AttributeValue], Attempt[A]]
private[dynamodb] case class Unmarshaller[A](run: Map[String, AttributeValue] => Attempt[A]) {

  def map[B](f: A => B): Unmarshaller[B] =
    Unmarshaller { run(_).map(f) }

  def flatMap[B](f: A => Unmarshaller[B]): Unmarshaller[B] =
    Unmarshaller { m => run(m) >>= { f(_).run(m) } }

  final def fromMap(m: Map[String, AttributeValue]): Attempt[A] =
    run(m)

  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  def option(map: java.util.Map[String, AttributeValue]): Attempt[Option[A]] =
    if (map == null)
      Attempt.ok(none[A])
    else {
      import collection.JavaConverters._
      map.asScala.toMap |> fromMap |> { _.map(some) }
    }

  def liftOption: Unmarshaller[Option[A]] =
    Unmarshaller { run(_).toOption.point[Attempt] }

  //  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  def unmarshall(map: java.util.Map[String, AttributeValue]): Attempt[A] = {
    import collection.JavaConverters._
    if (map == null)
      Attempt.fail("No values to unmarshall")
    else
      map.asScala.toMap |> fromMap
  }
}

private[dynamodb] object Unmarshaller {
  /**
   * Main convenience method for creating an [[Unmarshaller]].
   * @param name The name of the field to extract.
   * @tparam A The type of the field to unmarshall.
   * @return Unmarshall operation for unmarshalling a field in the object.
   */
  def get[A: Decoder](name: String): Unmarshaller[A] =
    Unmarshaller { m =>
      (m.get(name) |> Decoder[A].decode).lift {
        _.leftMap { _ |+| Invalid.Message(s"Cannot decode field $name") }
      }
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
}
