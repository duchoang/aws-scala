package io.atlassian.aws
package dynamodb

import org.joda.time.{ DateTimeZone, DateTime }

import com.amazonaws.services.dynamodbv2.model.AttributeValue

import scalaz.Monad
import scalaz.syntax.id._
import scalaz.syntax.monad.ToBindOps

/**
 * Represents a function that tries to convert an AttributeValue into a
 * Scala value (typically that represents a field in an object).
 */
case class Decoder[A] private[Decoder] (run: Value => Attempt[A]) {
  def decode(o: Value): Attempt[A] =
    run(o)

  def map[B](f: A => B): Decoder[B] =
    Decoder { run(_).map(f) }

  def flatMap[B](f: A => Decoder[B]): Decoder[B] =
    Decoder { m => run(m) >>= { f(_).decode(m) } }

  def mapPartial[B](f: PartialFunction[A, B]): Decoder[B] =
    Decoder {
      run(_).flatMap { a =>
        if (f.isDefinedAt(a))
          Attempt.ok(f(a))
        else
          Attempt.fail(s"'$a' is an invalid value")
      }
    }
}

/**
 * Contains implicit decoders for different types and useful functions for creating your own [[Decoder]]s.
 */
object Decoder {
  def apply[A: Decoder] =
    implicitly[Decoder[A]]

  private[Decoder] def option[A](f: Value => Attempt[A]): Decoder[A] =
    Decoder(f)

  def ok[A](strict: A): Decoder[A] = from { Attempt.ok(strict) }

  def from[A](a: Attempt[A]): Decoder[A] = Decoder { _ => a }

  def mandatoryField[A](f: AttributeValue => A, label: String): Decoder[A] =
    option {
      case None     => Attempt.fail(s"No $label value present")
      case Some(av) => Attempt.safe(f(av))
    }

  // instances

  implicit def LongDecode: Decoder[Long] =
    mandatoryField(_.getN.toLong, "Long")

  implicit def IntDecode: Decoder[Int] =
    mandatoryField(_.getN.toInt, "Int")

  implicit def DateTimeDecode: Decoder[DateTime] =
    mandatoryField(_.getN.toLong |> { i => new DateTime(i, DateTimeZone.UTC) }, "DateTime")

  implicit def StringDecode: Decoder[String] =
    option {
      // No attribute value means an empty string (because DynamoDB doesn't support empty strings as attribute values)
      case None => Attempt.ok("")
      case Some(av) =>
        if (av.getS == null) Attempt.fail("No string value present")
        else Attempt.ok(av.getS)
    }

  implicit def OptionDecode[A: Decoder]: Decoder[Option[A]] =
    option {
      case None                => Attempt.ok(None)
      case someValue @ Some(_) => Decoder[A].decode(someValue).toOption |> Attempt.ok
    }

  implicit def DecodeAttributeValueMonad: Monad[Decoder] =
    new Monad[Decoder] {
      def point[A](v: => A) = Decoder.ok(v)
      def bind[A, B](m: Decoder[A])(f: A => Decoder[B]) = m flatMap f
      override def map[A, B](m: Decoder[A])(f: A => B) = m map f
    }
}
