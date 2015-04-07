package io.atlassian.aws
package dynamodb

import org.joda.time.{ DateTimeZone, DateTime }

import com.amazonaws.services.dynamodbv2.model.AttributeValue

import scalaz.Functor
import scalaz.syntax.id._

/**
 * Represents a function that tries to convert an AttributeValue into a
 * Scala value (typically that represents a field in an object).
 */
case class Decoder[A] private[Decoder] (val keyType: Key.Type)(run: Value => Attempt[A]) {
  def decode(o: Value): Attempt[A] =
    run(o)

  def map[B](f: A => B): Decoder[B] =
    Decoder(keyType) { run(_).map(f) }

  def mapPartial[B](f: PartialFunction[A, B]): Decoder[B] =
    Decoder(keyType) {
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

  private[Decoder] def option[A](f: Value => Attempt[A])(keyType: Key.Type): Decoder[A] =
    Decoder(keyType) { f }

  def mandatoryField[A](f: AttributeValue => A)(label: String)(keyType: Key.Type): Decoder[A] =
    option {
      case None     => Attempt.fail(s"No $label value present")
      case Some(av) => Attempt.safe(f(av))
    }(keyType)

  // instances

  implicit val LongDecode: Decoder[Long] =
    mandatoryField { _.getN.toLong }("Long")(Key.NumberType)

  implicit val IntDecode: Decoder[Int] =
    mandatoryField { _.getN.toInt }("Int")(Key.NumberType)

  implicit val DateTimeDecode: Decoder[DateTime] =
    mandatoryField { _.getN.toLong |> { i => new DateTime(i, DateTimeZone.UTC) } }("DateTime")(Key.NumberType)

  implicit val StringDecode: Decoder[String] =
    option {
      case None => Attempt.fail("No string value present")
      case Some(a) => a.getS |> { s =>
        if (s == null) Attempt.fail("No string value present")
        else if (s == EMPTY_STRING_PLACEHOLDER) Attempt.ok("") // TODO, nasty, nasty, nasty 
        else Attempt.ok(s)
      }
    }(Key.StringType)

  implicit def OptionDecode[A](implicit decoder: Decoder[A]): Decoder[Option[A]] =
    option {
      case None                => Attempt.ok(None)
      case someValue @ Some(_) => decoder.decode(someValue).toOption |> Attempt.ok
    }(decoder.keyType)

  implicit def DecodeAttributeValueMonad: Functor[Decoder] =
    new Functor[Decoder] {
      def map[A, B](m: Decoder[A])(f: A => B) = m map f
    }
}
