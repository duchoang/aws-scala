package io.atlassian.aws
package dynamodb

import argonaut.DecodeJson
import org.joda.time.{ DateTimeZone, DateTime }

import com.amazonaws.services.dynamodbv2.model.AttributeValue

import scalaz.{ Bind, Functor }
import scalaz.syntax.id._
import argonaut.Argonaut._
import scalaz.syntax.monad.ToBindOps

/**
 * Represents a function that tries to convert an AttributeValue into a
 * Scala value (typically that represents a field in an object).
 */
case class Decoder[A] private[Decoder] (run: Value => Attempt[A])(private[dynamodb] val dynamoType: Underlying.Type) {
  def decode(o: Value): Attempt[A] =
    run(o)

  def map[B](f: A => B): Decoder[B] =
    Decoder { run(_).map(f) }(dynamoType)

  def mapPartial[B](f: PartialFunction[A, B]): Decoder[B] =
    Decoder {
      run(_).flatMap { a =>
        if (f.isDefinedAt(a))
          Attempt.ok(f(a))
        else
          Attempt.fail(s"'$a' is an invalid value")
      }
    }(dynamoType)

  def mapAttempt[B](f: A => Attempt[B]): Decoder[B] =
    Decoder { run(_) flatMap f }(dynamoType)
}

/**
 * Contains the implicit decoders for different types.
 * Custom decoders are derived from this base set.
 */
object Decoder {
  def apply[A: Decoder] =
    implicitly[Decoder[A]]

  private[Decoder] def decoder[A](typ: Underlying.Type)(f: Value => Attempt[A]): Decoder[A] =
    Decoder { f }(typ)

  def mandatoryField[A](typ: Underlying.Type)(label: String)(f: AttributeValue => A): Decoder[A] =
    decoder(typ) {
      case None     => Attempt.fail(s"No $label value present")
      case Some(av) => Attempt.safe(f(av))
    }

  // instances

  implicit val LongDecode: Decoder[Long] =
    mandatoryField(Underlying.NumberType)("Long") { _.getN.toLong }

  implicit val IntDecode: Decoder[Int] =
    mandatoryField(Underlying.NumberType)("Int") { _.getN.toInt }

  implicit val DateTimeDecode: Decoder[DateTime] =
    mandatoryField(Underlying.StringType)("DateTime") { _.getN.toLong |> { i => new DateTime(i, DateTimeZone.UTC) } }

  implicit val StringDecode: Decoder[String] =
    decoder(Underlying.StringType) {
      case None => Attempt.fail("No string value present")
      case Some(a) => a.getS |> { s =>
        if (s == null) Attempt.fail("No string value present")
        else if (s == EMPTY_STRING_PLACEHOLDER) Attempt.ok("") // TODO, nasty, nasty, nasty 
        else Attempt.ok(s)
      }
    }

  implicit def OptionDecode[A](implicit d: Decoder[A]): Decoder[Option[A]] =
    decoder(d.dynamoType) {
      case s @ Some(_) => d.decode(s).toOption |> Attempt.ok
      case None        => Attempt.ok(None)
    }

  implicit def JsonDecode[A: DecodeJson]: Decoder[A] =
    Decoder[String].mapAttempt {
      _.decodeEither[A].fold(Attempt.fail, Attempt.safe(_))
    }

  implicit object DecodeAttributeValueFunctor extends Functor[Decoder] {
    def map[A, B](m: Decoder[A])(f: A => B) = m map f
  }
}
