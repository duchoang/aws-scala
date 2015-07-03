package io.atlassian.aws
package dynamodb

import argonaut._, Argonaut._
import org.joda.time.{ DateTimeZone, DateTime, Instant }

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scodec.bits.ByteVector

import scalaz.Free.Trampoline
import scalaz.{ Traverse, Foldable, Functor }
import scalaz.syntax.id._

/**
 * Represents a function that tries to convert an AttributeValue into a
 * Scala value (typically that represents a field in an object).
 */
case class Decoder[A] private[Decoder] (run: Value => Attempt[A])(private[dynamodb] val dynamoType: Underlying.Type) {
  def decode(o: Value): Attempt[A] =
    run(o)

  def map[B](f: A => B): Decoder[B] =
    Decoder { run(_).map(f) }(dynamoType)

  def collect[B](f: A => Option[B]): Decoder[B] =
    Decoder {
      run(_).flatMap { a => f(a).fold(Attempt.fail[B](s"'$a' is an invalid value"))(Attempt.ok) }
    }(dynamoType)

  def collect[B](f: PartialFunction[A, B]): Decoder[B] =
    collect(f.lift)

  def mapAttempt[B](f: A => Attempt[B]): Decoder[B] =
    Decoder { run(_) flatMap f }(dynamoType)

  def or[AA >: A](f: => Decoder[AA]): Decoder[AA] =
    Decoder { v =>
      run(v).fold({ _ => f.decode(v) }, { a => Attempt.ok(a) })
    }(dynamoType)
}

/**
 * Contains the implicit decoders for different types.
 * Custom decoders are derived from this base set.
 */
object Decoder {
  import DynamoString.syntax._

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

  implicit val InstantDecode: Decoder[Instant] =
    Decoder[DateTime].map { _.toInstant }

  implicit val DynamoStringDecode: Decoder[DynamoString] =
    decoder(Underlying.StringType) {
      case None => Attempt.fail("No string value present")
      case Some(a) => a.getS |> { s =>
        if (s == null)
          Attempt.fail("No string value present")
        else
          Attempt.ok(DynamoString.apply(s))
      }
    }

  implicit val StringDecode: Decoder[String] =
    DynamoStringDecode.mapAttempt { _.asString.fold(Attempt.fail[String]("No string value present")) { Attempt.ok } }

  implicit def OptionDecode[A](implicit d: Decoder[A]): Decoder[Option[A]] =
    decoder(d.dynamoType) {
      case s @ Some(_) => d.decode(s).toOption |> Attempt.ok
      case None        => Attempt.ok(None)
    }

  implicit val NonEmptyBytesDecode: Decoder[NonEmptyBytes] =
    decoder(Underlying.BinaryType) {
      case None => Attempt.fail("No value present")
      case Some(a) => a.getB |> { bytes =>
        if (bytes == null) Attempt.fail("No value present")
        else
          NonEmptyBytes.unapply(ByteVector(bytes)).fold(Attempt.fail[NonEmptyBytes]("No value present")) { Attempt.ok }
      }
    }

  implicit val NothingDecoder: Decoder[Nothing] =
    decoder(Underlying.StringType) { _ => Attempt.fail("Cannot decode to Nothing") }
  
  implicit object DecodeAttributeValueFunctor extends Functor[Decoder] {
    def map[A, B](m: Decoder[A])(f: A => B) = m map f
  }

  private def decodeJson: Decoder[Json] =
    decoder(Underlying.StringType) {
      case None => Attempt.fail("No value present")
      case Some(a) =>
        import scala.collection.JavaConverters._
        import scalaz.syntax.traverse._, scalaz.std.list._
        lazy val optBool: Option[Json] = Option(a.getBOOL).map { b => jBool(b.booleanValue) }
        lazy val optNull: Option[Json] = Option(a.getNULL).flatMap { b => if (b) Some(jNull) else None }
        lazy val optNum: Option[Json] = Option(a.getN).flatMap { jNumber }
        lazy val optString: Option[Json] = Option(a.getS).flatMap { s => DynamoString(s).asString }.map { jString }
        lazy val optArray: Option[Json] =
          Option(a.getL).flatMap {
            _.asScala.toList.traverseU { av =>
              decodeJson.decode(Some(av))
            }.map { lj => jArray(lj) }.toOption
          }

        lazy val optObj: Option[Json] = Option(a.getM).flatMap {
          _.asScala.toList.traverseU {
            case (f, av) =>
              decodeJson.decode(Some(av)).map { decoded => f -> decoded }
          }.map { lj => jObjectFields(lj: _*) }.toOption
        }
        optBool orElse optNull orElse optNum orElse optString orElse optArray orElse optObj match {
          case None    => Attempt.fail("Could not parse JSON")
          case Some(j) => Attempt.ok(j)
        }
    }

  private def decodeJsonDirectEncoding[A: DecodeJson]: Decoder[A] =
    decodeJson.mapAttempt { _.as[A].fold({ case (msg, h) => Attempt.fail(msg) }, { a => Attempt.ok(a) }) }

  private def decodeJsonStringEncoding[A: DecodeJson]: Decoder[A] =
    StringDecode.mapAttempt { _.decodeEither[A].fold(Attempt.fail, Attempt.safe(_)) }

  implicit def decodeJsonDecoder[A: DecodeJson]: Decoder[A] =
    decodeJsonDirectEncoding[A] or decodeJsonStringEncoding[A]
}
