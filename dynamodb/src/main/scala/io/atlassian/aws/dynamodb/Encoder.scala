package io.atlassian.aws
package dynamodb

import argonaut._
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import org.joda.time.{ DateTimeZone, DateTime }
import scalaz.Contravariant
import scalaz.Free.Trampoline
import scalaz.syntax.id._
import scalaz.syntax.std.option._

case class Encoder[A](run: A => Option[AttributeValue]) {
  def encode(a: A): Option[AttributeValue] =
    run(a)

  def unapply(a: A): Option[AttributeValue] =
    run(a)

  def contramap[B](f: B => A) =
    Encoder(f andThen run)
}

object Encoder {
  def apply[A: Encoder] =
    implicitly[Encoder[A]]

  private def attribute[A](f: A => AttributeValue => AttributeValue): Encoder[A] =
    Encoder { a => (new AttributeValue() <| { f(a) }).some }

  implicit val LongEncode: Encoder[Long] =
    attribute { l => _.withN(l.toString) }

  implicit val IntEncode: Encoder[Int] =
    attribute { i => _.withN(i.toString) }

  // Encode an empty string as no attribute value (DynamoDB doesn't support empty string for attribute value)
  implicit val StringEncode: Encoder[String] =
    attribute { s => _.withS { if (s.isEmpty) EMPTY_STRING_PLACEHOLDER else s } }

  implicit val DateTimeEncode: Encoder[DateTime] =
    attribute { d => _.withN(d.withZone(DateTimeZone.UTC).toInstant.getMillis.toString) }

  implicit def OptionEncode[A: Encoder]: Encoder[Option[A]] =
    Encoder { _.flatMap { Encoder[A].run } }

  implicit def JsonEncode[A: EncodeJson]: Encoder[A] =
    JsonEncoder.encode.contramap { a => implicitly[EncodeJson[A]].encode(a) }

  implicit val NonEmptyBytesEncode: Encoder[NonEmptyBytes] =
    attribute { b => _.withB(b.bytes.toByteBuffer) }

  implicit object EncoderContravariant extends Contravariant[Encoder] {
    def contramap[A, B](r: Encoder[A])(f: B => A) =
      r contramap f
  }
}

private[dynamodb] object JsonEncoder {
  import scala.collection.JavaConverters._
  import scalaz.Trampoline
  import scalaz.syntax.traverse._, scalaz.std.list._
  private def trampolinedJsonEncoder(json: Json): Trampoline[Option[AttributeValue]] =
    new AttributeValue() |> { a =>
      json.fold(
        Trampoline.done(a.withNULL(true).some),
        { b => Trampoline.done(a.withBOOL(b).some) },
        { n =>
          Trampoline.done {
            if (n.asJsonOrNull.isNull)
              a.withNULL(true).some
            else
              a.withN(asString(n)).some
          }
        },
        { s =>
          Trampoline.done(a.withS { if (s.isEmpty) EMPTY_STRING_PLACEHOLDER else s }.some)
        },
        { array =>
          array.traverse { trampolinedJsonEncoder }.map { loa => a.withL(loa.flatten.asJava).some }
        },
        { obj =>
          obj.toList.traverseU {
            case (f, j) =>
              trampolinedJsonEncoder(j).map { oa => oa.map { a => f -> a } }
          }.map { l => a.withM(l.flatten.toMap.asJava).some }
        })
    }

  def encode: Encoder[Json] =
    Encoder { json => trampolinedJsonEncoder(json).run }

  private def asString(n: JsonNumber): String =
    n match {
      case JsonLong(v)       => v.toString
      case JsonDouble(v)     => v.toString
      case JsonDecimal(v)    => v
      case JsonBigDecimal(v) => v.toString
    }
}