package io.atlassian.aws
package dynamodb

import argonaut._
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import org.joda.time.{ DateTimeZone, DateTime }
import scalaz.Contravariant
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
  def encode: Encoder[Json] =
    Encoder { json =>
      new AttributeValue() <| { a =>
        json.fold(
          a.setNULL(true),
          { b => a.setBOOL(b) },
          { n =>
            if (n.asJsonOrNull.isNull)
              a.setNULL(true)
            else
              a.setN(asString(n))
          },
          { s =>
            a.setS { if (s.isEmpty) EMPTY_STRING_PLACEHOLDER else s }
          },
          { array =>
            a.setL {
              array.flatMap { j =>
                encode.encode(j)
              }.asJava
            }
          },
          { obj =>
            a.setM {
              obj.toMap.flatMap {
                case (f, j) =>
                  encode.encode(j).map { encoded => f -> encoded }
              }.asJava
            }
          }
        )
      } |> { _.some }
    }

  private def asString(n: JsonNumber): String =
    n match {
      case JsonLong(v)       => v.toString
      case JsonDouble(v)     => v.toString
      case JsonDecimal(v)    => v
      case JsonBigDecimal(v) => v.toString
    }
}