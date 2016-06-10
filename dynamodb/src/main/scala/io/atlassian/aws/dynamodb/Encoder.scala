package io.atlassian.aws
package dynamodb

import java.util.UUID

import argonaut._
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import org.joda.time.{ DateTime, DateTimeZone, Instant }

import scalaz.syntax.id._
import scalaz.syntax.std.option._
import scalaz.{ @@, Contravariant, Tag }

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
    Encoder { a =>
      val n = new AttributeValue()
      f(a)(n).some
    }

  implicit val BooleanEncode: Encoder[Boolean] =
    attribute { b => _.withBOOL(b) }

  implicit val LongEncode: Encoder[Long] =
    attribute { l => _.withN(l.toString) }

  implicit val IntEncode: Encoder[Int] =
    attribute { i => _.withN(i.toString) }

  implicit val DynamoStringEncode: Encoder[DynamoString] =
    attribute { s => _.withS { s.unwrap } }

  // Encode an empty string as no attribute value (DynamoDB doesn't support empty string for attribute value)
  implicit val StringEncode: Encoder[String] =
    DynamoStringEncode.contramap { DynamoString.apply }

  implicit val DateTimeEncode: Encoder[DateTime] =
    attribute { d => _.withN(d.withZone(DateTimeZone.UTC).toInstant.getMillis.toString) }

  implicit val InstantEncode: Encoder[Instant] =
    Encoder[DateTime].contramap { _.toDateTime }

  implicit val UUIDEncode: Encoder[UUID] =
    Encoder[String].contramap { _.toString }

  implicit def OptionEncode[A: Encoder]: Encoder[Option[A]] =
    Encoder { _.flatMap { Encoder[A].run } }

  implicit def JsonEncode[A: EncodeJson]: Encoder[A] =
    JsonEncoder.encode.contramap { a => implicitly[EncodeJson[A]].encode(a) }

  implicit def TaggedTypeEncode[A: Encoder, B]: Encoder[A @@ B] =
    Encoder[A].contramap(Tag.unwrap)

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
      val a = new AttributeValue()
      json.fold(
        a.setNULL(true),
        b => a.setBOOL(b),
        n =>
          if (n.asJson.isNull)
            a.setNULL(true)
          else
            a.setN(asString(n)),
        s => a.setS { DynamoString(s).unwrap },
        array =>
          a.setL {
            array.flatMap { j =>
              encode.encode(j)
            }.asJava
          },
        obj =>
          a.setM {
            obj.toMap.flatMap {
              case (f, j) =>
                encode.encode(j).map { encoded => f -> encoded }
            }.asJava
          })
      a.some
    }

  private def asString(n: JsonNumber): String =
    n match {
      case JsonLong(v)       => v.toString
      case JsonDecimal(v)    => v
      case JsonBigDecimal(v) => v.toString
    }
}