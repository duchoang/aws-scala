package io.atlassian.aws
package dynamodb

import java.nio.ByteBuffer
import argonaut.EncodeJson
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import org.joda.time.{ DateTimeZone, DateTime }
import scalaz.Contravariant
import scalaz.syntax.id._
import scalaz.syntax.std.option._

case class Encoder[A](run: A => Option[AttributeValue]) {
  def encode(a: A): Option[AttributeValue] =
    run(a)

  def unapply(a: A): Option[AttributeValue] =
    encode(a)

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
    Encoder[String].contramap(implicitly[EncodeJson[A]].apply(_).nospaces)

  implicit val ByteBufferEncode: Encoder[ByteBuffer] =
    attribute { b => _.withB(b) }

  implicit object EncoderContravariant extends Contravariant[Encoder] {
    def contramap[A, B](r: Encoder[A])(f: B => A) =
      r contramap f
  }
}
