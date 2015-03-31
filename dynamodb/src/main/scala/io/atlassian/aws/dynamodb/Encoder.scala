package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import org.joda.time.{ DateTimeZone, DateTime }
import scalaz.{ Contravariant, Functor }
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

  implicit def LongEncode: Encoder[Long] =
    attribute { l => _.withN(l.toString) }

  implicit def IntEncode: Encoder[Int] =
    attribute { i => _.withN(i.toString) }

  implicit def StringEncode: Encoder[String] =
    Encoder { s =>
      // Encode an empty string as no attribute value (DynamoDB doesn't support empty string for attribute value)
      new AttributeValue().withS { if (s.isEmpty) EMPTY_STRING_PLACEHOLDER else s }.some
    }

  implicit val DateTimeEncode: Encoder[DateTime] =
    attribute { d => _.withN(d.withZone(DateTimeZone.UTC).toInstant.getMillis.toString) }

  implicit def OptionEncode[A](implicit e: Encoder[A]): Encoder[Option[A]] =
    Encoder { _.flatMap { e.run } }

  implicit object EncoderContravariant extends Contravariant[Encoder] {
    def contramap[A, B](r: Encoder[A])(f: B => A) =
      r contramap f
  }
}
