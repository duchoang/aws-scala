package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.MessageAttributeValue
import org.joda.time.{ DateTime, DateTimeZone }

import scalaz.Contravariant
import scalaz.syntax.id._
import scalaz.syntax.std.option._

/**
 * Encodes a value of type A into MessageAttributeValue
 */
case class MessageAttributeEncoder[A](run: A => Option[MessageAttributeValue]) {
  def apply(a: A): Option[MessageAttributeValue] =
    run(a)

  def unapply(a: A): Option[MessageAttributeValue] =
    this(a)

  def contramap[B](f: B => A) =
    MessageAttributeEncoder(f andThen run)
}

object MessageAttributeEncoder {
  def apply[A: MessageAttributeEncoder] =
    implicitly[MessageAttributeEncoder[A]]

  private def attribute[A](f: A => MessageAttributeValue => MessageAttributeValue): MessageAttributeEncoder[A] =
    MessageAttributeEncoder { a =>
      val v = new MessageAttributeValue()
      f(a)(v).some
    }

  implicit def LongEncode: MessageAttributeEncoder[Long] =
    attribute { l => _.withStringValue(l.toString).withDataType(FieldMainType.Number.name) }

  implicit def IntEncode: MessageAttributeEncoder[Int] =
    attribute { i => _.withStringValue(i.toString).withDataType(FieldMainType.Number.name) }

  implicit def StringEncode: MessageAttributeEncoder[String] =
    MessageAttributeEncoder { s =>
      // Encode an empty string as no attribute value
      if (s.isEmpty) None
      else new MessageAttributeValue().withStringValue(s).withDataType(FieldMainType.String.name).some
    }

  implicit def DateTimeEncode: MessageAttributeEncoder[DateTime] =
    attribute { d => _.withStringValue(d.withZone(DateTimeZone.UTC).toInstant.getMillis.toString).withDataType(HeaderTypes.MsSinceEpoch.awsFieldType) }

  implicit def OptionEncode[A](implicit e: MessageAttributeEncoder[A]): MessageAttributeEncoder[Option[A]] =
    MessageAttributeEncoder { _.flatMap { e.run } }

  implicit object EncoderContravariant extends Contravariant[MessageAttributeEncoder] {
    def contramap[A, B](r: MessageAttributeEncoder[A])(f: B => A) =
      r contramap f
  }
}
