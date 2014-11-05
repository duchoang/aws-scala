package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.MessageAttributeValue
import org.joda.time.{DateTime, DateTimeZone}

import scalaz.Monad
import scalaz.syntax.id._
import scalaz.syntax.bind._

/**
 * Represents a function that tries to convert an AttributeValue into a
 * Scala value (typically that represents a field in an object).
 */
case class MessageAttributeDecoder[A] private[MessageAttributeDecoder] (run: Value => Attempt[A]) extends (Value => Attempt[A]) {
  def apply(o: Value): Attempt[A] =
    run(o)

  def map[B](f: A => B): MessageAttributeDecoder[B] = flatMap(f andThen MessageAttributeDecoder.ok)

  def flatMap[B](f: A => MessageAttributeDecoder[B]): MessageAttributeDecoder[B] =
    MessageAttributeDecoder { m => run(m) >>= { f(_)(m) } }
}

/**
 * Contains implicit decoders for different types and useful functions for creating your own [[MessageAttributeDecoder]]s.
 */
object MessageAttributeDecoder {
  def apply[A: MessageAttributeDecoder] =
    implicitly[MessageAttributeDecoder[A]]

  private def option[A](f: PartialFunction[Value, Attempt[A]]) =
    MessageAttributeDecoder[A](f)

  def ok[A](strict: A): MessageAttributeDecoder[A] = from { Attempt.ok(strict) }

  def from[A](a: Attempt[A]): MessageAttributeDecoder[A] = MessageAttributeDecoder { _ => a }

  def mandatory[A](typeLabel: String)(f: MessageAttributeValue => Attempt[A]): MessageAttributeDecoder[A] =
    option {
      case None     => Attempt.fail(s"Could not decode $typeLabel value")
      case Some(av) => f(av)
    }

  @deprecated("use mandatory instead", "1.0")
  def mandatoryField[A](f: MessageAttributeValue => A, typeLabel: String): MessageAttributeDecoder[A] =
    mandatory(typeLabel) { v => Attempt.safe { f(v) } }

  protected def mandatoryFromString[A](typeLabel: String)(convert: String => A) =
    mandatory(typeLabel) { v => Attempt.safe { convert(v.getStringValue) } }

  implicit val LongMessageAttributeDecode: MessageAttributeDecoder[Long] =
    mandatoryFromString("Long") { _.toLong }

  implicit val IntMessageAttributeDecode: MessageAttributeDecoder[Int] =
    mandatoryFromString("Int") { _.toInt }

  implicit val DateTimeMessageAttributeDecode: MessageAttributeDecoder[DateTime] =
    mandatoryFromString("DateTime") { _.toLong |> { i => new DateTime(i, DateTimeZone.UTC) } }

  implicit val StringMessageAttributeDecode: MessageAttributeDecoder[String] =
    option {
      // No attribute value means an empty string (because DynamoDB doesn't support empty strings as attribute values)
      case None     => Attempt.ok("")
      case Some(av) => Option(av.getStringValue).map { Attempt.ok }.getOrElse { Attempt.fail("No string value present") }
    }

  implicit def OptionMessageAttributeDecode[A](implicit decode: MessageAttributeDecoder[A]): MessageAttributeDecoder[Option[A]] =
    option {
      case None                => Attempt.ok(None)
      case someValue @ Some(_) => decode(someValue).toOption |> Attempt.ok
    }

  implicit val DecodeMessageAttributeValueMonad: Monad[MessageAttributeDecoder] =
    new Monad[MessageAttributeDecoder] {
      def point[A](v: => A) = ok(v)
      def bind[A, B](m: MessageAttributeDecoder[A])(f: A => MessageAttributeDecoder[B]) = m flatMap f
      override def map[A, B](m: MessageAttributeDecoder[A])(f: A => B) = m map f
    }
}
