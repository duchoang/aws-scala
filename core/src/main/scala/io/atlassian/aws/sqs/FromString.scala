package io.atlassian.aws
package sqs

import scalaz.syntax.id._
import scalaz.syntax.bind.ToBindOps
import org.joda.time.{ DateTime, DateTimeZone }

/**
 * Represents a function that parses an Option[String] to a value of type A safely
 * i.e. catching format exceptions etc.
 */
case class FromString[A](run: Option[String] => Attempt[A]) {
  def apply(o: Option[String]): Attempt[A] =
    run(o)

  def map[B](f: A => B): FromString[B] = flatMap(f andThen FromString.ok)

  def flatMap[B](f: A => FromString[B]): FromString[B] =
    FromString {
      m => run(m) >>= { f(_)(m) }
    }
}

object FromString {
  def apply[A: FromString] =
    implicitly[FromString[A]]

  def ok[A](strict: A): FromString[A] = from { Attempt.ok(strict) }

  def from[A](a: Attempt[A]): FromString[A] = FromString { _ => a }

  implicit val IntFromString: FromString[Int] =
    mandatoryField( _.toInt, "Int")

  implicit val StringFromString: FromString[String] =
    mandatoryField(identity, "String")

  implicit val DateTimeFromString: FromString[DateTime] =
    mandatoryField( _.toLong |> { i => new DateTime(i, DateTimeZone.UTC) }, "DateTime")

  def mandatoryField[A](f: String => A, label: String): FromString[A] =
    FromString { o =>
      o match {
        case None => Attempt.fail(s"Could not decode $label value")
        case Some(v) => Attempt.safe(f(v))
      }
    }
}
