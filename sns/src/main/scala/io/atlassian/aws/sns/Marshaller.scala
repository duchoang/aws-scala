package io.atlassian.aws.sns

import argonaut.EncodeJson

/**
 * Type class for marshalling objects into a message body and subject suitable for passing to AWS SQS client.
 * @tparam A The type of the object to marshall.
 */
trait Marshaller[A] {

  def subject(a: A): Option[String]

  def message(a: A): String
}

object Marshaller {
  def apply[A: Marshaller] =
    implicitly[Marshaller[A]]

  def from[A](toSubject: ToSubject[A], toBody: ToMessage[A]): Marshaller[A] =
    new Marshaller[A] {
      override def subject(a: A): Option[String] =
        toSubject(a)

      override def message(a: A): String =
        toBody(a)
    }

  def jsonBodyWithSubject[A](subject: ToSubject[A])(implicit ev: EncodeJson[A]): Marshaller[A] = {
    from(subject, { a => ev.encode(a).nospaces })
  }

  def jsonBody[A](implicit ev: EncodeJson[A]): Marshaller[A] =
    jsonBodyWithSubject[A] { (_: A) => None }
}
