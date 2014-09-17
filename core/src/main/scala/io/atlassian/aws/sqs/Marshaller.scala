package io.atlassian.aws
package sqs

import argonaut.EncodeJson
import com.amazonaws.services.sqs.model.MessageAttributeValue

/**
 * Type class for marshalling objects into a message body and message attribute value map suitable for passing to AWS SQS client.
 * @tparam A The type of the object to marshall.
 */
trait Marshaller[A] {

  def header(a: A): Header[A]

  def headerFlattened(a: A): Map[String, MessageAttributeValue] =
    header(a).collect {
      case (key, Some(value)) => key -> value
    }

  def body(a: A): String
}

object Marshaller {
  def apply[A: Marshaller] =
    implicitly[Marshaller[A]]

  def from[A](toHeader: ToHeader[A], toBody: ToBody[A]): Marshaller[A] =
    new Marshaller[A] {
      override def header(a: A): Header[A] =
        toHeader(a)

      override def body(a: A): String =
        toBody(a)
    }

  def jsonBodyWithHeader[A](header: ToHeader[A])(implicit ev: EncodeJson[A]): Marshaller[A] = {
    from(header, { a => ev.encode(a).nospaces })
  }

  def jsonBody[A](implicit ev: EncodeJson[A]): Marshaller[A] =
    jsonBodyWithHeader[A] { (_: A) => Map.empty }
}
