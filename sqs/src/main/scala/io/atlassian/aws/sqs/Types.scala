package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.MessageAttributeValue

import scalaz.{ Tag, @@ }

trait Types {
  sealed trait QueueURLMarker
  type QueueURL = String @@ QueueURLMarker
  object QueueURL extends Tagger[String, QueueURLMarker] {
    implicit def QueueURLAsString(q: QueueURL): String =
      Tag.unwrap(q)
  }

  sealed trait ReceiptHandleMarker
  type ReceiptHandle = String @@ ReceiptHandleMarker
  object ReceiptHandle extends Tagger[String, ReceiptHandleMarker] {
    implicit def ReceiptHandleAsString(r: ReceiptHandle): String =
      Tag.unwrap(r)
  }

  sealed trait HeaderSubTypeMarker
  type HeaderSubType = String @@ HeaderSubTypeMarker
  object HeaderSubType extends Tagger[String, HeaderSubTypeMarker] {
    implicit def HeaderSubTypeAsString(h: HeaderSubType): String =
      Tag.unwrap(h)
  }

  sealed trait MessageIdMarker
  type MessageId = String @@ MessageIdMarker
  object MessageId extends Tagger[String, MessageIdMarker] {
    implicit def MessageIdAsString(m: MessageId): String =
      Tag.unwrap(m)
  }

  type HeaderValue = Option[MessageAttributeValue]
  type ToHeader[A] = A => Map[String, HeaderValue]
  type ToBody[A] = A => String
  type Header[A] = Map[String, HeaderValue]
  type HeaderFieldTuple[A] = (String, HeaderValue)
}
