package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.MessageAttributeValue

import scalaz.{ Tag, @@ }

trait Types {
  sealed trait QueueURLMarker
  type QueueURL = String @@ QueueURLMarker
  object QueueURL extends Tagger[String, QueueURLMarker]

  sealed trait ReceiptHandleMarker
  type ReceiptHandle = String @@ ReceiptHandleMarker
  object ReceiptHandle extends Tagger[String, ReceiptHandleMarker]

  sealed trait HeaderSubTypeMarker
  type HeaderSubType = String @@ HeaderSubTypeMarker
  object HeaderSubType extends Tagger[String, HeaderSubTypeMarker]

  sealed trait MessageIdMarker
  type MessageId = String @@ MessageIdMarker
  object MessageId extends Tagger[String, MessageIdMarker]

  type HeaderValue = Option[MessageAttributeValue]
  type ToHeader[A] = A => Map[String, HeaderValue]
  type ToBody[A] = A => String
  type Header[A] = Map[String, HeaderValue]
  type HeaderFieldTuple[A] = (String, HeaderValue)
}
