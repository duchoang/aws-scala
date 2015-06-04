package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.MessageAttributeValue

import scalaz.{ Tag, @@ }

trait Types {
  type QueueURL = String @@ QueueURL.Marker
  object QueueURL extends Tagger[String]

  type ReceiptHandle = String @@ ReceiptHandle.Marker
  object ReceiptHandle extends Tagger[String]

  type HeaderSubType = String @@ HeaderSubType.Marker
  object HeaderSubType extends Tagger[String]

  type MessageId = String @@ MessageId.Marker
  object MessageId extends Tagger[String]

  type HeaderValue = Option[MessageAttributeValue]
  type ToHeader[A] = A => Map[String, HeaderValue]
  type ToBody[A] = A => String
  type Header[A] = Map[String, HeaderValue]
  type HeaderFieldTuple[A] = (String, HeaderValue)
}
