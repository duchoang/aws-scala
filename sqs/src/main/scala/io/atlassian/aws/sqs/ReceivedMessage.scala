package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.Message
import kadai.Invalid
import org.joda.time.DateTime

import scalaz.{\/-, -\/, \/}

/*
 * This is just a glorified InvalidReceivedMessage[A] \/ ValidReceivedMessage[A].
 */
sealed trait ReceivedMessage[+A] {
  val messageId: MessageId
  val receiptHandle: ReceiptHandle

  def toOr: InvalidReceivedMessage \/ ValidReceivedMessage[A] = this match {
    case i: InvalidReceivedMessage => -\/(i)
    case v: ValidReceivedMessage[A]   => \/-(v)
  }

  def fold[X](invalid: InvalidReceivedMessage => X, valid: ValidReceivedMessage[A] => X): X = this match {
    case i: InvalidReceivedMessage => invalid(i)
    case v: ValidReceivedMessage[A]   => valid(v)
  }
}

case class StandardAttributes(approxFirstReceived: DateTime,
                              approxReceiveCount: Int,
                              senderId: String,
                              sentTime: DateTime)

case class ValidReceivedMessage[+A](messageId: MessageId,
                                   receiptHandle: ReceiptHandle,
                                   attributes: StandardAttributes,
                                   message: A)
  extends ReceivedMessage[A]

case class InvalidReceivedMessage(rawMessage: Message,
                                     failureReason: Invalid)
  extends ReceivedMessage[Nothing] {

  val messageId: MessageId = MessageId(rawMessage.getMessageId)
  val receiptHandle: ReceiptHandle = ReceiptHandle(rawMessage.getReceiptHandle)

}
