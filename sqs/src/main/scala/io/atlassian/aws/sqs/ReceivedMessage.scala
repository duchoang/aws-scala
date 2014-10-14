package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.Message
import kadai.Invalid
import org.joda.time.DateTime

import scalaz.{\/-, -\/, \/}

/*
 * This is just a glorified InvalidReceivedMessage[A] \/ ValidReceivedMessage[A].
 */
sealed trait ReceivedMessage[A] {
  val messageId: MessageId
  val receiptHandle: ReceiptHandle

  def toOr: InvalidReceivedMessage[A] \/ ValidReceivedMessage[A] = this match {
    case i: InvalidReceivedMessage[A] => -\/(i)
    case v: ValidReceivedMessage[A]   => \/-(v)
  }

  def fold[X](invalid: InvalidReceivedMessage[A] => X, valid: ValidReceivedMessage[A] => X): X = this match {
    case i: InvalidReceivedMessage[A] => invalid(i)
    case v: ValidReceivedMessage[A]   => valid(v)
  }
}

case class StandardAttributes(approxFirstReceived: DateTime,
                              approxReceiveCount: Int,
                              senderId: String,
                              sentTime: DateTime)

case class ValidReceivedMessage[A](messageId: MessageId,
                                   receiptHandle: ReceiptHandle,
                                   attributes: StandardAttributes,
                                   message: A)
  extends ReceivedMessage[A]

case class InvalidReceivedMessage[A](rawMessage: Message,
                                     failureReason: Invalid)
  extends ReceivedMessage[A] {

  val messageId: MessageId = MessageId(rawMessage.getMessageId)
  val receiptHandle: ReceiptHandle = ReceiptHandle(rawMessage.getReceiptHandle)

}
