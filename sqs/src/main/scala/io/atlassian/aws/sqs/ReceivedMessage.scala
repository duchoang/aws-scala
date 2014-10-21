package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.Message
import kadai.{ Invalid => KInvalid }
import org.joda.time.DateTime

import scalaz.{ \/-, -\/, \/ }

/**
 * May be valid or not.
 */
sealed trait ReceivedMessage[+A] {
  import ReceivedMessage._

  val messageId: MessageId
  val receiptHandle: ReceiptHandle

  def toOr: Invalid \/ Valid[A] =
    fold(-\/.apply, \/-.apply)

  def fold[X](invalid: Invalid => X, valid: Valid[A] => X): X =
    this match {
      case i @ Invalid(_, _)     => invalid(i)
      case v @ Valid(_, _, _, _) => valid(v)
    }
}

object ReceivedMessage {

  case class Valid[+A](messageId: MessageId, receiptHandle: ReceiptHandle, attributes: StandardAttributes, message: A) extends ReceivedMessage[A]

  case class Invalid(rawMessage: Message, failureReason: KInvalid) extends ReceivedMessage[Nothing] {
    val messageId: MessageId = MessageId(rawMessage.getMessageId)
    val receiptHandle: ReceiptHandle = ReceiptHandle(rawMessage.getReceiptHandle)
  }
}

/**
 * Note that received and receiveCount are approximate only.
 */
case class StandardAttributes(received: DateTime, receiveCount: Int, senderId: String, sentTime: DateTime)
