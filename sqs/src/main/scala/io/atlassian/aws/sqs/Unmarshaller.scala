package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.Message
import org.joda.time.DateTime
import scalaz.Monad
import scalaz.syntax.id._
import scalaz.syntax.monad._
import scalaz.syntax.monoid._
import scala.collection.JavaConverters._
import kadai.Invalid
import argonaut._, Argonaut._

/**
 * Use an unmarshaller to convert an AWS SQS message into a object of type A.
 */
trait Unmarshaller[A] {
  def unmarshall: Unmarshaller.Operation[A]
}

object Unmarshaller {
  /**
   * Generates an Unmarshaller for a ReceivedMessage[A], basically a wrapper of your type A with
   * standard attributes such as received count, and sent time.
   */
  def receivedMessage[A: Unmarshaller]: Unmarshaller[ValidReceivedMessage[A]] =
    from {
      for {
        stdAttributes <- standardAttributes
        a <- Unmarshaller[A].unmarshall
        msgId <- messageId
        handle <- receiptHandle
      } yield ValidReceivedMessage(msgId, handle, stdAttributes, a)
    }

  def from[A](f: Operation[A]) =
    new Unmarshaller[A] {
      def unmarshall = f
    }

  def jsonBody[A: DecodeJson] =
    from {
      Operation.jsonBody[A]
    }

  def apply[A: Unmarshaller] =
    implicitly[Unmarshaller[A]]

  val standardAttributes: Operation[StandardAttributes] = {
    import Unmarshaller.Operation._
    import FromString._
    for {
      approxReceiveCount <- stdAttr[Int]("ApproximateReceiveCount")
      approxFirstReceived <- stdAttr[DateTime]("ApproximateFirstReceiveTimestamp")
      senderId <- stdAttr[String]("SenderId")
      sentTime <- stdAttr[DateTime]("SentTimestamp")
    } yield StandardAttributes(approxFirstReceived, approxReceiveCount, senderId, sentTime)
  }

  val messageId: Operation[MessageId] =
    Operation.withMessage { m => MessageId(m.getMessageId) }

  val receiptHandle: Operation[ReceiptHandle] =
    Operation.withMessage { m => ReceiptHandle(m.getReceiptHandle) }

  /**
   * Sequence a bunch of Operations together to form an unmarshaller. e.g. have an operation
   * for each header stored in the message attribute value map, and an operation to parse the
   * message body
   */
  case class Operation[A](run: Message => Attempt[A]) {
    def apply(m: Message): Attempt[A] =
      run(m)

    def map[B](f: A => B): Operation[B] =
      flatMap(f andThen Operation.ok)

    def flatMap[B](f: A => Operation[B]): Operation[B] =
      Operation {
        m => run(m) >>= { f(_)(m) }
      }
  }

  object Operation {
    def ok[A](strict: A): Operation[A] = Operation { _ => Attempt.ok(strict) }

    def fail[A](error: Invalid): Operation[A] = Operation { _ => Attempt(error.left) }

    def withMessage[A](f: Message => A): Operation[A] =
      Operation { m => Attempt.ok(f(m)) }

    /**
     * Try to extract a value of type A from the 'standard' attributes available for every SQS message
     * e.g. approximate first received time, received count, sender ID and sent time.
     */
    def stdAttr[A](name: String)(implicit decoder: FromString[A]): Operation[A] =
      Operation { m =>
        decoder(m.getAttributes.asScala.get(name)).lift {
          _.leftMap { _ |+| Invalid.Message(s"Cannot decode standard attribute field $name") }
        }
      }

    /**
     * Try to extract a value of type A from the message attributes. These are custom attributes that the
     * message sender may have added.
     */
    def msgAttr[A](name: String)(implicit decoder: MessageAttributeDecoder[A]): Operation[A] =
      Operation { m =>
        (m.getMessageAttributes.asScala.get(name) |> decoder).lift {
          _.leftMap { _ |+| Invalid.Message(s"Cannot decode header field $name") }
        }
      }

    /**
     * Extract and parse a JSON message from the message body.
     */
    def jsonBody[A: DecodeJson]: Operation[A] =
      Operation { m =>
        m.getBody.decodeEither[A].leftMap(Invalid.Message) |> Attempt.apply
      }

    implicit def OperationMonad: Monad[Operation] =
      new Monad[Operation] {
        def point[A](v: => A) = Operation.ok(v)
        def bind[A, B](m: Operation[A])(f: A => Operation[B]) = m flatMap f
        override def map[A, B](m: Operation[A])(f: A => B) = m map f
      }
  }
}
