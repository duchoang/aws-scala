package io.atlassian.aws
package sqs

import scalaz.syntax.std.option._

/**
 * Useful wrapper around a given message type that adds a retry count that is stored as a message attribute.
 */
case class RetriedMessage[A](retryCount: Int, message: A) {
  def next: RetriedMessage[A] = this.copy(retryCount = retryCount + 1)
}

object RetriedMessage {
  val RetryCountHeaderField = HeaderField[Option[Int]]("retry-count")

  implicit def RetriedMessageMarshaller[A: Marshaller]: Marshaller[RetriedMessage[A]] =
    Marshaller.from({ r =>
      Map(
        RetryCountHeaderField(r.retryCount.some)
      ) ++ Marshaller[A].header(r.message)
    }, { r =>
      Marshaller[A].body(r.message)
    })

  implicit def RetriedMessageUnmarshaller[A: Unmarshaller]: Unmarshaller[RetriedMessage[A]] =
    Unmarshaller.from {
      for {
        retryCount <- RetryCountHeaderField.get
        message <- Unmarshaller[A].unmarshall
      } yield RetriedMessage(retryCount | 0, message)
    }
}