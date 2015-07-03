package io.atlassian.aws.dynamodb

import scalaz.Equal
import scodec.bits.ByteVector

case class NonEmptyBytes private (bytes: ByteVector)

object NonEmptyBytes {
  def unapply(bytes: ByteVector): Option[NonEmptyBytes] =
    if (bytes.length < 1)
      None
    else
      Some(NonEmptyBytes(bytes))

  implicit val NonEmptyBytesEqual: Equal[NonEmptyBytes] =
    Equal.equalA
}