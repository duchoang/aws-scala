package io.atlassian.aws.dynamodb

import scodec.bits.ByteVector

case class NonEmptyBytes private (bytes: ByteVector)

object NonEmptyBytes {
  def unapply(bytes: ByteVector): Option[NonEmptyBytes] =
    if (bytes.length < 1)
      None
    else
      Some(NonEmptyBytes(bytes))
}