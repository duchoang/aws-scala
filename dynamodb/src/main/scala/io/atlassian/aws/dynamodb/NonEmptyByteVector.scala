package io.atlassian.aws.dynamodb

import scodec.bits.ByteVector

case class NonEmptyByteVector private (bytes: ByteVector)

object NonEmptyByteVector {
  def unapply(bytes: ByteVector): Option[NonEmptyByteVector] =
    if (bytes.length < 1)
      None
    else
      Some(NonEmptyByteVector(bytes))
}