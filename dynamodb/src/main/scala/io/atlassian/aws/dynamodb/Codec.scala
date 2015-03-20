package io.atlassian.aws.dynamodb

import scalaz.InvariantFunctor

/**
 * Brings together an Encoder/Decoder pair. Has an invariant functor
 */
trait Codec[A] {
  self =>

  def encode: Encoder[A]
  def decode: Decoder[A]

  def xmap[B](f: A => B, g: B => A): Codec[B] =
    new Codec[B] {
      val encode = self.encode contramap g
      val decode = self.decode map f
    }
}

object Codec {
  def apply[A: Codec]: Codec[A] =
    implicitly[Codec[A]]

  def from[A: Encoder: Decoder] =
    new Codec[A] {
      val encode = Encoder[A]
      val decode = Decoder[A]
    }

  implicit object DynamoCodecInvariantFunctor extends InvariantFunctor[Codec] {
    def xmap[A, B](ma: Codec[A], f: A => B, g: B => A): Codec[B] =
      ma.xmap(f, g)
  }
}