package io.atlassian.aws
package dynamodb

import scodec.bits.ByteVector
import spec.ScalaCheckSpec
import org.junit.runner.RunWith
import scalaz.syntax.id._
import org.joda.time.DateTime

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class DecodeSpec extends ScalaCheckSpec {

  def is = s2"""
    Decoder should
      not fall over if it fails to decode long      $longDecodeHandlesExceptions
      not fall over if it fails to decode int       $intDecodeHandlesExceptions
      not fall over if it fails to decode DateTime  $dateTimeDecodeHandlesExceptions
      not fall over if it fails to decode String    $stringDecodeHandlesExceptions
      not fall over if it fails to decode options   $optionDecodeHandlesExceptions
      not fall over if it fails to decode binary    $byteBufferDecodeHandlesExceptions
      propagate failure from mapAttempt function    $mapAttemptPropagatesExceptions
  """

  import Encoder._
  import Decoder._

  def longDecodeHandlesExceptions =
    (Encoder[String].encode("Foo") |> Decoder[Long].decode).toOr.toEither must beLeft

  def intDecodeHandlesExceptions =
    (Encoder[String].encode("Foo") |> Decoder[Int].decode).toOr.toEither must beLeft

  def dateTimeDecodeHandlesExceptions =
    (Encoder[String].encode("Foo") |> Decoder[DateTime].decode).toOr.toEither must beLeft

  def stringDecodeHandlesExceptions =
    (Encoder[Int].encode(100) |> Decoder[String].decode).toOr.toEither must beLeft

  def optionDecodeHandlesExceptions =
    (Encoder[Int].encode(100) |> Decoder[Option[String]].decode) === Attempt.ok(None)

  def byteBufferDecodeHandlesExceptions =
    (Encoder[Int].encode(100) |> Decoder[NonEmptyByteVector].decode).toOr.toEither must beLeft

  def mapAttemptPropagatesExceptions =
    (Encoder[NonEmptyByteVector].encode(ByteVector.fromByte(1) match { case NonEmptyByteVector(b) => b }) |>
      Decoder[TwoLongs].decode).toOr.toEither must beLeft
}
