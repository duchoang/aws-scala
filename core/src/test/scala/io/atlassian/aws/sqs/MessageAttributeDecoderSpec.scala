package io.atlassian.aws
package sqs

import spec.ScalaCheckSpec
import org.junit.runner.RunWith
import scalaz.syntax.id._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageAttributeDecoderSpec extends ScalaCheckSpec {
  import MessageAttributeDecoder._
  import MessageAttributeEncoder._

  def is = s2"""
    Decoder should
      not fall over if it fails to decode long      $longDecodeHandlesExceptions
      not fall over if it fails to decode int       $intDecodeHandlesExceptions
      not fall over if it fails to decode DateTime  $dateTimeDecodeHandlesExceptions
      not fall over if it fails to decode options   $optionDecodeandlesExceptions
  """

  def longDecodeHandlesExceptions =
    (StringEncode("Foo") |> LongMessageAttributeDecode).toOr.toEither must beLeft

  def intDecodeHandlesExceptions =
    (StringEncode("Foo") |> IntMessageAttributeDecode).toOr.toEither must beLeft

  def dateTimeDecodeHandlesExceptions =
    (StringEncode("Foo") |> DateTimeMessageAttributeDecode).toOr.toEither must beLeft

  def optionDecodeandlesExceptions =
    (StringEncode("Foo") |> OptionMessageAttributeDecode[Int]) === Attempt.ok(None)
}
