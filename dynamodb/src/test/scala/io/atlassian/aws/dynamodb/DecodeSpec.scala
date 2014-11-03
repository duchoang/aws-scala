package io.atlassian.aws
package dynamodb

import spec.ScalaCheckSpec
import org.junit.runner.RunWith
import scalaz.syntax.id._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class DecodeSpec extends ScalaCheckSpec {

  def is = s2"""
    Decoder should
      not fall over if it fails to decode long      $longDecodeHandlesExceptions
      not fall over if it fails to decode int       $intDecodeHandlesExceptions
      not fall over if it fails to decode DateTime  $dateTimeDecodeHandlesExceptions
      not fall over if it fails to decode String    $stringDecodeHandlesExceptions
      not fall over if it fails to decode options   $optionDecodeandlesExceptions
  """

  import Encoder._
  import Decoder._

  def longDecodeHandlesExceptions =
    (StringEncode("Foo") |> LongDecode).toOr.toEither must beLeft

  def intDecodeHandlesExceptions =
    (StringEncode("Foo") |> IntDecode).toOr.toEither must beLeft

  def dateTimeDecodeHandlesExceptions =
    (StringEncode("Foo") |> DateTimeDecode).toOr.toEither must beLeft

  def stringDecodeHandlesExceptions =
    (IntEncode(100) |> StringDecode).toOr.toEither must beLeft

  def optionDecodeandlesExceptions =
    (IntEncode(100) |> OptionDecode[String]) === Attempt.ok(None)
}
