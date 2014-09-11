package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec
import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Prop}
import Arbitrary._
import scalaz.syntax.id._
import org.joda.time.DateTime

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class EncodeDecodeSpec extends ScalaCheckSpec {
  import Attempt._

  def is = s2"""
    EncodeAttributeValue/DecodeAttributeValue should
      correctly encode and decode longs         $correctlyEncodeDecodeLongs
      correctly encode and decode ints          $correctlyEncodeDecodeInts
      correctly encode and decode strings       $correctlyEncodeDecodeStrings
      correctly encode and decode date times    $correctlyEncodeDecodeDateTime
      correctly encode and decode options       $correctlyEncodeDecodeOptions
  """

  import Encoder._
  import Decoder._

  def correctlyEncodeDecodeLongs = Prop.forAll {
    long: Long =>
      (LongEncode(long) |> LongDecode) === Attempt.ok(long)
  }

  def correctlyEncodeDecodeInts = Prop.forAll {
    int: Int =>
      (IntEncode(int) |> IntDecode) === Attempt.ok(int)
  }

  def correctlyEncodeDecodeStrings = Prop.forAll {
    string: String =>
      (StringEncode(string) |> StringDecode) === Attempt.ok(string)
  }

  def correctlyEncodeDecodeDateTime = Prop.forAll {
    value: DateTime =>
      (DateTimeEncode(value) |> DateTimeDecode) must equal(Attempt.ok(value))
  }

  def correctlyEncodeDecodeOptions = Prop.forAll {
    value: Option[String] =>
      value match {
        case None => OptionEncode[String].run(value) must beNone
        case Some("") => OptionEncode[String].run(value) must beNone
        case Some(v) => (OptionEncode[String].run(value) |> OptionDecode[String]) === Attempt.ok(value)
      }
  }
}
