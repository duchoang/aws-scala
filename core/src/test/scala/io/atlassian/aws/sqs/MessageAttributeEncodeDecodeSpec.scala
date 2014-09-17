package io.atlassian.aws
package sqs

import spec.ScalaCheckSpec
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.Prop
import scalaz.syntax.id._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class MessageAttributeEncodeDecodeSpec extends ScalaCheckSpec {
  import MessageAttributeDecoder._
  import MessageAttributeEncoder._

  def is = s2"""
    EncodeAttributeValue/DecodeAttributeValue should
      correctly encode and decode longs         $correctlyEncodeDecodeLongs
      correctly encode and decode ints          $correctlyEncodeDecodeInts
      correctly encode and decode strings       $correctlyEncodeDecodeStrings
      correctly encode and decode date times    $correctlyEncodeDecodeDateTime
      correctly encode and decode options       $correctlyEncodeDecodeOptions
  """

  def correctlyEncodeDecodeLongs = Prop.forAll {
    long: Long =>
      (LongEncode(long) |> LongMessageAttributeDecode) === Attempt.ok(long)
  }

  def correctlyEncodeDecodeInts = Prop.forAll {
    int: Int =>
      (IntEncode(int) |> IntMessageAttributeDecode) === Attempt.ok(int)
  }

  def correctlyEncodeDecodeStrings = Prop.forAll {
    string: String =>
      (StringEncode(string) |> StringMessageAttributeDecode) === Attempt.ok(string)
  }

  def correctlyEncodeDecodeDateTime = Prop.forAll {
    value: DateTime =>
      (DateTimeEncode(value) |> DateTimeMessageAttributeDecode) must equal(Attempt.ok(value))
  }

  def correctlyEncodeDecodeOptions = Prop.forAll {
    value: Option[String] =>
      value match {
        case None => OptionEncode[String].run(value) must beNone
        case Some("") => OptionEncode[String].run(value) must beNone
        case Some(v) => (OptionEncode[String].run(value) |> OptionMessageAttributeDecode[String]) === Attempt.ok(value)
      }
  }
}
