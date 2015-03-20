package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Prop }
import Arbitrary._
import scalaz.Equal
import scalaz.syntax.id._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.anyVal._
import org.joda.time.DateTime

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class EncodeDecodeSpec extends ScalaCheckSpec {
  import Attempt._

  def is = s2"""
  Encode/Decode pairs should correctly:
    round-trip longs         ${Prop.forAll { roundTrip(_: Long) }}
    round-trip ints          ${Prop.forAll { roundTrip(_: Int) }}
    round-trip strings       ${Prop.forAll { roundTrip(_: String) }}
    round-trip date times    ${Prop.forAll { roundTrip(_: DateTime) }}
    round-trip options       $correctlyEncodeDecodeOptions
  """

  def roundTrip[A: Encoder: Decoder: Equal](a: A) =
    (Encoder[A].encode(a) |> Decoder[A].decode) must equal(Attempt.ok(a))

  def correctlyEncodeDecodeOptions = Prop.forAll {
    value: Option[String] =>
      value match {
        case None     => Encoder.OptionEncode[String].run(value) must beNone
        case Some("") => Encoder.OptionEncode[String].run(value) must beNone
        case Some(v)  => roundTrip(value)
      }
  }
}
