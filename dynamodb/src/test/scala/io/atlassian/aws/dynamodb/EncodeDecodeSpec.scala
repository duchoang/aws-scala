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
import argonaut._, Argonaut._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class EncodeDecodeSpec extends ScalaCheckSpec {
  import Attempt._, JsonData._

  def is = s2"""
  Encode/Decode pairs should correctly:
    round-trip longs                 ${Prop.forAll { roundTrip(_: Long) }}
    round-trip ints                  ${Prop.forAll { roundTrip(_: Int) }}
    round-trip strings               ${Prop.forAll { roundTrip(_: String) }}
    round-trip date times            ${Prop.forAll { roundTrip(_: DateTime) }}
    round-trip options               ${Prop.forAll { roundTrip(_: Option[String]) }}
    round-trip object with CodecJson ${Prop.forAll { roundTrip(_: Foo) }}
    round-trip binary converted type ${Prop.forAll { roundTrip(_: TwoLongs) }}
    round-trip JSON                  ${Prop.forAll { roundTrip(_: Json) }}
  """

  def roundTrip[A: Encoder: Decoder: Equal](a: A) =
    (Encoder[A].encode(a) |> Decoder[A].decode) must equal(Attempt.ok(a))

  case class Foo(s: String, i: Int)
  implicit val CodecFoo = casecodec2(Foo.apply, Foo.unapply)("s", "i")
  implicit val EqualFoo: Equal[Foo] = Equal.equalA
  implicit val ArbitraryFoo: Arbitrary[Foo] =
    Arbitrary {
      for {
        s <- arbitrary[String]
        i <- arbitrary[Int]
      } yield Foo(s, i)
    }
}
