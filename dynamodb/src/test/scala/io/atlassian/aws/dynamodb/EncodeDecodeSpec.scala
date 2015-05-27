package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec
import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalacheck.{ Gen, Arbitrary, Prop }
import Arbitrary._
import scalaz.{ @@, Equal }
import scalaz.syntax.id._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.anyVal._
import org.joda.time.DateTime
import argonaut._, Argonaut._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class EncodeDecodeSpec extends ScalaCheckSpec {
  import Attempt._

  def is = s2"""
  Encode/Decode pairs should correctly:
    round-trip longs                                             ${Prop.forAll { roundTrip(_: Long) }}
    round-trip ints                                              ${Prop.forAll { roundTrip(_: Int) }}
    round-trip strings                                           ${Prop.forAll { roundTrip(_: String) }}
    round-trip date times                                        ${Prop.forAll { roundTrip(_: DateTime) }}
    round-trip options                                           ${Prop.forAll { roundTrip(_: Option[String]) }}
    round-trip object with CodecJson                             ${Prop.forAll { roundTrip(_: Foo) }}
    round-trip binary converted type                             ${Prop.forAll { roundTrip(_: TwoLongs) }}
    round-trip JSON                                              ${Prop.forAll { roundTrip(_: Json) }}
    round-trip deep JSON to test stack overflows                 ${Prop.forAll { roundTrip(_: DeepJson) }}
  """

  def roundTrip[A: Encoder: Decoder: Equal](a: A) =
    (Encoder[A].encode(a) |> Decoder[A].decode) must equal(Attempt.ok(a))

  type DeepJson = Json @@ DeepJson.Marker
  object DeepJson extends Tagger[Json]

  implicit val deepJsonArbitrary: Arbitrary[DeepJson] =
    Arbitrary(deepJsonValueGenerator(642).map { DeepJson.apply })

  private def deepJsonValueGenerator(depth: Int): Gen[Json] = {
    if (depth > 1) {
      deepJsonArrayGenerator(depth - 1)
    } else {
      Gen.oneOf(JsonData.jsonNumberGenerator, JsonData.jsonStringGenerator, JsonData.jsonBoolGenerator, JsonData.jsonNothingGenerator)
    }
  }

  private def deepJsonArrayItemsGenerator(depth: Int): Gen[Seq[Json]] = listOfN(1, deepJsonValueGenerator(depth - 1))

  private def deepJsonArrayGenerator(depth: Int): Gen[Json] = deepJsonArrayItemsGenerator(depth).map { values => jArray(values.toList) }

  implicit val DeepJsonEncoder: Encoder[DeepJson] =
    Encoder[Json].contramap { _.unwrap }

  implicit val DeepJsonDecoder: Decoder[DeepJson] =
    Decoder[Json].map { DeepJson.apply }

  implicit val DeepJsonEqual: Equal[DeepJson] =
    Equal[Json].contramap { _.unwrap }

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
  implicit val ArbitraryJson: Arbitrary[Json] = Arbitrary(JsonData.jsonValueGenerator(8))
}
