package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model._
import io.atlassian.aws.dynamodb.TestData._
import io.atlassian.aws.dynamodb.Write.Mode.Overwrite
import io.atlassian.aws.spec.ScalaCheckSpec
import org.scalacheck.{ Arbitrary, Prop }
import org.specs2.main.Arguments
import argonaut._, Argonaut._
import Arbitrary.arbitrary
import scalaz.syntax.std.option._
import scalaz.std.option._
import scalaz.Equal

class JsonIntegrationSpec(val arguments: Arguments) extends ScalaCheckSpec with LocalDynamoDB with DynamoDBActionMatchers {
  val NUM_TESTS =
    if (IS_LOCAL) 100
    else 10

  // Because AWS Local Dynamo doesn't support JSON
  override val useAwsLocalDynamo = false
  implicit val DYNAMO_CLIENT = dynamoClient

  val tableName = s"my_things3_${System.currentTimeMillis.toString}"
  val table =
    TableDefinition.from[Key, JsonValue, HashKey, RangeKey](tableName,
      Key.column, JsonValue.column, HashKey.column, RangeKey.column)

  def is =
    s2"""
         This is an integration test to make sure JSON encoding/decoding works with an actual Dynamo
          Set up local DB if required                     ${step(startLocalDynamoDB)}
          DynamoDB library should                         ${step(createTestTable)}
            work with JSON codec                          $getWhatWasPut
                                                          ${step(deleteTestTable)}
                                                          ${step(stopLocalDynamoDB)}
    """

  def getWhatWasPut =
    Prop.forAll { (key: Key, value: JsonValue) =>
      (for {
        firstPut <- DynamoDB.write[Key, JsonValue](key, value, Overwrite)(table.name, Key.column, JsonValue.column)
        firstGet <- DynamoDB.get[Key, JsonValue](key)(table.name, Key.column, JsonValue.column)
      } yield firstGet) must returnValue(value.some)
    }.set(minTestsOk = NUM_TESTS)

  case class JsonValue(s: String, d: Double, b: Boolean, od: Option[Double], l: List[String], nested: Option[Nested], nestedList: List[Nested])
  object JsonValue {
    lazy val column =
      Column[JsonValue]("jsonValue")

    implicit val JsonValueCodecJson: CodecJson[JsonValue] =
      casecodec7(JsonValue.apply, JsonValue.unapply)("s", "d", "b", "od", "l", "nested", "nested-list")

    implicit val ArbitraryJsonValue: Arbitrary[JsonValue] =
      Arbitrary {
        for {
          s <- arbitrary[String]
          d <- arbitrary[Double]
          b <- arbitrary[Boolean]
          od <- arbitrary[Option[Double]]
          l <- arbitrary[List[String]]
          nested <- arbitrary[Option[Nested]]
          nestedList <- arbitrary[List[Nested]]
        } yield JsonValue(s, 0.0, b, od, l, nested, nestedList)
      }

    implicit val JsonValueEqual: Equal[JsonValue] =
      Equal.equalA[JsonValue]
  }
  case class Nested(s: String, i: Int)
  object Nested {
    implicit val NestedCodecJson: CodecJson[Nested] =
      casecodec2(Nested.apply, Nested.unapply)("s", "i")

    implicit val ArbitraryNested: Arbitrary[Nested] =
      Arbitrary {
        for {
          s <- arbitrary[String]
          i <- arbitrary[Int]
        } yield Nested(s, i)
      }

    implicit val NestedEqual: Equal[Nested] =
      Equal.equalA[Nested]
  }

  object TestTable extends Table {
    type K = Key
    type V = JsonValue
    type H = HashKey
    type R = RangeKey
  }

  def createTestTable() =
    DynamoDBOps.createTable[Key, JsonValue, HashKey, RangeKey](table)

  def deleteTestTable =
    DynamoDBOps.deleteTable[Key, JsonValue, HashKey, RangeKey](table)

}
