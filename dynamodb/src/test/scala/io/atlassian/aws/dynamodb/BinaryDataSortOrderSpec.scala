package io.atlassian.aws
package dynamodb

import org.scalacheck.Prop
import spec.ScalaCheckSpec
import org.specs2.main.Arguments

import scalaz.Isomorphism.{ IsoSet, <=> }
import scalaz.Order
import scalaz.std.AllInstances._

/**
 * Test the sort order of binary range key in Dynamo
 */
class BinaryDataSortOrderSpec(val arguments: Arguments)
    extends ScalaCheckSpec
    with LocalDynamoDB
    with DBActionMatchers {

  import TestData._

  def is = stopOnFail ^ s2"""
     This specification tests the sort order of binary range keys in Dynamo

     Set up local DB if required                     ${step(startLocalDynamoDB)}
                                                     ${step(createTestTable)}
     Sort ordering should work                       $querySortOrderWorks
                                                     ${step(deleteTestTable)}
                                                     ${step(stopLocalDynamoDB)}

  """

  case class ComplexKey(h: HashKey, r: TwoLongs)
  object ComplexKey {
    lazy val twoLongsColumn = Column[TwoLongs]("twolongs")
    lazy val column = Column.compose2[ComplexKey](HashKey.column, twoLongsColumn) {
      case ComplexKey(h, r) => (h, r)
    }(ComplexKey.apply)
  }

  object table extends Table.ComplexKey {
    type K = ComplexKey
    type V = TestData.Value
    type H = HashKey
    type R = TwoLongs
    

    def keyIso: ComplexKey <=> (HashKey, TwoLongs) = new IsoSet[ComplexKey, (HashKey, TwoLongs)] {
      def from = (ComplexKey.apply _).tupled
      def to = c => (c.h, c.r)
    }

    val schema =
      defineSchema(s"my_things3_${System.currentTimeMillis.toString}", this)(ComplexKey.column, Value.column, HashKey.column, ComplexKey.twoLongsColumn)
  }

  implicit val DYNAMO_CLIENT = dynamoClient

  def run = DynamoDBOps.runAction.compose(DynamoDB.tableInterpreter(table)(table.schema.kv))

  val NUM_TESTS =
    if (IS_LOCAL) 100
    else 10

  def createTestTable() =
    DynamoDBOps.createTable(Schema.Create(table.schema))

  def deleteTestTable =
    DynamoDBOps.deleteTable(table.schema.kv)

  def querySortOrderWorks =
    Prop.forAll { (hashKey: HashKey, r1: TwoLongs, r2: TwoLongs, v1: TestData.Value, v2: TestData.Value) =>
      (r1 != r2) ==> {
        val k1 = ComplexKey(hashKey, Order[TwoLongs].min(r1, r2))
        val k2 = ComplexKey(hashKey, Order[TwoLongs].max(r1, r2))
        val queryAsc = table.Query.hash(hashKey)
        val queryDesc = queryAsc.withConfig(table.Query.Config(direction = ScanDirection.Descending))

        (for {
          _ <- table.putIfAbsent(k1, v1)
          _ <- table.putIfAbsent(k2, v2)
          ascResult <- table.query(queryAsc)
          descResult <- table.query(queryDesc)
        } yield (ascResult, descResult)) must returnResult {
          case (page1, page2) =>
            page1.result must equal(List(v1, v2)) and
              (page2.result must equal(List(v2, v1))) and
              (page1.next must beNone) and
              (page2.next must beNone)
        }
      }
    }.set(minTestsOk = NUM_TESTS)
}
