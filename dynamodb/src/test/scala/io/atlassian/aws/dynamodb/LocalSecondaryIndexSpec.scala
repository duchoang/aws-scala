package io.atlassian.aws
package dynamodb

import io.atlassian.aws.dynamodb.Schema.Create.IndexProjection
import io.atlassian.aws.spec.ScalaCheckSpec

import org.junit.runner.RunWith
import org.scalacheck.Prop
import org.specs2.main.Arguments

import scalaz.std.AllInstances._
import scalaz.~>

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class LocalSecondaryIndexSpec(val arguments: Arguments)
    extends ScalaCheckSpec
    with LocalDynamoDB
    with DBActionMatchers {
  import TestData._

  object realTable extends Table.ComplexKey {
    type K = Key
    type V = Value
    type H = HashKey
    type R = RangeKey
    val keyIso = Key.Iso

        
    val schema = defineSchema(s"my_things4_${System.currentTimeMillis.toString}", this)(Key.column, Value.column, HashKey.named, RangeKey.named)
  }

  val table = realTable.localSecondary[IndexRange](realTable.View.Full)
  val indexSchema = realTable.schema.deriveLocalIndex(s"my_index4_${System.currentTimeMillis.toString}", IndexRange.named, Value.column)

  implicit val DYNAMO_CLIENT = dynamoClient

  def run = DynamoDBOps.runAction.compose(DynamoDB.indexInterpreter(table)(indexSchema))

  def runRealTable = DynamoDBOps.runAction.compose(DynamoDB.tableInterpreter(realTable)(realTable.schema))
  def runFreeRealTable: realTable.DBAction ~> InvalidOr = realTable.transform[InvalidOr](runRealTable)

  val NUM_TESTS =
    if (IS_LOCAL) 100
    else 10

  val NUM_PAGING_TESTS =
    if (IS_LOCAL) 20
    else 1

  // TODO - These tests are sequential because of flakiness with integration tests.
  def is = stopOnFail ^ sequential ^ s2"""

  This is a specification to test Table actions integrated with a DynamoDB interpreter.

  Set up local DB if required                     ${step(startLocalDynamoDB)}
  Table should be created with LSI                ${step(createTestTable)}

  Local Secondary Index should
    support querying for non-existent hash keys   $queryWorksWhenHashKeyDoesntExist
    correctly handle sort ordering of range keys  $querySortOrderWorks
    support querying for hash and range keys      $queryForHashAndRangeWorks
    support paginated queries                     ${if (IS_LOCAL) queryWorksWithPaging else skipped("Not running paging test in integration mode")}

                                                  ${step(deleteTestTable)}
                                                  ${step(stopLocalDynamoDB)}
  """

  import Write.Mode._

  def queryWorksWhenHashKeyDoesntExist =
    Prop.forAll { (k: Key) =>
      val hashKey = HashKey(k.a, k.b, k.c)
      table.query(table.Query.hash(hashKey)) must returnResult {
        page => page.result.isEmpty && page.next.isEmpty
      }
    }.set(minTestsOk = NUM_TESTS)

  def queryWorksWithPaging =
    Prop.forAll { (k: Key, v: Value) =>
      // Generate a really long string to max out item size
      val str = (1 to 12000).toList.map { _ => 'a' }.mkString
      val valueToSave = v.copy(hash = str)

      (1 to 200).sliding(25, 25).foreach { window =>
        val valuesToSave = window.map { i =>
          k.copy(seq = i.toLong) -> valueToSave.copy(length = IndexRange(i.toLong))
        }.toMap
        runFreeRealTable(realTable.batchPut(valuesToSave))
      }

      val hashKey = HashKey(k.a, k.b, k.c)
      val query = table.Query.hash(hashKey)

      (for {
        result <- table.query(query)
      } yield result) must returnResult { page =>
        page.next must not beNone
      }
    }.set(minTestsOk = NUM_PAGING_TESTS) // This test takes ages, so don't run it that much

  def querySortOrderWorks =
    Prop.forAll { (k: Key, v1: Value, v2o: Value) =>
      val k2 = k.copy(seq = k.seq - 1)
      val v2 = v2o.copy(length = IndexRange(v1.length.length + 1))
      val valuesToSave = for {
        _ <- realTable.putIfAbsent(k, v1)
        _ <- realTable.putIfAbsent(k2, v2)
      } yield ()
      runFreeRealTable(valuesToSave)

      val hashKey = HashKey(k.a, k.b, k.c)
      val queryAsc = table.Query.hash(hashKey)
      val queryDesc = queryAsc.withConfig(table.Query.Config(direction = ScanDirection.Descending))

      (for {
        ascResult <- table.query(queryAsc)
        descResult <- table.query(queryDesc)
      } yield (ascResult, descResult)) must returnResult {
        case (page1, page2) =>
          page1.result must equal(List(v1, v2)) and
            (page2.result must equal(List(v2, v1))) and
            (page1.next must beNone) and
            (page2.next must beNone)
      }
    }.set(minTestsOk = NUM_TESTS)

  def queryForHashAndRangeWorks =
    Prop.forAll { (k: Key, v1: Value, v2o: Value, v3o: Value) =>
      val k2 = k.copy(seq = k.seq - 1)
      val k3 = k2.copy(seq = k2.seq - 1)
      val v2 = v2o.copy(length = IndexRange(v1.length.length + 1))
      val v3 = v3o.copy(length = IndexRange(v2.length.length + 1))
      val valuesToSave = for {
        _ <- realTable.putIfAbsent(k, v1)
        _ <- realTable.putIfAbsent(k2, v2)
        _ <- realTable.putIfAbsent(k3, v3)
      } yield ()
      runFreeRealTable(valuesToSave)

      val hashKey = HashKey(k.a, k.b, k.c)
      val query = table.Query.range(hashKey, v2.length, Comparison.Lte)
      (for {
        result <- table.query(query)
      } yield result) must returnResult { page =>
        page.result must equal(List(v1, v2)) and
          (page.next must beNone)
      }
    }.set(minTestsOk = NUM_TESTS)

  def createTestTable() =
    DynamoDBOps.createTable(Schema.Create.complexKey(realTable.schema, defaultThroughput).addLocalIndex(indexSchema, IndexProjection.All))

  def deleteTestTable =
    DynamoDBOps.deleteTable(realTable.schema)
}
