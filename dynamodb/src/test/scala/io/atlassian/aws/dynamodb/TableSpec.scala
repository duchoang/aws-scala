package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec

import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.Prop
import org.specs2.main.Arguments

import java.util.UUID.randomUUID
import scalaz.std.AllInstances._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class TableSpec(val arguments: Arguments)
    extends ScalaCheckSpec
    with LocalDynamoDB
    with DBActionMatchers {
  import TestData._

  object table extends Table.ComplexKey {
    type K = Key
    type V = Value
    type H = HashKey
    type R = RangeKey

    def keyIso = Key.Iso

    val schema = defineSchema(s"my_things5_${System.currentTimeMillis.toString}", this)(Key.column, Value.column, HashKey.named, RangeKey.named)
  }

  implicit val DYNAMO_CLIENT = dynamoClient

  def run = DynamoDBOps.runAction.compose(DynamoDB.tableInterpreter(table)(table.schema))

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
  Table should                                    ${step(createTestTable)}
    returns none if there is no value             $getWorksIfNoValue
    write a new value correctly                   $newWriteWorks
    write replaces a value correctly              $writeReplaceWorks
    update with a deleted field works             $updateWithDeletedFieldWorks
    update fails with a different expected value  $updateFailsIfDifferent
    have a working delete                         $deleteWorks
    handle non-existent keys on delete            $deleteWorksForNonExistentKey

  DynamoDB query capability should
    support querying for non-existent hash keys   $queryWorksWhenHashKeyDoesntExist
    correctly handle sort ordering of range keys  $querySortOrderWorks
    support querying for hash and range keys      $queryForHashAndRangeWorks
    support paginated queries                     ${if (IS_LOCAL) queryWorksWithPaging else skipped("Not running paging test in integration mode")}

                                                  ${step(deleteTestTable)}
                                                  ${step(stopLocalDynamoDB)}
  """

  def getWorksIfNoValue =
    table.get(Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)) must returnValue(Option.empty[Value])

  import Write.Mode._

  def newWriteWorks =
    Prop.forAll { (key: Key, value: Value) =>
      (for {
        _ <- table.putIfAbsent(key, value)
        v <- table.get(key)
      } yield v) must returnValue(Some(value))
    }.set(minTestsOk = NUM_TESTS)

  def writeReplaceWorks =
    Prop.forAll { (key: Key, value: Value, value2: Value) =>
      (for {
        firstWrite <- table.putIfAbsent(key, value)
        firstGet <- table.get(key)
        secondWrite <- table.replace(key, value, value2)
        secondGet <- table.get(key)
      } yield (firstWrite, firstGet, secondWrite, secondGet)
      ) must returnValue((Insert.New, Some(value), Replace.Wrote, Some(value2)))
    }.set(minTestsOk = NUM_TESTS)

  def updateWithDeletedFieldWorks =
    Prop.forAll { (key: Key, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        firstWrite <- table.putIfAbsent(key, value1)
        update <- table.replace(key, value1, value2)
        secondGet <- table.get(key)
      } yield (firstWrite, update, secondGet)) must returnValue((Insert.New, Replace.Wrote, Some(value2)))
    }.set(minTestsOk = NUM_TESTS)

  def updateFailsIfDifferent =
    Prop.forAll { (key: Key, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        _ <- table.putIfAbsent(key, value1)
        update <- table.replace(key, value2, value1)
      } yield update) must returnValue(Replace.Failed)
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorks =
    Prop.forAll { (key: Key, value: Value) =>
      (for {
        _ <- table.putIfAbsent(key, value)
        _ <- table.delete(key)
        result <- table.get(key)
      } yield result) must returnValue(None)
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorksForNonExistentKey =
    table.delete(Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)) must returnSuccess

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
        runFree(table.batchPut(valuesToSave))
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
    Prop.forAll { (k: Key, v1: Value, v2: Value) =>
      val k2 = k.copy(seq = k.seq + 1)
      val hashKey = HashKey(k.a, k.b, k.c)
      val queryAsc = table.Query.hash(hashKey)
      val queryDesc = queryAsc.withConfig(table.Query.Config(direction = ScanDirection.Descending))
      (for {
        _ <- table.putIfAbsent(k, v1)
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
    }.set(minTestsOk = NUM_TESTS)

  def queryForHashAndRangeWorks =
    Prop.forAll { (k: Key, v1: Value, v2: Value, v3: Value) =>
      val k2 = k.copy(seq = k.seq + 1)
      val k3 = k2.copy(seq = k2.seq + 1)
      val hashKey = HashKey(k.a, k.b, k.c)
      val query = table.Query.range(hashKey, RangeKey(k2.seq), Comparison.Lte)
      (for {
        _ <- table.putIfAbsent(k, v1)
        _ <- table.putIfAbsent(k2, v2)
        _ <- table.putIfAbsent(k3, v3)
        result <- table.query(query)
      } yield result) must returnResult { page =>
        page.result must equal(List(v1, v2)) and
          (page.next must beNone)
      }
    }.set(minTestsOk = NUM_TESTS)

  def createTestTable() =
    DynamoDBOps.createTable(Schema.Create.complexKey(table.schema, defaultThroughput))

  def deleteTestTable =
    DynamoDBOps.deleteTable(table.schema)
}
