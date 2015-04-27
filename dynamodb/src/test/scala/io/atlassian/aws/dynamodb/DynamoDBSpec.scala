package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import spec.ScalaCheckSpec

import scala.collection.JavaConverters.{ mapAsJavaMapConverter, mapAsScalaMapConverter }

import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.Prop

import com.amazonaws.services.dynamodbv2.model.{ ConditionalCheckFailedException, AttributeAction, AttributeValueUpdate, UpdateItemRequest }

import java.util.UUID.randomUUID
import scalaz.syntax.id._, scalaz.std.AllInstances._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class DynamoDBSpec extends ScalaCheckSpec with LocalDynamoDB with DynamoDBActionMatchers {
  import TestData._, Attempt._

  val NUM_TESTS =
    if (IS_LOCAL) 100
    else 10

  val NUM_PAGING_TESTS =
    if (IS_LOCAL) 20
    else 1

  val table = tableNamed(s"my_things2_${System.currentTimeMillis.toString}")

  implicit val client = dynamoClient

  // TODO - These tests are sequential because of flakiness with integration tests.
  def is = {
    stopOnFail ^ sequential ^ s2"""

  This is a specification to test DynamoDB actions.

  Set up local DB if required                     ${step(startLocalDynamoDB)}
  DynamoDB library should                         ${step(createTestTable)}
    have a working get                            $getWorks
    returns none if there is no value             $getWorksIfNoValue
    fails gracefully if there is a deserialisation error $getWorksIfCantDeserialize
    write a new value correctly                   $newPutWorks
    write replaces a value correctly              $writeReplaceWorks
    update with a deleted field works             $updateWithDeletedFieldWorks
    update with incorrect value fails             $updateWithIncorrectValueFails
    have a working delete                         $deleteWorks
    handle non-existent keys on delete            $deleteWorksForNonExistentKey
    have a working describeTable                  $describeTableWorks
    have a describeTable that handles unknown tables $describeTableHandlesUnknownTable
    return error when trying to replace an entry while NoOverwrite is set $noOverwriteWorks

  DynamoDB query capability should
    support querying for non-existent hash keys   $queryWorksWhenHashKeyDoesntExist
    correctly handle sort ordering of range keys  $querySortOrderWorks
    support querying for hash and range keys      $queryForHashAndRangeWorks
    support paginated queries                     ${if (IS_LOCAL) queryWorksWithPaging else skipped("Not running paging test in integration mode")}

                                                  ${step(deleteTestTable)}
                                                  ${step(stopLocalDynamoDB)}
  """
  }

  def getWorks =
    Prop.forAll { (key: Key, value: Value) =>
      val keyAttr = Key.column.marshall.toFlattenedMap(key)
      val valueAttr = Value.column.marshall.toFlattenedMap(value).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }

      client.updateItem {
        new UpdateItemRequest()
          .withTableName(table.name)
          .withKey(keyAttr.asJava)
          .withAttributeUpdates(valueAttr.asJava)
      }

      DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column) must returnValue(Some(value))
    }.set(minTestsOk = NUM_TESTS)

  def getWorksIfNoValue =
    DynamoDB.get[Key, Value](Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L))(table.name, Key.column, Value.column) must returnValue(None)

  def getWorksIfCantDeserialize =
    Prop.forAll { (key: Key, value: Value, str: String) =>
      case class Value2(foo: String)
      implicit val Value2Encoder = Encoder[String].contramap[Value2] { _.foo }
      implicit val Value2Dcoder = Decoder[String].map[Value2] { Value2.apply }
      val column = Column[Value2]("foo")

      val keyAttr = Key.column.marshall.toFlattenedMap(key)
      val valueAttr = column.marshall.toFlattenedMap(new Value2(str)).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }

      client.updateItem {
        new UpdateItemRequest()
          .withTableName(table.name)
          .withKey(keyAttr.asJava)
          .withAttributeUpdates(valueAttr.asJava)
      }

      DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column) must returnFailure
    }.set(minTestsOk = NUM_TESTS)

  import Write.Mode._

  def newPutWorks =
    Prop.forAll { (key: Key, value: Value) =>
      DynamoDB.write[Key, Value](key, value, Overwrite)(table.name, Key.column, Value.column) must returnValue(Overwrite.New) and (
        (client.getItem(table.name, Key.column.marshall.toFlattenedMap(key).asJava).getItem.asScala.toMap |> Value.column.unmarshall)
        must equal(Attempt.ok(value))
      )
    }.set(minTestsOk = NUM_TESTS)

  def writeReplaceWorks =
    Prop.forAll { (key: Key, value: Value, value2: Value) =>
      (for {
        firstPut <- DynamoDB.write[Key, Value](key, value, Overwrite)(table.name, Key.column, Value.column)
        firstGet <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
        secondPut <- DynamoDB.write[Key, Value](key, value2, Overwrite)(table.name, Key.column, Value.column)
        secondGet <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
      } yield (firstPut, firstGet, secondPut, secondGet)) must returnValue((Overwrite.New, Some(value), Overwrite.Replaced(value), Some(value2)))
    }.set(minTestsOk = NUM_TESTS)

  def updateWithDeletedFieldWorks =
    Prop.forAll { (key: Key, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        firstPut <- DynamoDB.write[Key, Value](key, value1, Overwrite)(table.name, Key.column, Value.column)
        update <- DynamoDB.update[Key, Value](key, value1, value2)(table.name, Key.column, Value.column)
        secondGet <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
      } yield (firstPut, update, secondGet)) must returnValue((Overwrite.New, Replace.Wrote, Some(value2)))
    }.set(minTestsOk = NUM_TESTS)

  def updateWithIncorrectValueFails =
    Prop.forAll { (key: Key, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        _ <- DynamoDB.write[Key, Value](key, value1, Overwrite)(table.name, Key.column, Value.column)
        update <- DynamoDB.update[Key, Value](key, value2, value1)(table.name, Key.column, Value.column)
        get <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
      } yield (update, get)) must returnValue((Replace.Failed, Some(value1)))
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorks =
    Prop.forAll { (key: Key, value: Value) =>
      (for {
        _ <- DynamoDB.write[Key, Value](key, value, Overwrite)(table.name, Key.column, Value.column)
        _ <- DynamoDB.delete[Key, Value](key)(table.name, Key.column)
        result <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
      } yield result) must returnValue(None)
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorksForNonExistentKey =
    DynamoDB.delete[Key, Value](Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L))(table.name, Key.column) must returnSuccess

  def noOverwriteWorks =
    Prop.forAll { (key: Key, value: Value, value2: Value) =>
      (for {
        firstPut <- DynamoDB.write[Key, Value](key, value, Overwrite)(table.name, Key.column, Value.column)
        firstGet <- DynamoDB.get[Key, Value](key)(table.name, Key.column, Value.column)
        secondPut <- DynamoDB.write[Key, Value](key, value2, Insert)(table.name, Key.column, Value.column)
      } yield secondPut) must returnValue(Insert.Failed)
    }.set(minTestsOk = NUM_TESTS)

  def describeTableWorks =
    DynamoDB.describeTable(table.name).map(_.getTableName) must returnValue(table.name)

  def describeTableHandlesUnknownTable =
    DynamoDB.describeTable("some_dodgy_table") must returnFailure

  def queryWorksWhenHashKeyDoesntExist =
    Prop.forAll { (k: Key) =>
      val hashKey = HashKey(k.a, k.b, k.c)
      DynamoDB.query(QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column))(RangeKey.column, Value.column) must returnResult { page =>
        page.result.isEmpty && page.next.isEmpty
      }
    }.set(minTestsOk = NUM_TESTS)

  def queryWorksWithPaging =
    Prop.forAll { (k: Key, v: Value) =>
      // Generate a really long string to max out item size
      val str = (1 to 12000).toList.map { _ => 'a' }.mkString
      val valueToSave = v.copy(hash = str)

      (1 to 200).sliding(25, 25).foreach { window =>
        val valuesToSave = window.map { i =>
          k.copy(seq = i.toLong) -> valueToSave.copy(length = i.toLong)
        }.toMap
        DynamoDB.batchPut(valuesToSave)(table.name, Key.column, Value.column).run(client).run
      }

      val hashKey = HashKey(k.a, k.b, k.c)
      val query = QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column)

      (for {
        result <- DynamoDB.query(query)(RangeKey.column, Value.column)
      } yield result) must returnResult { page =>
        page.next must not beNone
      }
    }.set(minTestsOk = NUM_PAGING_TESTS) // This test takes ages, so don't run it that much

  def querySortOrderWorks =
    Prop.forAll { (k: Key, v1: Value, v2: Value) =>
      val k2 = k.copy(seq = k.seq + 1)
      val hashKey = HashKey(k.a, k.b, k.c)
      val queryAsc = QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column)
      val queryDesc = QueryImpl.forHash[HashKey](hashKey = hashKey, scanDirection = ScanDirection.Descending)(table.name, HashKey.column)
      (for {
        _ <- DynamoDB.write(k, v1, Overwrite)(table.name, Key.column, Value.column)
        _ <- DynamoDB.write(k2, v2, Overwrite)(table.name, Key.column, Value.column)
        ascResult <- DynamoDB.query(queryAsc)(RangeKey.column, Value.column)
        descResult <- DynamoDB.query(queryDesc)(RangeKey.column, Value.column)
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
      val query = QueryImpl.forHashAndRange[HashKey, RangeKey](hashKey, RangeKey(k2.seq), Comparison.Lte)(table.name, HashKey.column, RangeKey.column)
      (for {
        _ <- DynamoDB.write(k, v1, Overwrite)(table.name, Key.column, Value.column)
        _ <- DynamoDB.write(k2, v2, Overwrite)(table.name, Key.column, Value.column)
        _ <- DynamoDB.write(k3, v3, Overwrite)(table.name, Key.column, Value.column)
        result <- DynamoDB.query(query)(RangeKey.column, Value.column)
      } yield result) must returnResult { page =>
        page.result must equal(List(v1, v2)) and
          (page.next must beNone)
      }
    }.set(minTestsOk = NUM_TESTS)

  object TestTable extends Table {
    type K = Key
    type V = Value
    type H = HashKey
    type R = RangeKey

  }

  def createTestTable =
    DynamoDBOps.createTable[Key, Value, HashKey, RangeKey](table)

  def deleteTestTable =
    DynamoDBOps.deleteTable[Key, Value, HashKey, RangeKey](table)
}
