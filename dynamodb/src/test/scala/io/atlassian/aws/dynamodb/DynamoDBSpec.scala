package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec

import scala.collection.JavaConverters.{ mapAsJavaMapConverter, mapAsScalaMapConverter }

import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.Prop
import org.specs2.main.Arguments
import org.specs2.specification.Step

import com.amazonaws.services.dynamodbv2.model.{ ConditionalCheckFailedException, AttributeAction, AttributeValueUpdate, UpdateItemRequest }

import java.util.UUID.randomUUID
import scalaz.syntax.id._, scalaz.std.AllInstances._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class DynamoDBSpec(val arguments: Arguments) extends ScalaCheckSpec with LocalDynamoDBSpec {
  import TestData._, Attempt._

  val NUM_TESTS =
    if (IS_LOCAL) 100
    else 10

  val NUM_PAGING_TESTS =
    if (IS_LOCAL) 20
    else 1

  implicit val DYNAMO_CLIENT = dynamoClient

  implicit val table = tableNamed(s"my_things2_${System.currentTimeMillis.toString}")

  // TODO - These tests are sequential because of flakiness with integration tests.
  def is = stopOnFail ^ sequential ^ s2"""

  This is a specification to test DynamoDB actions.

  Set up local DB if required                     ${Step(startLocalDynamoDB)}
  DynamoDB library should                         ${Step(createTestTable)}
    have a working get                            $getWorks
    returns none if there is no value             $getWorksIfNoValue
    fails gracefully if there is a deserialisation error $getWorksIfCantDeserialize
    put a new value correctly                     $newPutWorks
    put replaces a value correctly                $putReplaceWorks
    update with a deleted field works             $updateWithDeletedFieldWorks
    have a working delete                         $deleteWorks
    handle non-existent keys on delete            $deleteWorksForNonExistentKey
    have a working describeTable                  $describeTableWorks
    have a describeTable that handles unknown tables $describeTableHandlesUnknownTable
    return error when trying to replace an entry while NoOverwrite is set $noOverwriteWorks

  DynamoDB query capability should
    support querying for non-existent hash keys   $queryWorksWhenHashKeyDoesntExist
    correctly handle sort ordering of range keys  $querySortOrderWorks
    support querying for hash and range keys      $queryForHashAndRangeWorks
    support paginated queries                     $queryWorksWithPaging

                                                  ${Step(deleteTestTable)}
                                                  ${Step(stopLocalDynamoDB)}
  """

  def getWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value) =>
      val key = Key.column.marshaller.toFlattenedMap(thingKey)
      val value = Value.column.marshaller.toFlattenedMap(thingValue).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }
      val putRequest =
        new UpdateItemRequest()
          .withTableName(table.name)
          .withKey(key.asJava)
          .withAttributeUpdates(value.asJava)

      DYNAMO_CLIENT.updateItem(putRequest)

      DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column) must returnValue(Some(thingValue))
  }.set(minTestsOk = NUM_TESTS)

  def getWorksIfNoValue =
    DynamoDB.get[Key, Value](Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L))(Key.column, Value.column) must returnValue(None)

  def getWorksIfCantDeserialize = Prop.forAll {
    (thingKey: Key, thingValue: Value) =>
      case class Value2(foo: String)
      val Value2Marshaller =
        Marshaller.from[Value2](
          a => Map(Marshaller.set("foo", a.foo))
        )

      val thingKey = Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)
      val thingValue2 = Value2(randomUUID.toString)
      val key = Key.column.marshaller.toFlattenedMap(thingKey)
      val value = Value2Marshaller.toFlattenedMap(thingValue2).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }
      val putRequest =
        new UpdateItemRequest()
          .withTableName(table.name)
          .withKey(key.asJava)
          .withAttributeUpdates(value.asJava)

      DYNAMO_CLIENT.updateItem(putRequest)

      DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column) must returnFailure
  }.set(minTestsOk = NUM_TESTS)

  def newPutWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value) =>
      DynamoDB.put[Key, Value](thingKey, thingValue)(Key.column, Value.column) must returnValue(None) and
        ((DYNAMO_CLIENT.getItem(table.name, Key.column.marshaller.toFlattenedMap(thingKey).asJava).getItem.asScala.toMap |>
          Value.column.unmarshaller.fromMap) must equal(Attempt.ok(thingValue)))
  }.set(minTestsOk = NUM_TESTS)

  def putReplaceWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value, thingValue2: Value) =>
      (for {
        firstPut <- DynamoDB.put[Key, Value](thingKey, thingValue)(Key.column, Value.column)
        firstGet <- DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column)
        secondPut <- DynamoDB.put[Key, Value](thingKey, thingValue2)(Key.column, Value.column)
        secondGet <- DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column)
      } yield (firstPut, firstGet, secondPut, secondGet)) must returnValue((None, Some(thingValue), Some(thingValue), Some(thingValue2)))
  }.set(minTestsOk = NUM_TESTS)

  def updateWithDeletedFieldWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value, date: DateTime) =>
      val thingValue1 = thingValue.copy(deletedTimestamp = Some(date))
      val thingValue2 = thingValue.copy(deletedTimestamp = None)
      (for {
        firstPut <- DynamoDB.put[Key, Value](thingKey, thingValue1)(Key.column, Value.column)
        update <- DynamoDB.update[Key, Value](thingKey, thingValue1, thingValue2)(Key.column, Value.column)
        secondGet <- DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column)
      } yield (firstPut, update, secondGet)) must returnValue((None, Some(thingValue1), Some(thingValue2)))
  }.set(minTestsOk = NUM_TESTS)

  def deleteWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value) =>
      (for {
        _ <- DynamoDB.put[Key, Value](thingKey, thingValue)(Key.column, Value.column)
        _ <- DynamoDB.delete[Key, Value](thingKey)(Key.column)
        result <- DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column)
      } yield result) must returnValue(None)
  }.set(minTestsOk = NUM_TESTS)

  def deleteWorksForNonExistentKey =
    DynamoDB.delete[Key, Value](Key(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L))(Key.column) must returnSuccess

  def noOverwriteWorks = Prop.forAll {
    (thingKey: Key, thingValue: Value, thingValue2: Value) =>
      (for {
        firstPut <- DynamoDB.put[Key, Value](thingKey, thingValue)(Key.column, Value.column)
        firstGet <- DynamoDB.get[Key, Value](thingKey)(Key.column, Value.column)
        secondPut <- DynamoDB.put[Key, Value](thingKey, thingValue2, OverwriteMode.NoOverwrite)(Key.column, Value.column)
      } yield firstPut) must returnException[Option[Value], ConditionalCheckFailedException]
  }.set(minTestsOk = NUM_TESTS)

  def describeTableWorks =
    DynamoDB.describeTable(table.name).map(_.getTableName) must returnValue(table.name)

  def describeTableHandlesUnknownTable =
    DynamoDB.describeTable("some_dodgy_table") must returnFailure

  def queryWorksWhenHashKeyDoesntExist = Prop.forAll {
    (k: Key) =>
      val hashKey = HashKey(k.a, k.b, k.c)
      DynamoDB.query(QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column))(Value.column) must returnResult { page =>
        page.result.isEmpty && page.next.isEmpty
      }
  }.set(minTestsOk = NUM_TESTS)

  def queryWorksWithPaging = Prop.forAll {
    (k: Key, v: Value) =>
      // Generate a really long string to max out item size
      val str = (1 to 12000).toList.map { _ => 'a' }.mkString
      val valueToSave = v.copy(hash = str)

      (1 to 200).sliding(25, 25).foreach { window =>
        val valuesToSave = window.map { i =>
          k.copy(seq = i.toLong) -> valueToSave.copy(length = i.toLong)
        }.toMap
        DynamoDB.batchPut(valuesToSave)(Key.column, Value.column).run(DYNAMO_CLIENT).run
      }

      val hashKey = HashKey(k.a, k.b, k.c)
      val query = QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column)

      (for {
        result <- DynamoDB.query(query)(Value.column)
      } yield result) must returnResult { page =>
        page.next must not beNone
      }
  }.set(minTestsOk = NUM_PAGING_TESTS) // This test takes ages, so don't run it that much

  def querySortOrderWorks = Prop.forAll {
    (k: Key, v1: Value, v2: Value) =>
      val k2 = k.copy(seq = k.seq + 1)
      val hashKey = HashKey(k.a, k.b, k.c)
      val queryAsc = QueryImpl.forHash[HashKey](hashKey)(table.name, HashKey.column)
      val queryDesc = QueryImpl.forHash[HashKey](hashKey = hashKey, scanDirection = ScanDirection.Descending)(table.name, HashKey.column)
      (for {
        _ <- DynamoDB.put(k, v1)(Key.column, Value.column)
        _ <- DynamoDB.put(k2, v2)(Key.column, Value.column)
        ascResult <- DynamoDB.query(queryAsc)(Value.column)
        descResult <- DynamoDB.query(queryDesc)(Value.column)
      } yield (ascResult, descResult)) must returnResult {
        case (page1, page2) =>
          page1.result must equal(List(v1, v2)) and
            (page2.result must equal(List(v2, v1))) and
            (page1.next must beNone) and
            (page2.next must beNone)
      }
  }.set(minTestsOk = NUM_TESTS)

  def queryForHashAndRangeWorks = Prop.forAll {
    (k: Key, v1: Value, v2: Value, v3: Value) =>
      val k2 = k.copy(seq = k.seq + 1)
      val k3 = k2.copy(seq = k2.seq + 1)
      val hashKey = HashKey(k.a, k.b, k.c)
      val query = QueryImpl.forHashAndRange[HashKey, RangeKey](hashKey, RangeKey(k2.seq), Comparison.Lte)(table.name, HashKey.column, RangeKey.column)
      (for {
        _ <- DynamoDB.put(k, v1)(Key.column, Value.column)
        _ <- DynamoDB.put(k2, v2)(Key.column, Value.column)
        _ <- DynamoDB.put(k3, v3)(Key.column, Value.column)
        result <- DynamoDB.query(query)(Value.column)
      } yield result) must returnResult { page =>
        page.result must equal(List(v1, v2)) and
          (page.next must beNone)
      }
  }.set(minTestsOk = NUM_TESTS)

  def createTestTable() =
    createTable[Key, Value]

  def deleteTestTable =
    deleteTable[Key, Value]
}
