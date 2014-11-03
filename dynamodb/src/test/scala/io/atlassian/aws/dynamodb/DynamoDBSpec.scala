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

  implicit val ThingDynamoMapping = thingDynamoMappingForTableName(s"my_things2_${System.currentTimeMillis.toString}")

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
    (thingKey: ThingKey, thingValue: ThingValue) =>
      val key = Marshaller[ThingKey].toFlattenedMap(thingKey)
      val value = Marshaller[ThingValue].toFlattenedMap(thingValue).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }
      val putRequest =
        new UpdateItemRequest()
          .withTableName(ThingDynamoMapping.name)
          .withKey(key.asJava)
          .withAttributeUpdates(value.asJava)

      DYNAMO_CLIENT.updateItem(putRequest)

      DynamoDB.get[ThingKey, ThingValue](thingKey) must returnValue(Some(thingValue))
  }.set(minTestsOk = NUM_TESTS)

  def getWorksIfNoValue =
    DynamoDB.get[ThingKey, ThingValue](ThingKey(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)) must returnValue(None)

  def getWorksIfCantDeserialize = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue) =>
      case class ThingValue2(foo: String)
      implicit val ThingValue2DynamoSerializer =
        Marshaller.from[ThingValue2](
          a => Map(Marshaller.set("foo", a.foo))
        )

      val thingKey = ThingKey(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)
      val thingValue2 = ThingValue2(randomUUID.toString)
      val key = Marshaller[ThingKey].toFlattenedMap(thingKey)
      val value = Marshaller[ThingValue2].toFlattenedMap(thingValue2).mapValues {
        av => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(av)
      }
      val putRequest =
        new UpdateItemRequest()
          .withTableName(ThingDynamoMapping.name)
          .withKey(key.asJava)
          .withAttributeUpdates(value.asJava)

      DYNAMO_CLIENT.updateItem(putRequest)

      DynamoDB.get[ThingKey, ThingValue](thingKey) must returnFailure
  }.set(minTestsOk = NUM_TESTS)

  def newPutWorks = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue) =>
      DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue) must returnValue(None) and
        ((DYNAMO_CLIENT.getItem(ThingDynamoMapping.name, Marshaller[ThingKey].toFlattenedMap(thingKey).asJava).getItem.asScala.toMap |>
          Unmarshaller[ThingValue].fromMap) must equal(Attempt.ok(thingValue)))
  }.set(minTestsOk = NUM_TESTS)

  def putReplaceWorks = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue, thingValue2: ThingValue) =>
      (for {
        firstPut <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue)
        firstGet <- DynamoDB.get[ThingKey, ThingValue](thingKey)
        secondPut <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue2)
        secondGet <- DynamoDB.get[ThingKey, ThingValue](thingKey)
      } yield (firstPut, firstGet, secondPut, secondGet)) must returnValue((None, Some(thingValue), Some(thingValue), Some(thingValue2)))
  }.set(minTestsOk = NUM_TESTS)

  def updateWithDeletedFieldWorks = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue, date: DateTime) =>
      val thingValue1 = thingValue.copy(deletedTimestamp = Some(date))
      val thingValue2 = thingValue.copy(deletedTimestamp = None)
      (for {
        firstPut <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue1)
        update <- DynamoDB.update[ThingKey, ThingValue](thingKey, thingValue1, thingValue2)
        secondGet <- DynamoDB.get[ThingKey, ThingValue](thingKey)
      } yield (firstPut, update, secondGet)) must returnValue((None, Some(thingValue1), Some(thingValue2)))
  }.set(minTestsOk = NUM_TESTS)

  def deleteWorks = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue) =>
      (for {
        _ <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue)
        _ <- DynamoDB.delete[ThingKey, ThingValue](thingKey)
        result <- DynamoDB.get[ThingKey, ThingValue](thingKey)
      } yield result) must returnValue(None)
  }.set(minTestsOk = NUM_TESTS)

  def deleteWorksForNonExistentKey =
    DynamoDB.delete[ThingKey, ThingValue](ThingKey(randomUUID.toString, randomUUID.toString, randomUUID.toString, 0L)) must returnSuccess

  def noOverwriteWorks = Prop.forAll {
    (thingKey: ThingKey, thingValue: ThingValue, thingValue2: ThingValue) =>
      (for {
        firstPut <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue)
        firstGet <- DynamoDB.get[ThingKey, ThingValue](thingKey)
        secondPut <- DynamoDB.put[ThingKey, ThingValue](thingKey, thingValue2, OverwriteMode.NoOverwrite)
      } yield firstPut) must returnException[Option[ThingValue], ConditionalCheckFailedException]
  }.set(minTestsOk = NUM_TESTS)

  def describeTableWorks =
    DynamoDB.describeTable(ThingDynamoMapping.name).map(_.getTableName) must returnValue(ThingDynamoMapping.name)

  def describeTableHandlesUnknownTable =
    DynamoDB.describeTable("some_dodgy_table") must returnFailure

  def queryWorksWhenHashKeyDoesntExist = Prop.forAll {
    (k: ThingKey) =>
      val hashKey = ThingHashKey(k.tenant, k.app, k.blobId)
      DynamoDB.query(Query.forHash[ThingHashKey, ThingKey, ThingValue](hashKey)) must returnResult { page =>
        page.result.isEmpty && page.next.isEmpty
      }
  }.set(minTestsOk = NUM_TESTS)

  def queryWorksWithPaging = Prop.forAll {
    (k: ThingKey, v: ThingValue) =>
      // Generate a really long string to max out item size
      val str = (1 to 12000).toList.map { _ => 'a' }.mkString
      val valueToSave = v.copy(blobHash = str)

      (1 to 200).sliding(25, 25).foreach { window =>
        val valuesToSave = window.map { i =>
          k.copy(seq = i.toLong) -> valueToSave.copy(length = i.toLong)
        }.toMap
        DynamoDB.batchPut(valuesToSave).run(DYNAMO_CLIENT).run
      }

      val hashKey = ThingHashKey(k.tenant, k.app, k.blobId)
      val query = Query.forHash[ThingHashKey, ThingKey, ThingValue](hashKey)

      (for {
        result <- DynamoDB.query(query)
      } yield result) must returnResult { page =>
        page.next must not beNone
      }
  }.set(minTestsOk = NUM_PAGING_TESTS) // This test takes ages, so don't run it that much

  def querySortOrderWorks = Prop.forAll {
    (k: ThingKey, v1: ThingValue, v2: ThingValue) =>
      val k2 = k.copy(seq = k.seq + 1)
      val hashKey = ThingHashKey(k.tenant, k.app, k.blobId)
      val queryAsc = Query.forHash[ThingHashKey, ThingKey, ThingValue](hashKey)
      val queryDesc = Query.forHash[ThingHashKey, ThingKey, ThingValue](hashKey = hashKey, scanDirection = ScanDirection.Descending)
      (for {
        _ <- DynamoDB.put(k, v1)
        _ <- DynamoDB.put(k2, v2)
        ascResult <- DynamoDB.query(queryAsc)
        descResult <- DynamoDB.query(queryDesc)
      } yield (ascResult, descResult)) must returnResult {
        case (page1, page2) =>
          page1.result must equal(List(v1, v2)) and
            (page2.result must equal(List(v2, v1))) and
            (page1.next must beNone) and
            (page2.next must beNone)
      }
  }.set(minTestsOk = NUM_TESTS)

  def queryForHashAndRangeWorks = Prop.forAll {
    (k: ThingKey, v1: ThingValue, v2: ThingValue, v3: ThingValue) =>
      val k2 = k.copy(seq = k.seq + 1)
      val k3 = k2.copy(seq = k2.seq + 1)
      val hashKey = ThingHashKey(k.tenant, k.app, k.blobId)
      val query = Query.forHashAndRange[ThingHashKey, ThingRangeKey, ThingKey, ThingValue](hashKey, ThingRangeKey(k2.seq), Comparison.Lte)
      (for {
        _ <- DynamoDB.put(k, v1)
        _ <- DynamoDB.put(k2, v2)
        _ <- DynamoDB.put(k3, v3)
        result <- DynamoDB.query(query)
      } yield result) must returnResult { page =>
        page.result must equal(List(v1, v2)) and
          (page.next must beNone)
      }
  }.set(minTestsOk = NUM_TESTS)

  def createTestTable() =
    createTable[ThingKey, ThingValue]

  def deleteTestTable =
    deleteTable[ThingKey, ThingValue]
}
