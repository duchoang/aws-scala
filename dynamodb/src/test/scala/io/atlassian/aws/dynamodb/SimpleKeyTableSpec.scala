package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec

import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.Prop
import org.scalacheck.Arbitrary._
import org.specs2.main.Arguments

import scalaz.syntax.id._, scalaz.std.AllInstances._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class SimpleKeyTableSpec (val arguments: Arguments)
    extends ScalaCheckSpec
    with LocalDynamoDB
    with DBActionMatchers {
  import TestData._

  object table extends Table.Simple {
    type K = HashKey
    type V = KeyValue
    val schema = simpleKeyTableNamed(s"my_things1_${System.currentTimeMillis.toString}")
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

                                                  ${step(deleteTestTable)}
                                                  ${step(stopLocalDynamoDB)}
  """

  def getWorksIfNoValue =
    table.get(arbitrary[HashKey].sample.get) must returnValue(Option.empty[KeyValue])

  import Write.Mode._

  def newWriteWorks =
    Prop.forAll { (keyValue: KeyValue) =>
      (for {
        _ <- table.putIfAbsent(keyValue.key, keyValue)
        v <- table.get(keyValue.key)
      } yield v) must returnValue(Some(keyValue))
    }.set(minTestsOk = NUM_TESTS)

  def writeReplaceWorks =
    Prop.forAll { (key: HashKey, value: Value, value2: Value) =>
      (for {
        firstWrite <- table.putIfAbsent(key, KeyValue(key, value))
        firstGet <- table.get(key)
        secondWrite <- table.replace(key, KeyValue(key, value), KeyValue(key, value2))
        secondGet <- table.get(key)
      } yield (firstWrite, firstGet, secondWrite, secondGet)
      ) must returnValue((Insert.New, Some(KeyValue(key, value)), Replace.Wrote, Some(KeyValue(key, value2))))
    }.set(minTestsOk = NUM_TESTS)

  def updateWithDeletedFieldWorks =
    Prop.forAll { (key: HashKey, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        firstWrite <- table.putIfAbsent(key, KeyValue(key, value1))
        update <- table.replace(key, KeyValue(key, value1), KeyValue(key, value2))
        secondGet <- table.get(key)
      } yield (firstWrite, update, secondGet)) must returnValue((Insert.New, Replace.Wrote, Some(KeyValue(key, value2))))
    }.set(minTestsOk = NUM_TESTS)

  def updateFailsIfDifferent =
    Prop.forAll { (key: HashKey, value: Value, date: DateTime) =>
      val value1 = value.copy(deletedTimestamp = Some(date))
      val value2 = value.copy(deletedTimestamp = None)
      (for {
        _ <- table.putIfAbsent(key, KeyValue(key, value1))
        update <- table.replace(key, KeyValue(key, value2), KeyValue(key, value1))
      } yield update) must returnValue(Replace.Failed)
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorks =
    Prop.forAll { (key: HashKey, value: Value) =>
      (for {
        _ <- table.putIfAbsent(key, KeyValue(key, value))
        _ <- table.delete(key)
        result <- table.get(key)
      } yield result) must returnValue(None)
    }.set(minTestsOk = NUM_TESTS)

  def deleteWorksForNonExistentKey =
    table.delete(arbitrary[HashKey].sample.get) must returnSuccess

  def createTestTable() =
    DynamoDBOps.createTable[HashKey, KeyValue, Nothing, Nothing](Schema.Create[HashKey, KeyValue, Nothing, Nothing](Schema.Standard(table.schema)))

  def deleteTestTable =
    DynamoDBOps.deleteTable(table.schema)
}
