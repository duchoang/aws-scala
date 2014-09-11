package io.atlassian.aws
package dynamodb

import scalaz.syntax.id._
import scalaz.std.option._
import scalaz.concurrent.Task
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._

/**
 * Contains functions that perform operations on a DynamoDB table. Functions return a DynamoDBAction that can be run by
 * providing an instance of an AmazonDynamoDBClient (see [[AmazonClient]] for
 * convenient constructor functions).
 *
 * Tables are represented as key-value mappings, so you need classes to represent the key and the value. In addition,
 * you need to create instances of:
 *   * [[TableDefinition]] for the table.
 *   * [[Marshaller]] for the key
 *   * [[Marshaller]], [[Unmarshaller]] and [[StoreValue]] for the value
 */
object DynamoDB extends QueryOps {

  import io.atlassian.scalaz71.EnhancedScalazConcurrent._
  import scala.concurrent.duration._
  import DynamoDBAction.withClient

  def get[A, B](key: A, consistency: ReadConsistency = ReadConsistency.Eventual)(
    implicit evKeyMarshaller: Marshaller[A],
    evValueUnmarshaller: Unmarshaller[B],
    evMapping: TableDefinition[A, B]): DynamoDBAction[Option[B]] =
    withClient {
      _.getItem(evMapping.name, evKeyMarshaller.toFlattenedMap(key).asJava, ReadConsistency.asBool(consistency))
    }.flatMap {
      r => unmarshallOpt(r.getItem)
    }

  def put[A, B](key: A, value: B, overwrite: OverwriteMode = OverwriteMode.Overwrite)(
    implicit evKeyMarshaller: Marshaller[A],
    evValueMarshaller: Marshaller[B],
    evValueUnmarshaller: Unmarshaller[B],
    evValue: StoreValue[B],
    evMapping: TableDefinition[A, B]): DynamoDBAction[Option[B]] =
    doUpdate(key, evValue.asNew(value), overwrite)

  def update[A, B](key: A, original: B, newValue: B)(
    implicit evKeyMarshaller: Marshaller[A],
    evValueMarshaller: Marshaller[B],
    evValueUnmarshaller: Unmarshaller[B],
    evValue: StoreValue[B],
    evMapping: TableDefinition[A, B]): DynamoDBAction[Option[B]] =
    doUpdate(key, evValue.asUpdated(original, newValue), OverwriteMode.Overwrite)

  def delete[A, B](key: A)(
    implicit evKeyMarshaller: Marshaller[A],
    evMapping: TableDefinition[A, B]): DynamoDBAction[DeleteItemResult] =
    withClient {
      _.deleteItem(evMapping.name, evKeyMarshaller.toFlattenedMap(key).asJava)
    }

  def tableExists(tableName: String): DynamoDBAction[Boolean] = withClient { client =>
    try { client.describeTable(tableName).getTable.getTableName == tableName } catch {
      case (_: ResourceNotFoundException) => false
    }
  }

  /**
   * Perform a batch put operation using the given key -> value pairs. DynamoDB has the following restrictions:
   *   - item size must be < 64kb
   *   - we can only batch put 25 items at a time
   *
   * @param keyValues The key -> value pairs to save
   * @param evKeyMarshaller marshaller for the key
   * @param evKeyUnmarshaller unmarshaller for the key
   * @param evValueMarshaller marshaller for the value
   * @param evValueUnmarshaller unmarshaller for the value
   * @param evMapping Table definition for the key -> value store
   * @tparam A The key type
   * @tparam B The value type
   * @return Map of key -> values that failed to be saved.
   */
  def batchPut[A, B](keyValues: Map[A, B])(
    implicit evKeyMarshaller: Marshaller[A],
    evKeyUnmarshaller: Unmarshaller[A],
    evValueMarshaller: Marshaller[B],
    evValueUnmarshaller: Unmarshaller[B],
    evMapping: TableDefinition[A, B]): DynamoDBAction[Map[A, B]] =
    withClient {
      client =>
        val writeRequests =
          keyValues.map {
            case (k, v) =>
              val attributes = evKeyMarshaller.toFlattenedMap(k) ++ evValueMarshaller.toFlattenedMap(v)
              new WriteRequest().withPutRequest(
                new PutRequest().withItem(
                  attributes.asJava
                )
              )
          }.toList.asJava
        val requestItems = Map(evMapping.name -> writeRequests).asJava
        client.batchWriteItem(new BatchWriteItemRequest().withRequestItems(requestItems))
          .getUnprocessedItems.asScala.get(evMapping.name) match {
            case None => Map()
            case Some(reqs) => reqs.asScala.map { req =>
              val item = req.getPutRequest.getItem.asScala.toMap
              (for {
                failedKey <- evKeyUnmarshaller.fromMap(item)
                failedValue <- evValueUnmarshaller.fromMap(item)
              } yield failedKey -> failedValue).toOption
            }.flatten.toMap
          }

    }

  /**
   * Creates a table for the given entity. The table name comes from the entity and is transformed by the
   * tableNameTransformer (e.g. to create tables for different environments)
   * @tparam A The key of the table
   * @tparam B The value of the table
   * @param checkTableActiveIntervals Tables take a while to become active. This is a sequence of interval between testing
   *                                  tests for whether the table is active.
   * @param evMapping Enforces a mapping between A and B that represents the table.
   * @return DynamoDBAction that when run provides a Task for creating the table. The task will return when the table
   *         is active.
   */
  private[dynamodb] def createTable[A, B](checkTableActiveIntervals: Seq[Duration] = Seq.fill(12)(5000.milli))(implicit evMapping: TableDefinition[A, B]): DynamoDBAction[Task[TableDescription]] =
    withClient {
      _.createTable {
        new CreateTableRequest().withTableName(evMapping.name)
          .withAttributeDefinitions(evMapping.attributeDefinitions.asJavaCollection)
          .withKeySchema(evMapping.schemaElements.asJavaCollection)
          .withProvisionedThroughput(evMapping.provisionedThroughput)
      }
    }.flatMap { createTableResult =>
      withClient { client =>
        Task {
          client.describeTable(createTableResult.getTableDescription.getTableName).getTable
        }.flatMap { description =>
          if (TableStatus.fromValue(description.getTableStatus) == TableStatus.ACTIVE)
            Task.now(description)
          else
            Task.fail(new RuntimeException("Table not ready"))
        }.retry(checkTableActiveIntervals)
      }
    }

  /**
   * Describes the table in DynamoDB
   */
  private[dynamodb] def describeTable(name: String): DynamoDBAction[TableDescription] =
    withClient {
      _.describeTable(name).getTable
    }

  /**
   * Deletes the table for the given entity. The table name comes from the entity and is transformed by the
   * tableNameTransformer (e.g. to create tables for different environments)
   */
  private[dynamodb] def deleteTable[A, B](implicit Table: TableDefinition[A, B]): DynamoDBAction[DeleteTableResult] =
    withClient {
      _.deleteTable(Table.name)
    }

  sealed trait ReadConsistency
  object ReadConsistency {
    case object Strong extends ReadConsistency
    case object Eventual extends ReadConsistency

    private[dynamodb] val asBool: ReadConsistency => Boolean = {
      case Strong   => true
      case Eventual => false
    }
  }

  private def doUpdate[A, B](key: A, updateItemRequestEndo: UpdateItemRequestEndo, overwrite: OverwriteMode)(
    implicit evKeyMarshaller: Marshaller[A],
    evValueMarshaller: Marshaller[B],
    evValueUnmarshaller: Unmarshaller[B],
    evValue: StoreValue[B],
    evMapping: TableDefinition[A, B]): DynamoDBAction[Option[B]] =
    DynamoDBAction.withClient {
      _.updateItem {
        new UpdateItemRequest()
          .withTableName(evMapping.name)
          .withReturnValues(ReturnValue.ALL_OLD)
          .withKey(evKeyMarshaller.toFlattenedMap(key).asJava) |> updateItemRequestEndo.run |> {
            req =>
              overwrite match {
                case OverwriteMode.NoOverwrite =>
                  req.withExpected {
                    evKeyMarshaller.toFlattenedMap(key).map {
                      case (col, _) =>
                        col -> new ExpectedAttributeValue().withExists(false)
                    }.asJava
                  }
                case _ =>
                  req
              }
          }
      }
    }.flatMap { result => unmarshallOpt(result.getAttributes) }
}
