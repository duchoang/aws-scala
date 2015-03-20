package io.atlassian.aws
package dynamodb

import scalaz.~>
import scalaz.syntax.id._
import scalaz.std.option._
import scalaz.concurrent.Task
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._

case class TableEnvironment[K, V](key: Column[K], value: Column[V], storeValue: StoreValue[V], tableDef: TableDefinition[K, V]) {
  private[dynamodb] object asImplicits {
    implicit val sv = storeValue
    implicit val td = tableDef
  }
}

/**
 * Contains functions that perform operations on a DynamoDB table. Functions return a DynamoDBAction that can be run by
 * providing an instance of an AmazonDynamoDBClient (see AmazonClient for
 * convenient constructor functions).
 *
 * Tables are represented as key-value mappings, so you need classes to represent the key and the value. In addition,
 * you need to create instances of:
 *   * [[TableDefinition]] for the table.
 *   * [[Marshaller]] for the key
 *   * [[Marshaller]], [[Unmarshaller]] and [[StoreValue]] for the value
 */
object DynamoDB extends QueryOps {

  import scala.concurrent.duration._
  import DynamoDBAction.withClient

  def get[K, V](key: K, consistency: ReadConsistency = ReadConsistency.Eventual)(kc: Column[K], vc: Column[V])(implicit evMapping: TableDefinition[K, V]): DynamoDBAction[Option[V]] =
    withClient {
      _.getItem(evMapping.name, kc.marshaller.toFlattenedMap(key).asJava, ReadConsistency.asBool(consistency))
    }.flatMap {
      r => vc.unmarshaller.option(r.getItem)
    }

  def put[K, V](key: K, value: V, overwrite: OverwriteMode = OverwriteMode.Overwrite)(kc: Column[K], vc: Column[V])(implicit evValue: StoreValue[V], evMapping: TableDefinition[K, V]): DynamoDBAction[Option[V]] =
    doUpdate(key, evValue.asNew(value)(vc), overwrite)(kc, vc)

  def update[K, V](key: K, original: V, newValue: V)(kc: Column[K], vc: Column[V])(
    implicit evValue: StoreValue[V], evMapping: TableDefinition[K, V]): DynamoDBAction[Option[V]] =
    doUpdate(key, evValue.asUpdated(original, newValue), OverwriteMode.Overwrite)(kc, vc)

  def delete[K, V](key: K)(col: Column[K])(
    implicit evMapping: TableDefinition[K, V]): DynamoDBAction[DeleteItemResult] =
    withClient {
      _.deleteItem(evMapping.name, col.marshaller.toFlattenedMap(key).asJava)
    }

  def tableExists(tableName: String): DynamoDBAction[Boolean] =
    withClient { client =>
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
   * @param tableDef Table definition for the key -> value store
   * @tparam K The key type
   * @tparam V The value type
   * @return Map of key -> values that failed to be saved.
   */
  def batchPut[K, V](keyValues: Map[K, V])(kc: Column[K], vc: Column[V])(
    implicit table: TableDefinition[K, V]): DynamoDBAction[Map[K, V]] =
    withClient {
      client =>
        val writeRequests =
          keyValues.map {
            case (k, v) =>
              new WriteRequest().withPutRequest(
                new PutRequest().withItem(
                  (kc.marshaller.toFlattenedMap(k) ++ vc.marshaller.toFlattenedMap(v)).asJava
                )
              )
          }.toList.asJava
        val requestItems = Map(table.name -> writeRequests).asJava
        client.batchWriteItem(new BatchWriteItemRequest().withRequestItems(requestItems))
          .getUnprocessedItems.asScala.get(table.name) match {
            case None => Map()
            case Some(reqs) => reqs.asScala.map { req =>
              val item = req.getPutRequest.getItem.asScala.toMap
              (for {
                failedKey <- kc.unmarshaller.fromMap(item)
                failedValue <- vc.unmarshaller.fromMap(item)
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
  private[dynamodb] def createTable[K, V](checkTableActiveIntervals: Seq[Duration] = Seq.fill(12)(5000.milli))(implicit evMapping: TableDefinition[K, V]): DynamoDBAction[Task[TableDescription]] =
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
  private[dynamodb] def deleteTable[K, V](implicit Table: TableDefinition[K, V]): DynamoDBAction[DeleteTableResult] =
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

  private def doUpdate[K, V](key: K, updateItemRequestEndo: UpdateItemRequestEndo, overwrite: OverwriteMode)(kc: Column[K], vc: Column[V])(
    implicit evValue: StoreValue[V], table: TableDefinition[K, V]): DynamoDBAction[Option[V]] =
    DynamoDBAction.withClient {
      _.updateItem {
        new UpdateItemRequest()
          .withTableName(table.name)
          .withReturnValues(ReturnValue.ALL_OLD)
          .withKey(kc.marshaller.toFlattenedMap(key).asJava) |> updateItemRequestEndo.run |> {
            req =>
              overwrite match {
                case OverwriteMode.NoOverwrite =>
                  req.withExpected {
                    kc.marshaller.toFlattenedMap(key).map {
                      case (col, _) =>
                        col -> new ExpectedAttributeValue().withExists(false)
                    }.asJava
                  }
                case _ =>
                  req
              }
          }
      }
    }.flatMap { result => vc.unmarshaller.option(result.getAttributes) }

  def interpreter(kv: KeyValueDB)(env: TableEnvironment[kv.K, kv.V]): kv.DBOp ~> DynamoDBAction =
    new (kv.DBOp ~> DynamoDBAction) {
      import env.asImplicits._
      def apply[A](fa: kv.DBOp[A]): DynamoDBAction[A] =
        fa match {
          case kv.Get(k)                         => get(k)(env.key, env.value)
          case kv.Put(k, v)                      => put(k, v)(env.key, env.value)
          case kv.Update(v, original, newVal)    => update(v, original, newVal)(env.key, env.value)
          case kv.Delete(k)                      => delete(k)(env.key).map { _ => () }
          case kv.TableExists(name)              => tableExists(name)
          case kv.BatchPut(kvs /*: Map[K, V]*/ ) => batchPut(kvs)(env.key, env.value)
        }
    }
}
