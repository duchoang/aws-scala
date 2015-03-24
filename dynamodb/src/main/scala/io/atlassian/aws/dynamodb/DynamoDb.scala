package io.atlassian.aws
package dynamodb

import scalaz.~>
import scalaz.concurrent.Task
import scalaz.std.option._
import scalaz.std.list._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._

case class TableEnvironment[K, H, R, V](key: Column[K], hash: NamedColumn[H], range: NamedColumn[R], value: Column[V], update: ValueUpdate[V], table: TableDefinition[K, V]) {
  private[dynamodb] object asImplicits {
    implicit val sv = update
    //implicit val td = table
  }
}

/**
 * Contains functions that perform operations on a DynamoDB table. Functions return a DynamoDBAction that can be run by
 * providing an instance of an AmazonDynamoDBClient (see AmazonClient for
 * convenient constructor functions).
 *
 * This class is generally not intended to be used directly, but used through the Table algebra and
 *
 * Tables are represented as key-value mappings, so you need classes to represent the key and the value. In addition,
 * you need to create instances of:
 *   * TODO describe new Column based definition
 */
object DynamoDB {

  import scala.concurrent.duration._
  import DynamoDBAction.withClient

  def get[K, V](key: K, consistency: ReadConsistency = ReadConsistency.Eventual)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Option[V]] =
    withClient {
      _.getItem(table, kc.marshaller.toFlattenedMap(key).asJava, ReadConsistency.asBool(consistency))
    }.flatMap {
      r => vc.unmarshaller.option(r.getItem)
    }

  def put[K, V](key: K, value: V, overwrite: OverwriteMode = OverwriteMode.Overwrite)(table: String, kc: Column[K], vc: Column[V])(implicit evValue: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    doUpdate(key, evValue.asNew(value)(vc), overwrite)(table, kc, vc)

  def update[K, V](key: K, original: V, newValue: V)(table: String, kc: Column[K], vc: Column[V])(implicit evValue: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    doUpdate(key, evValue.asUpdated(original, newValue), OverwriteMode.Overwrite)(table, kc, vc)

  def delete[K, V](key: K)(table: String, col: Column[K]): DynamoDBAction[DeleteItemResult] =
    withClient {
      _.deleteItem(table, col.marshaller.toFlattenedMap(key).asJava)
    }

  def query[V](q: QueryImpl)(col: Column[V]): DynamoDBAction[Page[V]] =
    DynamoDBAction.withClient {
      _.query(q.asQueryRequest)
    } flatMap { res =>
      res.getItems.asScala.toList.traverse[DynamoDBAction, V] {
        col.unmarshaller.unmarshall
      }.map {
        vs =>
          Page(vs,
            Option(res.getLastEvaluatedKey).map { lastKey =>
              QueryImpl.nextFromQuery(q, lastKey.asScala.toMap)
            }
          )
      }
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
  def batchPut[K, V](keyValues: Map[K, V])(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Map[K, V]] =
    withClient {
      _.batchWriteItem {
        new BatchWriteItemRequest().withRequestItems {
          Map(
            table -> keyValues.map {
              case (k, v) => new WriteRequest().withPutRequest {
                new PutRequest().withItem {
                  (kc.marshaller.toFlattenedMap(k) ++ vc.marshaller.toFlattenedMap(v)).asJava
                }
              }
            }.toList.asJava
          ).asJava
        }
      }.getUnprocessedItems.asScala.get(table) match {
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

  private def doUpdate[K, V](key: K, updateItemRequestEndo: UpdateItemRequestEndo, overwrite: OverwriteMode)(tableName: String, kc: Column[K], vc: Column[V])(
    implicit evValue: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    DynamoDBAction.withClient {
      _.updateItem {
        new UpdateItemRequest()
          .withTableName(tableName)
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

  def interpreter(kv: KeyValueDB)(env: TableEnvironment[kv.K, kv.H, kv.R, kv.V]): kv.DBOp ~> DynamoDBAction =
    new (kv.DBOp ~> DynamoDBAction) {
      import env.asImplicits._
      import kv.DBOp._
      import kv.Query._
      def apply[A](fa: kv.DBOp[A]): DynamoDBAction[A] =
        fa match {
          case Get(k)                      => get(k)(env.table.name, env.key, env.value)
          case Put(k, v)                   => put(k, v)(env.table.name, env.key, env.value)
          case Update(v, original, newVal) => update(v, original, newVal)(env.table.name, env.key, env.value)
          case Delete(k)                   => delete(k)(env.table.name, env.key).map { _ => () }
          case QueryOp(q)                  => queryImpl(q)
          case TableExists(name)           => tableExists(name)
          case BatchPut(kvs)               => batchPut(kvs)(env.table.name, env.key, env.value)
        }

      def queryImpl[A]: kv.Query => DynamoDBAction[Page[kv.V]] =
        _ match {
          case Hashed(h, cfg)         => query(QueryImpl.forHash(h)(env.table.name, env.hash))(env.value)
          case Ranged(h, r, cmp, cfg) => query(QueryImpl.forHashAndRange(h, r, cmp)(env.table.name, env.hash, env.range))(env.value)
        }
    }
}
