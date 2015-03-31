package io.atlassian.aws
package dynamodb

import scalaz.~>
import scalaz.concurrent.Task
import scalaz.std.list._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._
import Unmarshaller._

/**
 * Contains functions that perform operations on a DynamoDB table. Functions return a DynamoDBAction that can be run by
 * providing an instance of an AmazonDynamoDBClient (see AmazonClient for
 * convenient constructor functions).
 *
 * This class is generally not intended to be used directly, but used through the Table algebra with column definitions.
 *
 * Tables are represented as key-value mappings, so you need classes to represent the key and the value. In addition,
 * you need to create instances of:
 *   * TODO describe new Column based definition
 *   * Table - specify the key, value, hash key and range key types, and a TableDefinition (which includes a name, the
 *       key types, and a couple of other DynamoDB parameters).
 *   * Columns - Columns map your Scala types into columns in DynamoDB i.e. start with a Column with a name for each
 *       column in DynamoDB, and then create composite columns using Column.composeX to be able to map your high-level
 *       Scala classes.
 *      * Encoders/Decoders - Under the covers, we use Encoders/Decoders to convert 'primitive' or low-level Scala types
 *          into suitable values for DynamoDB (ints, strings, dates). In most cases you don't need to be concerned with
 *          Encoders/Decoders (they will be picked up automatically in your Column definition). However, you
 *          you can extend the standard set if you need to.
 */
object DynamoDB {

  import scala.concurrent.duration._
  import DynamoDBAction.withClient

  def get[K, V](key: K, consistency: ReadConsistency = ReadConsistency.Eventual)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Option[V]] =
    withClient {
      _.getItem(table, kc.marshall.toFlattenedMap(key).asJava, ReadConsistency.asBool(consistency))
    }.flatMap { r =>
      DynamoDBAction.attempt {
        vc.unmarshall.option(r.getItem)
      }
    }

  def put[K, V](key: K, value: V, overwrite: OverwriteMode = OverwriteMode.Overwrite)(table: String, kc: Column[K], vc: Column[V], update: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    doUpdate(key, update.asNew(value)(vc), overwrite)(table, kc, vc, update)

  def update[K, V](key: K, original: V, newValue: V)(table: String, kc: Column[K], vc: Column[V], update: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    doUpdate(key, update.asUpdated(original, newValue), OverwriteMode.Overwrite)(table, kc, vc, update)

  def delete[K, V](key: K)(table: String, col: Column[K]): DynamoDBAction[DeleteItemResult] =
    withClient {
      _.deleteItem(table, col.marshall.toFlattenedMap(key).asJava)
    }

  /** takes a Range Key */
  def query[KR, V](q: QueryImpl)(ck: Column[KR], cv: Column[V]): DynamoDBAction[Page[KR, V]] =
    DynamoDBAction.withClient {
      _.query(q.asQueryRequest)
    } flatMap { res =>
      DynamoDBAction.attempt {
        res.getItems.asScala.toList.traverse[Attempt, V] {
          cv.unmarshall.unmarshall
        }.map { vs =>
          Page(vs,
            Option(res.getLastEvaluatedKey).flatMap {
              lastKey => ck.unmarshall(lastKey.asScala.toMap).toOption
            }
          )
        }
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
   */
  def batchPut[K, V](keyValues: Map[K, V])(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Map[K, V]] =
    withClient {
      _.batchWriteItem {
        new BatchWriteItemRequest().withRequestItems {
          Map(
            table -> keyValues.map {
              case (k, v) => new WriteRequest().withPutRequest {
                new PutRequest().withItem {
                  (kc.marshall.toFlattenedMap(k) ++ vc.marshall.toFlattenedMap(v)).asJava
                }
              }
            }.toList.asJava
          ).asJava
        }
      }.getUnprocessedItems.asScala.get(table).map {
        _.asScala.map { req => Column.unmarshall(kc, vc)(req.getPutRequest.getItem.asScala.toMap).toOption }.flatten.toMap
      }.getOrElse { Map() }
    }

  /**
   * Creates a table for the given entity. The table name comes from the entity and is transformed by the
   * tableNameTransformer (e.g. to create tables for different environments)
   */
  private[dynamodb] def createTable[K, V, H, R](table: TableDefinition[K, V, H, R], checkTableActiveIntervals: Seq[Duration] = Seq.fill(12)(5000.milli)): DynamoDBAction[Task[TableDescription]] =
    withClient {
      _.createTable {
        new CreateTableRequest().withTableName(table.name)
          .withAttributeDefinitions(table.attributeDefinitions.asJavaCollection)
          .withKeySchema(table.schemaElements.asJavaCollection)
          .withProvisionedThroughput(table.provisionedThroughput)
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
  private[dynamodb] def deleteTable[K, V, H, R](Table: TableDefinition[K, V, H, R]): DynamoDBAction[DeleteTableResult] =
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

  private def doUpdate[K, V](key: K, updateItemRequestEndo: UpdateItemRequestEndo, overwrite: OverwriteMode)(tableName: String, kc: Column[K], vc: Column[V], update: ValueUpdate[V]): DynamoDBAction[Option[V]] =
    DynamoDBAction.withClient {
      _.updateItem {
        new UpdateItemRequest()
          .withTableName(tableName)
          .withReturnValues(ReturnValue.ALL_OLD)
          .withKey(kc.marshall.toFlattenedMap(key).asJava) |> updateItemRequestEndo.run |> {
            req =>
              overwrite match {
                case OverwriteMode.NoOverwrite =>
                  req.withExpected {
                    kc.marshall.toFlattenedMap(key).map {
                      case (col, _) =>
                        col -> new ExpectedAttributeValue().withExists(false)
                    }.asJava
                  }
                case _ =>
                  req
              }
          }
      }
    }.flatMap { result =>
      DynamoDBAction.attempt {
        vc.unmarshall.option(result.getAttributes)
      }
    }

  def interpreter(kv: Table)(t: TableDefinition[kv.K, kv.V, kv.H, kv.R]): kv.DBOp ~> DynamoDBAction =
    new (kv.DBOp ~> DynamoDBAction) {
      import kv.DBOp._
      import kv.Query._
      def apply[A](fa: kv.DBOp[A]): DynamoDBAction[A] =
        fa match {
          case Get(k)                      => get(k)(t.name, t.key, t.value)
          case Put(k, v)                   => put(k, v)(t.name, t.key, t.value, t.update)
          case Update(v, original, newVal) => update(v, original, newVal)(t.name, t.key, t.value, t.update)
          case Delete(k)                   => delete(k)(t.name, t.key).map { _ => () }
          case QueryOp(q)                  => queryImpl(q)
          case TableExists                 => tableExists(t.name)
          case BatchPut(kvs)               => batchPut(kvs)(t.name, t.key, t.value)
        }

      def queryImpl: kv.Query => DynamoDBAction[Page[kv.R, kv.V]] = {
        case Hashed(h, Config(dir, _))         => query(QueryImpl.forHash(h, scanDirection = dir)(t.name, t.hash))(t.range, t.value)
        case Ranged(h, r, cmp, Config(dir, _)) => query(QueryImpl.forHashAndRange(h, r, rangeComparison = cmp, scanDirection = dir)(t.name, t.hash, t.range))(t.range, t.value)
      }
    }
}
