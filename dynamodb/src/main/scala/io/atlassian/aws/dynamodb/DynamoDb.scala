package io.atlassian.aws
package dynamodb

import scalaz.~>
import scalaz.concurrent.Task
import scalaz.std.list._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import kadai.Invalid
import scala.collection.JavaConverters._
import Unmarshaller._
import com.amazonaws.services.dynamodbv2.model.{
  AttributeAction,
  AttributeValue,
  AttributeValueUpdate,
  BatchWriteItemRequest,
  ComparisonOperator,
  ConditionalCheckFailedException,
  CreateTableRequest,
  DeleteItemResult,
  DeleteTableResult,
  ExpectedAttributeValue,
  PutRequest,
  ResourceNotFoundException,
  ReturnValue,
  TableDescription,
  TableStatus,
  UpdateItemRequest,
  WriteRequest
}

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
  import DynamoDBAction._

  def get[K, V](key: K, consistency: ReadConsistency = ReadConsistency.Eventual)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Option[V]] =
    withClient {
      _.getItem(table, kc.marshall.toFlattenedMap(key).asJava, ReadConsistency.asBool(consistency))
    }.flatMap { r =>
      DynamoDBAction.attempt {
        vc.unmarshall.option(r.getItem)
      }
    }

  import Write.Mode._
  /**
   * Write a value using the supplied update mode semantics.
   *
   * Note that replace mode only works correctly if you supply an old value to replace.
   */
  def write[K, V](k: K, v: V, m: Write.Mode, old: Option[V] = None)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Write.Result[V, m.Mode]] =
    DynamoDBAction.withClient {
      _.updateItem {
        new UpdateItemRequest()
          .withTableName {
            table
          }
          .withReturnValues {
            ReturnValue.ALL_OLD
          }
          .withKey {
            kc.marshall.toFlattenedMap(k).asJava
          }
          .withAttributeUpdates {
            toUpdates(vc.marshall(v)).asJava
          } |> { req =>
            m match {
              case Overwrite => req
              case Insert => // all keys shouldn't be present, should this be all values aren't present?
                req.withExpected {
                  kc.marshall.toFlattenedMap(k).map {
                    case (c, _) => c -> new ExpectedAttributeValue().withExists(false)
                  }.asJava
                }

              case Replace => // old value should be exactly the same
                old.fold(req) { v =>
                  req.withExpected {
                    vc.marshall(v.asInstanceOf[V]).mapValues {
                      case Some(v) => new ExpectedAttributeValue(v).withComparisonOperator(ComparisonOperator.EQ.toString)
                      case None    => new ExpectedAttributeValue().withExists(false)
                    }.asJava
                  }
                }
            }
          }
      }
    }.flatMap {
      res =>
        DynamoDBAction.attempt {
          vc.unmarshall.option(res.getAttributes)
        }.map {
          m.result
        }
    }.handle {
      case Invalid.Err(e: ConditionalCheckFailedException) => DynamoDBAction.ok {
        m.fail
      }
    }

  def update[K, V](key: K, old: V, newValue: V)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Write.Result[V, Write.Mode.Replace.type]] =
    // TODO move this logic into the algebra
    write(key, newValue, Write.Mode.Replace, Some(old))(table, kc, vc) //.asInstanceOf[DynamoDBAction[Write.Result[V, Write.Mode.Replace[V]]]]

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
      _.describeTable(name)
    } map {
      _.getTable
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

  // the actual column updates
  private val toUpdates: KeyValue => Map[String, AttributeValueUpdate] =
    _.mapValues {
      case None        => new AttributeValueUpdate().withAction(AttributeAction.DELETE)
      case Some(value) => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(value)
    }

  def interpreter(kv: Table)(t: TableDefinition[kv.K, kv.V, kv.H, kv.R])(implicit EH: Encoder[kv.H], DH: Decoder[kv.H], ER: Encoder[kv.R], DR: Decoder[kv.R]): kv.DBOp ~> DynamoDBAction =
    new (kv.DBOp ~> DynamoDBAction) {
      import kv.DBOp._
      import kv.Query._
      def apply[A](fa: kv.DBOp[A]): DynamoDBAction[A] =
        fa match {
          case GetOp(k, c)          => get(k, c)(t.name, t.key, t.value)
          case WriteOp(k, v, mode)  => write(k, v, mode)(t.name, t.key, t.value).asInstanceOf[DynamoDBAction[A]] // cast required for path dependent type limitations
          case ReplaceOp(k, old, v) => write(k, v, Write.Mode.Replace, Some(old))(t.name, t.key, t.value)
          case DeleteOp(k)          => delete(k)(t.name, t.key).map { _ => () }
          case QueryOp(q)           => queryImpl(q)
          case TableExistsOp        => tableExists(t.name)
          case BatchPutOp(kvs)      => batchPut(kvs)(t.name, t.key, t.value)
        }

      def queryImpl: kv.Query => DynamoDBAction[Page[kv.R, kv.V]] = {
        case Hashed(h, Config(dir, limit, consistency))         => query(QueryImpl.forHash(h, scanDirection = dir, consistency = consistency, limit = limit)(t.name, t.hash))(t.range.column, t.value)
        case Ranged(h, r, cmp, Config(dir, limit, consistency)) => query(QueryImpl.forHashAndRange(h, r, rangeComparison = cmp, scanDirection = dir, consistency = consistency, limit = limit)(t.name, t.hash, t.range))(t.range.column, t.value)
      }
    }
}
