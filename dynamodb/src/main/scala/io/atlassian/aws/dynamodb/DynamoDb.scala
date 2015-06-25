package io.atlassian.aws
package dynamodb

import java.util

import scalaz.{NonEmptyList, Isomorphism, ~>}
import scalaz.Isomorphism.<=>
import scalaz.concurrent.Task
import scalaz.std.list._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import kadai.Invalid
import scala.collection.JavaConverters._
import Unmarshaller._
import AwsAction._
import com.amazonaws.services.dynamodbv2.model._

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
 *   * Table - specify the key, value, hash key and range key types, and a Schema (which includes a name, the
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
  import Convert._
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

  import Write.Mode._

  /**
   * Write a value using the supplied update mode semantics.
   *
   * Note that replace mode only works correctly if you supply an old value to replace.
   */
  def write[K, V](k: K, v: V, m: Write.Mode, old: Option[V] = None)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Write.Result[V, m.Mode]] =
    DynamoDBAction.withClient { client =>
      val keyAttrs = kc.marshall.toFlattenedMap(k)
      client.updateItem {
        new UpdateItemRequest()
          .withTableName { table }
          .withReturnValues { ReturnValue.ALL_OLD }
          .withKey { keyAttrs.asJava }
          .withAttributeUpdates { toUpdates(vc.marshall(v).filterNot { case (keyAttrName, _) => keyAttrs.contains(keyAttrName)}).asJava } |> { req =>
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
                      case None => new ExpectedAttributeValue().withExists(false)
                    }.asJava
                  }
                }
            }
          }
      }
    }.flatMap {
      res => DynamoDBAction.attempt { vc.unmarshall.option(res.getAttributes) }.map { m.result }
    }.handle {
      case Invalid.Err(e: ConditionalCheckFailedException) => DynamoDBAction.ok { m.fail }
    }

  def update[K, V](key: K, old: V, newValue: V)(table: String, kc: Column[K], vc: Column[V]): DynamoDBAction[Write.Result[V, Write.Mode.Replace.type]] =
    // TODO move this logic into the algebra
    write(key, newValue, Write.Mode.Replace, Some(old))(table, kc, vc) //.asInstanceOf[DynamoDBAction[Write.Result[V, Write.Mode.Replace[V]]]]

  def delete[K, V](key: K)(table: String, col: Column[K]): DynamoDBAction[DeleteItemResult] =
    withClient {
      _.deleteItem(table, col.marshall.toFlattenedMap(key).asJava)
    }

  def query[K, V](q: QueryImpl)(ck: Column[K], cv: Column[V]): DynamoDBAction[Page[K, V]] =
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
   * - item size must be < 64kb
   * - we can only batch put 25 items at a time
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

  implicit class ToOnelOps[A](val l: List[A]) extends AnyVal {
    def toOnel: Option[NonEmptyList[A]] = l match {
      case Nil    => None
      case h :: t => Some(NonEmptyList.nel(h, t))
    }
  }

  /**
   * Creates a table for the given entity. The table name comes from the entity and is transformed by the
   * tableNameTransformer (e.g. to create tables for different environments)
   */
  private[dynamodb] def createHashKeyTable[K, V, H: Decoder, R: Decoder](desc: Schema.Create[K, V, H, R], checkTableActiveIntervals: Seq[Duration] = Seq.fill(12)(5000.milli)): DynamoDBAction[Task[TableDescription]] =
    withClient {
      _.createTable { desc.convertTo[CreateTableRequest] }
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
   * Creates a table for the given entity. The table name comes from the entity and is transformed by the
   * tableNameTransformer (e.g. to create tables for different environments)
   */
  private[dynamodb] def createHashAndRangeKeyTable[K, H, R, V](t: Table.Index)(desc: Schema.Create[K, V, H, R], checkTableActiveIntervals: Seq[Duration] = Seq.fill(12)(5000.milli)): DynamoDBAction[Task[TableDescription]] =
    withClient {
      _.createTable { desc.convertTo[CreateTableRequest] }
    }.flatMap { result =>
      withClient { client =>
        Task {
          client.describeTable(result.getTableDescription.getTableName).getTable
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
  private[dynamodb] def deleteTable[K, V](table: Schema.KeyValue[K, V]): DynamoDBAction[DeleteTableResult] =
    withClient {
      _.deleteTable(table.name)
    }

  sealed trait ReadConsistency

  object ReadConsistency {

    case object Strong extends ReadConsistency

    case object Eventual extends ReadConsistency

    private[dynamodb] val asBool: ReadConsistency => Boolean = {
      case Strong => true
      case Eventual => false
    }
  }

  // the actual column updates
  private val toUpdates: KeyValue => Map[String, AttributeValueUpdate] =
    _.mapValues {
      case None => new AttributeValueUpdate().withAction(AttributeAction.DELETE)
      case Some(value) => new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(value)
    }

  /** simple table interpreter where the key is the hash key */
  def tableInterpreter(t: Table.Simple)(defn: Schema.KeyValue[t.K, t.V]): t.DBOp ~> DynamoDBAction =
    new (t.DBOp ~> DynamoDBAction) {
      def apply[A](fa: t.DBOp[A]): DynamoDBAction[A] = tableImpl(t)(defn)(fa)(Isomorphism.isoRefl)
    }

  def indexInterpreter[K](i: Table.Index)(defn: Schema.Index[i.K, i.V, i.H, i.R]): i.DBOp ~> DynamoDBAction =
    new (i.DBOp ~> DynamoDBAction) {

      import i.Query._

      def apply[A](fa: i.DBOp[A]): DynamoDBAction[A] =
        fa match {
          case i.QueryOp(q) => queryImpl(i)(defn)(Isomorphism.isoRefl)(q)
        }
    }

  private def tableImpl[K, A](kv: Table.Simple)(t: Schema.KeyValue[K, kv.V])(op: kv.DBOp[A])(implicit iso: kv.K <=> K): DynamoDBAction[A] = {
    def toMap: Map[kv.K, kv.V] => Map[K, kv.V] = _.toSeq.map{
      case (k, v) => (iso.to(k), v)
    }.toMap
    def fromMap: Map[K, kv.V] => Map[kv.K, kv.V] = _.toSeq.map{
      case (k, v) => (iso.from(k), v)
    }.toMap

    op match {
      case kv.GetOp(k) => get(iso.to(k))(t.name, t.key, t.value)
      case kv.WriteOp(k, v, mode) => write(iso.to(k), v, mode)(t.name, t.key, t.value).asInstanceOf[DynamoDBAction[A]] // cast required for path dependent type limitations
      case kv.ReplaceOp(k, old, v) => write(iso.to(k), v, Write.Mode.Replace, Some(old))(t.name, t.key, t.value)
      case kv.DeleteOp(k) => delete(iso.to(k))(t.name, t.key).map { _ => ()}
      case kv.TableExistsOp => tableExists(t.name)
      case kv.BatchPutOp(kvs) => batchPut(toMap(kvs))(t.name, t.key, t.value).map(fromMap)
    }
  }

  private def queryImpl[K](t: Table.Index)(idx: Schema.Index[K, t.V, t.H, t.R])(implicit iso: t.K <=> K): t.Query => DynamoDBAction[Page[t.K, t.V]] = {
    case t.Query.Hashed(h, t.Query.Config(_, dir, _, consistency)) =>
      query(QueryImpl.forHash(h, scanDirection = dir, indexName = idx.name, consistency = consistency)(idx.table.kv.name, idx.table.hashRange.a))(idx.table.kv.key, idx.table.kv.value) map {
        case Page(result, next) => Page(result, next.map(iso.from))
      }
    case t.Query.Ranged(h, r, cmp, t.Query.Config(_, dir, _, consistency)) =>
      query(QueryImpl.forHashAndRange(h, r, rangeComparison = cmp, scanDirection = dir, indexName = idx.name, consistency = consistency)(idx.table.kv.name, idx.table.hashRange.a, idx.table.hashRange.b))(idx.table.kv.key, idx.table.kv.value) map {
        case Page(result, next) => Page(result, next.map(iso.from))
      }
  }
}
