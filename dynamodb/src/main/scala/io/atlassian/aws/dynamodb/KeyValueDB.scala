package io.atlassian.aws.dynamodb

import scalaz.{ Coyoneda, Free, Monad }

/**
 * A single KV table. Implementations have concrete key and value types.
 */
trait KeyValueDB {
  type K
  type V

  type DBAction[A] = Free.FreeC[DBOp, A]

  def get(k: K): DBAction[Option[V]] =
    Free.liftFC { Get(k) }

  def put(k: K, v: V): DBAction[Option[V]] =
    Free.liftFC { Put(k, v) }

  def update(k: K, old: V, v: V): DBAction[Option[V]] =
    Free.liftFC { Update(k, old, v) }

  def delete(k: K): DBAction[Unit] =
    Free.liftFC { Delete(k) }

  def tableExists(name: String): DBAction[Boolean] =
    Free.liftFC { TableExists(name) }

  /**
   * Perform a batch put operation using the given key -> value pairs. DynamoDB has the following restrictions:
   *   - item size must be < 64kb
   *   - we can only batch put 25 items at a time
   *
   * @tparam K The key type
   * @tparam V The value type
   * @param vals the vals to put in the batch
   * @return Map of key -> values that failed to be saved
   */
  def batchPut(vals: Map[K, V]): DBAction[Map[K, V]] =
    Free.liftFC { BatchPut(vals) }

  sealed trait DBOp[A]
  case class Get(key: K) extends DBOp[Option[V]]
  case class Put(key: K, value: V) extends DBOp[Option[V]]
  case class Update(key: K, original: V, newValue: V) extends DBOp[Option[V]]
  case class Delete(key: K) extends DBOp[Unit]
  case class TableExists(tableName: String) extends DBOp[Boolean]
  case class BatchPut(keyValues: Map[K, V]) extends DBOp[Map[K, V]]

  implicit val MonadDBAction: Monad[DBAction] =
    Free.freeMonad[Coyoneda[DBOp, ?]]
}

object StringTable extends KeyValueDB {
  type K = String
  type V = String

  //  val table =
  //    TableEnvironment(
  //      Marshaller.fromColumn(Column[String]("name"))
  //    )
}