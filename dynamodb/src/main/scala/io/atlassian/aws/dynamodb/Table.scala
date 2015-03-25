package io.atlassian.aws.dynamodb

import scalaz.{ Coyoneda, Free, Monad }

/**
 * A key-value table. Implementations have concrete key and value types.
 */
trait Table extends Queries {
  type K
  type V

  type DBAction[A] = Free.FreeC[DBOp, A]

  import DBOp._

  def get(k: K): DBAction[Option[V]] =
    Get(k).action

  def put(k: K, v: V): DBAction[Option[V]] =
    Put(k, v).action

  def update(k: K, old: V, v: V): DBAction[Option[V]] =
    Update(k, old, v).action

  def delete(k: K): DBAction[Unit] =
    Delete(k).action

  def query(q: Query): DBAction[Page[V]] =
    QueryOp(q).action

  def tableExists: DBAction[Boolean] =
    TableExists.action

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
    DBOp.BatchPut(vals).action

  //
  // Ops
  //

  sealed trait DBOp[A]
  object DBOp {
    case class Get(key: K) extends DBOp[Option[V]]
    case class Put(key: K, value: V) extends DBOp[Option[V]]
    case class Update(key: K, original: V, newValue: V) extends DBOp[Option[V]]
    case class Delete(key: K) extends DBOp[Unit]
    case class QueryOp(query: Query) extends DBOp[Page[V]]
    case class BatchPut(keyValues: Map[K, V]) extends DBOp[Map[K, V]]
    case object TableExists extends DBOp[Boolean]
  }

  implicit val MonadDBAction: Monad[DBAction] =
    Free.freeMonad[Coyoneda[DBOp, ?]]

  private implicit class FreeOps[A](op: DBOp[A]) {
    def action: DBAction[A] = Free.liftFC(op)
  }
}

object StringTable extends Table {
  type K = String
  type V = String

  //  val table =
  //    TableEnvironment(
  //      Marshaller.fromColumn(Column[String]("name"))
  //    )
}