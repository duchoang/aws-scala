package io.atlassian.aws.dynamodb

import scalaz.{ Coyoneda, Free, Monad, ~> }

/**
 * A key-value table.
 *
 * Implementations have concrete key and value types,
 * as well as queryable hash and range types.
 *
 * Table returns DBActions that are pure values (DBOps
 * that line inside Free) and can be chained together with
 * map/flatMap (ie. there is a monad for DBAction.
 *
 * You can get an interpreter for DBActions (to any arbitrary
 * C[_]) by supplying a DBOp ~> C natural transformation.
 */
trait Table extends Queries {
  type K
  type V

  type DBAction[A] = Free.FreeC[DBOp, A]

  import DBOp._

  def get(k: K): DBAction[Option[V]] =
    GetOp(k).action

  def putIfAbsent(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Insert.type]] =
    writeOp(k, v, Write.Mode.Insert).action

  def overwrite(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Overwrite.type]] =
    writeOp(k, v, Write.Mode.Overwrite).action

  def replace(k: K, old: V, v: V): DBAction[Write.Result[V, Write.Mode.Replace.type]] =
    ReplaceOp(k, old, v).action

  def delete(k: K): DBAction[Unit] =
    DeleteOp(k).action

  def query(q: Query): DBAction[Page[R, V]] =
    QueryOp(q).action

  def tableExists: DBAction[Boolean] =
    TableExistsOp.action

  /**
   * Perform a batch put operation using the given key -> value pairs. DynamoDB has the following restrictions:
   *   - item size must be < 64kb
   *   - we can only batch put 25 items at a time
   *
   * @param vals the vals to put in the batch
   * @return Map of key -> values that failed to be saved
   */
  def batchPut(vals: Map[K, V]): DBAction[Map[K, V]] =
    BatchPutOp(vals).action

  //
  // Ops
  //

  sealed trait DBOp[A]
  object DBOp {
    case class GetOp(key: K) extends DBOp[Option[V]]
    case class WriteOp[X] private[DBOp] (key: K, value: V, m: Write.Mode) extends DBOp[Write.Result[V, Write.Mode]]

    def writeOp(k: K, v: V, m: Write.Mode) =
      WriteOp(k, v, m).asInstanceOf[DBOp[Write.Result[V, m.type]]]

    case class ReplaceOp(key: K, old: V, value: V) extends DBOp[Write.Result[V, Write.Mode.Replace.type]]
    case class DeleteOp(key: K) extends DBOp[Unit]
    case class QueryOp(query: Query) extends DBOp[Page[R, V]]
    case class BatchPutOp(keyValues: Map[K, V]) extends DBOp[Map[K, V]]
    case object TableExistsOp extends DBOp[Boolean]
  }

  implicit val MonadDBAction: Monad[DBAction] =
    Free.freeMonad[Coyoneda[DBOp, ?]]

  private implicit class FreeOps[A](op: DBOp[A]) {
    def action: DBAction[A] = Free.liftFC(op)
  }

  /** Turn a natural transform from DBOp to C into a DBAction to C */
  def transform[C[_]: Monad](op2c: DBOp ~> C): DBAction ~> C =
    new (DBAction ~> C) {
      def apply[A](a: DBAction[A]): C[A] =
        Free.runFC[DBOp, C, A](a)(op2c)
    }
}
