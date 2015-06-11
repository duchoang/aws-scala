package io.atlassian.aws
package dynamodb

import scalaz.{ Coyoneda, Free, Monad, ~> }
import scalaz.Isomorphism.<=>

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
trait FreeTable {
  type K
  type V
  
  type DBAction[A] = Free.FreeC[DBOp, A]

  sealed trait DBOp[A]

  implicit def lift[A](op: DBOp[A]): DBAction[A] =
    Free.liftFC(op)

  implicit val MonadDBAction: Monad[DBAction] =
    Free.freeMonad[Coyoneda[DBOp, ?]]

  /** Turn a natural transform from DBOp to C into a DBAction to C */
  def transform[C[_]: Monad](op2c: DBOp ~> C): DBAction ~> C =
    new (DBAction ~> C) {
      def apply[A](a: DBAction[A]): C[A] =
        Free.runFC[DBOp, C, A](a)(op2c)
    }
}

trait TableView extends FreeTable {

  def get(k: K): DBAction[Option[V]] =
    GetOp(k)

  def tableExists: DBAction[Boolean] =
    TableExistsOp

  case class GetOp(key: K) extends DBOp[Option[V]]
  case object TableExistsOp extends DBOp[Boolean]
}


trait TableWrite extends FreeTable {

  def put(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Overwrite.type]] =
    writeOp(k, v, Write.Mode.Overwrite)

  def putIfAbsent(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Insert.type]] =
    writeOp(k, v, Write.Mode.Insert)

  def replace(k: K, old: V, v: V): DBAction[Write.Result[V, Write.Mode.Replace.type]] =
    ReplaceOp(k, old, v)

  def delete(k: K): DBAction[Unit] =
    DeleteOp(k)

  /**
   * Perform a batch put operation using the given key -> value pairs. DynamoDB has the following restrictions:
   *   - item size must be < 64kb
   *   - we can only batch put 25 items at a time
   *
   * @param vals the vals to put in the batch
   * @return Map of key -> values that failed to be saved
   */
  def batchPut(vals: Map[K, V]): DBAction[Map[K, V]] =
    BatchPutOp(vals)

  def writeOp(k: K, v: V, m: Write.Mode) =
    WriteOp(k, v, m).asInstanceOf[DBOp[Write.Result[V, m.type]]]

  case class WriteOp[X] private[TableWrite] (key: K, value: V, m: Write.Mode) extends DBOp[Write.Result[V, Write.Mode]]
  case class ReplaceOp(key: K, old: V, value: V) extends DBOp[Write.Result[V, Write.Mode.Replace.type]]
  case class DeleteOp(key: K) extends DBOp[Unit]
  case class BatchPutOp(keyValues: Map[K, V]) extends DBOp[Map[K, V]]
}

trait TableQuery extends FreeTable with Queries {

  def query(q: Query): DBAction[Page[K, V]] =
    QueryOp(q)

  case class QueryOp(query: Query) extends DBOp[Page[K, V]]
}

trait Table extends TableView with TableWrite { self =>
  
  sealed trait View {
    type V
  }
  case object KeyView extends View {
    type V = self.K
  }
  case object FullView extends View {
    type V = self.V
  }
  case class PartialView[VV](projection: V => VV) extends View {
    type V = VV
  }

  def globalSecondaryIndex[IH, IR](view: View) =
    new TableQuery {
      type K = self.K
      type V = view.V
      type H = IH
      type R = IR
    }
}

trait SimpleKeyTable extends Table

trait ComplexKeyTable extends Table with TableQuery { self =>

  def isoKey: K <=> (H, R)

  def localSecondaryIndex[IR](view: View) =
    new TableQuery {
      type K = self.K
      type V = view.V
      type H = self.H
      type R = IR
    }
}
