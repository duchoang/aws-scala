package io.atlassian.aws
package dynamodb

import io.atlassian.aws.dynamodb.DynamoDB.ReadConsistency

import scalaz.{ Coyoneda, Free, Isomorphism, Monad, ~> }
import Isomorphism.<=>

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
object Table {
  /** the basic operations and actions that all Table types support */
  trait Operations {
    type K
    type V

    final type DBAction[A] = Free.FreeC[DBOp, A]

    sealed trait DBOp[A]

    implicit def lift[A](op: DBOp[A]): DBAction[A] =
      Free.liftFC(op)

    implicit val MonadDBAction: Monad[DBAction] =
      Free.freeMonad[Coyoneda[DBOp, ?]]

    /** Turn a natural transform from DBOp to C into a DBAction to C */
    final def transform[C[_]: Monad](op2c: DBOp ~> C): DBAction ~> C =
      new (DBAction ~> C) {
        def apply[A](a: DBAction[A]): C[A] =
          Free.runFC[DBOp, C, A](a)(op2c)
      }
  }

  /** simple tables don't have a range key part defined and don't allow queries using ranges */
  trait Simple extends Operations { self =>
    
    final def get(k: K, consistency: ReadConsistency = ReadConsistency.Eventual): DBAction[Option[V]] =
      GetOp(k, consistency)

    final def tableExists: DBAction[Boolean] =
      TableExistsOp

    final def put(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Overwrite.type]] =
      writeOp(k, v, Write.Mode.Overwrite)

    final def putIfAbsent(k: K, v: V): DBAction[Write.Result[V, Write.Mode.Insert.type]] =
      writeOp(k, v, Write.Mode.Insert)

    final def replace(k: K, old: V, v: V): DBAction[Write.Result[V, Write.Mode.Replace.type]] =
      ReplaceOp(k, old, v)

    final def delete(k: K): DBAction[Unit] =
      DeleteOp(k)

    /**
     * Perform a batch put operation using the given key -> value pairs. DynamoDB has the following restrictions:
     *   - item size must be < 64kb
     *   - we can only batch put 25 items at a time
     *
     * @param vals the vals to put in the batch
     * @return Map of key -> values that failed to be saved
     */
    final def batchPut(vals: Map[K, V]): DBAction[Map[K, V]] =
      BatchPutOp(vals)

    final def writeOp(k: K, v: V, m: Write.Mode) =
      WriteOp(k, v, m).asInstanceOf[DBOp[Write.Result[V, m.type]]]

    sealed trait View {
      type V
    }
    object View {
      case object KeyOnly extends View {
        type V = self.K
      }
      case object Full extends View {
        type V = self.V
      }
      case class Partial[VV](projection: V => VV) extends View {
        type V = VV
      }
    }

    case class GetOp(key: K, consistency: ReadConsistency) extends DBOp[Option[V]]
    case object TableExistsOp extends DBOp[Boolean]
    case class WriteOp[X] private[Table] (key: K, value: V, m: Write.Mode) extends DBOp[Write.Result[V, Write.Mode]]
    case class ReplaceOp(key: K, old: V, value: V) extends DBOp[Write.Result[V, Write.Mode.Replace.type]]
    case class DeleteOp(key: K) extends DBOp[Unit]
    case class BatchPutOp(keyValues: Map[K, V]) extends DBOp[Map[K, V]]

    /** Global indexes can have different hash and range key types */
    final def globalSecondary[IH, IR](view: View) =
      new Index {
        val table = self

        type K = self.K
        type V = view.V
        type H = IH
        type R = IR
      }
  }

  trait Index extends Operations with Queries {
    val table: Operations // link to underlying table to unify the 
    
    final def query(q: Query): DBAction[Page[K, V]] =
      QueryOp(q)

    case class QueryOp(query: Query) extends DBOp[Page[K, V]]
  }

  /** key has hash and range parts, allows more query operations as well as local indexes */
  trait ComplexKey extends Simple with Index { self =>

    def keyIso: K <=> (H, R)

    final val table = this
    
    /** local indexes have the same hash but different range keys */
    final def localSecondary[IR](view: View) =
      new Index {
        val table = self 

        type K = self.K
        type V = view.V
        type H = self.H
        type R = IR
      }
  }
}