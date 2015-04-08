package io.atlassian.aws.dynamodb

import scalaz.Equal

object Write {
  sealed trait Mode {
    type Mode = this.type

    private[dynamodb] def result[V]: Option[V] => Result[V, Mode]
    private[dynamodb] def fail[V]: Result[V, Mode]
  }
  /**
   * A successful result should always return the written value,
   * it may be different for instance have a write time-stamp.
   *
   * Use UpdateValue to control this
   */
  sealed trait Result[+A, M <: Mode]

  object Mode {
    /**
     * Overwrite any existing value, provides no protection against multiple writers.
     *
     * PUT semantics.
     */
    case object Overwrite extends Mode {
      case class Replaced[V](old: V) extends Result[V, Overwrite.type]
      case object New extends Result[Nothing, Overwrite.type]

      private[dynamodb] def result[V]: Option[V] => Result[V, Overwrite.type] = {
        case Some(v) => Replaced(v)
        case None    => New
      }
      private[dynamodb] def fail[V]: Result[V, Overwrite.type] =
        ??? // should never happen, cannot fail
    }

    /**
     * Only put a value if there is no value currently.
     *
     * PutIfAbsent semantics.
     */
    case object Insert extends Mode {
      /** return the written value, it may be different for instance have a write time-stamp */
      case object New extends Result[Nothing, Insert.type]
      case object Failed extends Result[Nothing, Insert.type]

      // if Insert fails we get exception
      private[dynamodb] def result[V]: Option[V] => Result[V, Insert.type] = {
        case None    => New
        case Some(v) => throw new AssertionError(s"we should never get back old data if we are Inserting only, got $v")
      }
      private[dynamodb] def fail[V] = Failed
    }

    /**
     * Only put a value if there is a specific value currently.
     *
     * Update semantics
     */
    //    // this being a case class and not a case object means some funky casts, but they're safe
    //    case class Replace[A](a: A) extends Mode {
    //    }
    case object Replace extends Mode {
      /** return the written value */
      case object Wrote extends Result[Nothing, Replace.type]
      case object Failed extends Result[Nothing, Replace.type]

      private[dynamodb] def result[V]: Option[V] => Result[V, Replace.type] = {
        case Some(v) => Wrote
        case None    => throw new AssertionError(s"we should always get back old data if we are Replacing")
      }
      private[dynamodb] def fail[V] = Failed
    }
  }

  object Result {
    import Mode._
    implicit def ResultEqual[A: Equal, M <: Mode]: Equal[Result[A, M]] =
      Equal.equal {
        case (Overwrite.Replaced(a1), Overwrite.Replaced(a2)) => Equal[A].equal(a1.asInstanceOf[A], a2.asInstanceOf[A]) // cast required for 2.10
        case (Overwrite.New, Overwrite.New) => true
        case (Insert.New, Insert.New) => true
        case (Insert.Failed, Insert.Failed) => true
        case (Replace.Wrote, Replace.Wrote) => true
        case (Replace.Failed, Replace.Failed) => true
        case (_, _) => false
      }
  }
}