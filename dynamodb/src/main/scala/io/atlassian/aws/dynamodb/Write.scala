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
      case object Wrote extends Result[Nothing, Insert.type]
      case object Rejected extends Result[Nothing, Insert.type]

      // if Insert fails we get exception
      private[dynamodb] def result[V]: Option[V] => Result[V, Insert.type] = {
        case None    => Wrote
        case Some(v) => throw new AssertionError(s"we should never get back old data if we are Inserting only, got $v")
      }
      private[dynamodb] def fail[V] = Rejected
    }

    /**
     * Only put a value if there is a specific value currently.
     *
     * Update semantics
     */
    // this being a case class and not a case object means some funky casts, but they're safe
    case class Replace[A](a: A) extends Mode {
      private[dynamodb] def result[V]: Option[V] => Result[V, this.Mode] = {
        case Some(v) => Replace.Wrote().asInstanceOf[Result[V, this.Mode]]
        case None    => throw new AssertionError("we should always get back the old data if we are Replacing")
      }
      private[dynamodb] def fail[V] = Replace.Rejected[V]().asInstanceOf[Result[V, this.Mode]]
    }
    object Replace {
      /** return the written value */
      case class Wrote[A]() extends Result[Nothing, Replace[A]]
      case class Rejected[A]() extends Result[Nothing, Replace[A]]
    }
  }

  object Result {
    import Mode._
    implicit def ResultEqual[A: Equal, M <: Mode]: Equal[Result[A, M]] =
      Equal.equal {
        case (Overwrite.Replaced(a1), Overwrite.Replaced(a2)) => Equal[A].equal(a1, a2)
        case (Overwrite.New, Overwrite.New) => true
        case (Insert.Wrote, Insert.Wrote) => true
        case (Insert.Rejected, Insert.Rejected) => true
        case (Replace.Wrote(), Replace.Wrote()) => true
        case (Replace.Rejected(), Replace.Rejected()) => true
        case _ => false
      }
  }
}