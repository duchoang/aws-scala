package io.atlassian.aws.s3

import scalaz.syntax.std.option._
import scala.collection.generic.IsTraversableLike

/**
 * Represents an HTTP Range:
 *
 * http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
 *
 * Note that the HTTP spec is end inclusive, unlike the rest of the world.
 * This means Range(0, 0) actually selects one byte.
 */
sealed trait Range {
  import Range._

  def get: Option[(Long, Long)] =
    this match {
      case All            => None
      case From(s)        => (s, Long.MaxValue).some
      case To(e)          => (0L, e).some
      case Interval(s, e) => (s, e).some
    }

  /** slice a Collection, do the off-by-one madness */
  def slice[A, CC[_]](coll: CC[A])(implicit fr: IsTraversableLike[CC[A]]): CC[A] =
    this match {
      case All            => coll
      case From(s)        => (fr conversion coll).drop(s.toInt)
      case To(e)          => (fr conversion coll).take(e.toInt + 1)
      case Interval(s, e) => (fr conversion coll).slice(s.toInt, e.toInt + 1)
    }
}

object Range {
  case object All extends Range
  case class From(start: Long) extends Range
  case class To(end: Long) extends Range
  case class Interval(start: Long, end: Long) extends Range
}
