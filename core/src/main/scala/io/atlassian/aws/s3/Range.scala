package io.atlassian.aws.s3

import scalaz.syntax.std.option._

sealed trait Range {
  import Range._

  def get: Option[(Long, Long)] =
    this match {
      case All            => None
      case From(s)        => (s, Long.MaxValue).some
      case To(e)          => (0L, e).some
      case Interval(s, e) => (s, e).some
    }
}

object Range {
  case object All extends Range
  case class From(start: Long) extends Range
  case class To(end: Long) extends Range
  case class Interval(start: Long, end: Long) extends Range
}
