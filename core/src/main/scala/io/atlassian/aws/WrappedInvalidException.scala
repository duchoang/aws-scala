package io.atlassian.aws

import kadai.Invalid
import scalaz.syntax.all._

object WrappedInvalidException {
  def orUnderlying(invalid: Invalid): Throwable =
    invalid match {
      case Invalid.Err(t) => t
      case _              => WrappedInvalidException(invalid)
    }
}

case class WrappedInvalidException(invalid: Invalid) extends RuntimeException {
  override def toString: String = s"WrappedInvalidException(${invalid.shows})"
}
