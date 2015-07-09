package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import kadai.Invalid
import scalaz.syntax.id._
import scalaz.syntax.std.option._
import scalaz.std.option._

object AmazonExceptions {
  sealed trait ExceptionType
  object ExceptionType {
    case object NotFound extends ExceptionType
    case object Unauthenticated extends ExceptionType
    case object Forbidden extends ExceptionType
    case object AmazonServerIssue extends ExceptionType
    case object RangeRequestedNotSatisfiable extends ExceptionType

    def unapply(e: AmazonServiceException): Option[ExceptionType] =
      e.getStatusCode match {
        case 404 => NotFound.some
        case 401 => Unauthenticated.some
        case 403 => Forbidden.some
        case 416 => RangeRequestedNotSatisfiable.some
        case _   => None
      }
  }

  case class ServiceException(exceptionType: ExceptionType, e: AmazonServiceException) extends Exception(e)

  object ServiceException {
    def from: AmazonServiceException => Option[ServiceException] =
      e => ExceptionType.unapply(e).map { t => ServiceException(t, e) }
  }

  private[aws] def transformException: Invalid => Invalid = {
    case Invalid.Err(e: AmazonServiceException) =>
      AmazonExceptions.ServiceException.from(e).getOrElse(e) |> Invalid.Err
    case i => i
  }
}
