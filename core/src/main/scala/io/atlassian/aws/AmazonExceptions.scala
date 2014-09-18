package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import com.amazonaws.services.dynamodbv2.model.ConditionalCheckFailedException
import kadai.Invalid

object AmazonExceptions {
  sealed trait ExceptionType
  object ExceptionType {
    case object NotFound extends ExceptionType
    case object Unauthenticated extends ExceptionType
    case object Forbidden extends ExceptionType
    case object AmazonServerIssue extends ExceptionType
    case object ConditionalCheckFailed extends ExceptionType

    def unapply(e: AmazonServiceException): ExceptionType =
      e match {
        case c: ConditionalCheckFailedException => ConditionalCheckFailed
        case _ =>
          e.getStatusCode match {
            case 404 => NotFound
            case 401 => Unauthenticated
            case 403 => Forbidden
            case _ => AmazonServerIssue
          }
      }
  }

  case class ServiceException(exceptionType: ExceptionType, e: AmazonServiceException) extends Exception

  object ServiceException {
    def apply(e: AmazonServiceException): ServiceException =
      ServiceException(ExceptionType.unapply(e), e)
  }

  private [aws] def transformException(i: Invalid): Invalid =
    i match {
      case Invalid.Err(e: AmazonServiceException) => Invalid.Err(AmazonExceptions.ServiceException(e))
      case _ => i
    }
}
