package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.ComparisonOperator

trait QueryTypes {

  sealed trait Comparison
  object Comparison {
    case object Eq extends Comparison
    case object Lte extends Comparison
    case object Lt extends Comparison
    case object Gte extends Comparison
    case object Gt extends Comparison

    private[dynamodb] val asAWS: Comparison => ComparisonOperator = {
      case Eq  => ComparisonOperator.EQ
      case Lte => ComparisonOperator.LE
      case Lt  => ComparisonOperator.LT
      case Gte => ComparisonOperator.GE
      case Gt  => ComparisonOperator.GT
    }
  }

  case class Page[A](result: List[A], next: Option[Query[A]])

  sealed trait ScanDirection
  object ScanDirection {
    case object Ascending extends ScanDirection
    case object Descending extends ScanDirection

    private[dynamodb] val asBool: ScanDirection => Boolean = {
      case Ascending  => true
      case Descending => false
    }
  }

}
