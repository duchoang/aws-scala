package io.atlassian.aws.dynamodb

import scalaz.syntax.traverse._
import scalaz.std.list._

trait QueryOps {
  import collection.JavaConverters._

  def query[A](q: QueryImpl)(col: Column[A]): DynamoDBAction[Page[QueryImpl, A]] =
    DynamoDBAction.withClient {
      _.query(q.asQueryRequest)
    } flatMap { res =>
      res.getItems.asScala.toList.traverse[DynamoDBAction, A] {
        col.unmarshaller.unmarshall
      }.map {
        Page(_,
          Option(res.getLastEvaluatedKey).map { lastKey =>
            QueryImpl.nextFromQuery(q, lastKey.asScala.toMap)
          }
        )
      }
    }
}
