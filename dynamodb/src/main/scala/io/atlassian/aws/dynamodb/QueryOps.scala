package io.atlassian.aws.dynamodb

import scalaz.syntax.traverse._
import scalaz.std.list._

trait QueryOps {
  import collection.JavaConverters._

  def query[A](query: Query[A])(col: Column[A]): DynamoDBAction[Page[A]] =
    DynamoDBAction.withClient { client =>
      client.query(query.asQueryRequest)
    } flatMap { result =>
      result.getItems.asScala.toList.traverse[DynamoDBAction, A] { m => col.unmarshaller.unmarshall(m) } map { values =>
        val lastKey = Option(result.getLastEvaluatedKey)
        val next = lastKey.map { lk =>
          Query.nextFromQuery(query, lk.asScala.toMap)
        }
        Page(values, next)
      }
    }
}
