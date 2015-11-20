package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient

import scalaz.concurrent.Task
import scalaz.~>

trait DynamoTask {
  type DynamoTask = DynamoDBAction ~> Task

  object DynamoTask {
    def apply(c: AmazonDynamoDBClient): DynamoTask =
      new DynamoTask {
        def apply[A](a: DynamoDBAction[A]): Task[A] =
          new Task(a.run(c).leftMap(WrappedInvalidException.orUnderlying).run.value)
      }
  }
}
