package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient

object DynamoDBAction {
  def value[O](v: => O): DynamoDBAction[O] =
    attempt(Attempt.ok(v))

  def config: DynamoDBAction[AmazonDynamoDBClient] =
    DynamoDBAction { c => Attempt.ok(c) }

  def ok[O](strict: O): DynamoDBAction[O] =
    value(strict)

  def attempt[O](attempt: Attempt[O]): DynamoDBAction[O] =
    DynamoDBAction { _ => attempt }

  def withClient[O](f: AmazonDynamoDBClient => O): DynamoDBAction[O] =
    AwsAction.withClient(f)

  def apply[A](run: AmazonDynamoDBClient => Attempt[A]): DynamoDBAction[A] =
    AwsAction.safe(run)

  def fail[A](msg: String): DynamoDBAction[A] =
    attempt(Attempt.fail(msg))
}
