package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient

object DynamoDBAction extends AwsAction.Functions[AmazonDynamoDBClient] {
  override type Action[A] = DynamoDBAction[A]
}