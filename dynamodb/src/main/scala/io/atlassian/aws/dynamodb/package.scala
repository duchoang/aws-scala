package io.atlassian.aws

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.model.{ AttributeValue, UpdateItemRequest }
import scalaz.ReaderT

package object dynamodb extends QueryTypes {
  type DynamoDBAction[A] = AwsAction[AmazonDynamoDBClient, A]

  object DynamoDBAction extends AwsAction.Functions[AmazonDynamoDBClient] {
    override type Action[A] = DynamoDBAction[A]
  }

  type Value = Option[AttributeValue]

  type Field[A] = (String, Value)

  type KeyValue = Map[String, Value]

  type DynamoMap = Map[String, AttributeValue]

  type Unmarshaller[A] = ReaderT[Attempt, DynamoMap, A]

  private[dynamodb] val EMPTY_STRING_PLACEHOLDER = 0.toChar.toString
}
