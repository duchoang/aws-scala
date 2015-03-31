package io.atlassian.aws

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.model.{ AttributeValue, UpdateItemRequest }
import scalaz.ReaderT
import scalaz.std.option.{ some, none }

package object dynamodb extends QueryTypes {
  type UpdateItemRequestEndo = scalaz.Endo[UpdateItemRequest]

  type DynamoDBAction[A] = AwsAction[AmazonDynamoDBClient, A]

  object DynamoDBAction extends AwsAction.Functions[AmazonDynamoDBClient] {
    override type Action[A] = DynamoDBAction[A]
  }

  type Value = Option[AttributeValue]

  type Field[A] = (String, Value)

  type KeyValue = Map[String, Value]

  type DynamoMap = Map[String, AttributeValue]

  private[dynamodb] val EMPTY_STRING_PLACEHOLDER = 0.toChar.toString // "\"" + 128169.toChar + "\""

  type Unmarshaller[A] = ReaderT[Attempt, DynamoMap, A]
}
