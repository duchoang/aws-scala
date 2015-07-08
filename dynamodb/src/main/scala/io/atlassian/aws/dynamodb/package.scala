package io.atlassian.aws

import com.amazonaws.services.dynamodbv2.AmazonDynamoDB
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import scalaz.ReaderT

package object dynamodb extends QueryTypes with DynamoStringType {
  type DynamoDBAction[A] = AwsAction[AmazonDynamoDB, MetaData, A]

  object DynamoDBAction extends AwsAction.Functions[AmazonDynamoDB, MetaData] {
    override type Action[A] = DynamoDBAction[A]

    override implicit def WMonoid = MetaDataMonoid

    override def extractRequestIds =
      Some {
        headers => headers.headers.get("x-amzn-RequestId").map(s => MetaData(List(s)))
      }

    override def extractRequestIdsFromException =
      Some {
        ase => Some(MetaData(List(ase.getRequestId)))
      }
  }

  type Value = Option[AttributeValue]

  type Field[A] = (String, Value)

  type KeyValue = Map[String, Value]

  type DynamoMap = Map[String, AttributeValue]

  type Unmarshaller[A] = ReaderT[Attempt, DynamoMap, A]
}
