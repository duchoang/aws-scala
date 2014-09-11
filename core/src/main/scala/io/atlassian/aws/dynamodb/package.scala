package io.atlassian.aws

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.model.{ AttributeValue, UpdateItemRequest }
import scalaz.std.option.{ some, none }
import scalaz.syntax.id._

package object dynamodb extends QueryTypes {
  type UpdateItemRequestEndo = scalaz.Endo[UpdateItemRequest]

  type DynamoDBAction[A] = AwsAction[AmazonDynamoDBClient, A]

  type Value = Option[AttributeValue]

  type Field[A] = (String, Value)

  type KeyValue = Map[String, Value]

  type ToKeyValue[A] = A => KeyValue

  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  private[dynamodb] def unmarshall[B](map: java.util.Map[String, AttributeValue])(implicit evValue: Unmarshaller[B]): DynamoDBAction[B] = {
    import collection.JavaConverters._
    if (map == null) DynamoDBAction.fail("No values to unmarshall")
    else map.asScala.toMap |> evValue.fromMap |> DynamoDBAction.attempt
  }

  /** Unmarshalls a given map of attribute values from AWS SDK into a value object */
  private[dynamodb] def unmarshallOpt[B](map: java.util.Map[String, AttributeValue])(implicit evValue: Unmarshaller[B]): DynamoDBAction[Option[B]] = {
    import collection.JavaConverters._
    if (map == null) DynamoDBAction.ok(none[B])
    else map.asScala.toMap |> evValue.fromMap |> { _.map(some) } |> DynamoDBAction.attempt
  }
}
