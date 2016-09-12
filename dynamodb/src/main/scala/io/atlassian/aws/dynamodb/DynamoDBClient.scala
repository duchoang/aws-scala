package io.atlassian.aws.dynamodb

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import io.atlassian.aws.AmazonClientBase

object DynamoDBClient extends AmazonClientBase[AmazonDynamoDBClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonDynamoDBClient(a, b, c)
}
