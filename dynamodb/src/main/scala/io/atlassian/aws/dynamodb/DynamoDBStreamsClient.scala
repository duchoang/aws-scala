package io.atlassian.aws.dynamodb

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBStreamsClient
import io.atlassian.aws.AmazonClientBase

object DynamoDBStreamsClient extends AmazonClientBase[AmazonDynamoDBStreamsClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonDynamoDBStreamsClient(a, b, c)
}
