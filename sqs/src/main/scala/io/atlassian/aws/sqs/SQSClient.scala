package io.atlassian.aws.sqs

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.sqs.AmazonSQSClient
import io.atlassian.aws.AmazonClientBase

object SQSClient extends AmazonClientBase[AmazonSQSClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonSQSClient(a, b, c)
}
