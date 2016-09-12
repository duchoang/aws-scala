package io.atlassian.aws.sns

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.sns.AmazonSNSClient
import io.atlassian.aws.AmazonClientBase

object SNSClient extends AmazonClientBase[AmazonSNSClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonSNSClient(a, b, c)
}