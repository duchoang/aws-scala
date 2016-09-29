package io.atlassian.aws.rds

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.rds.AmazonRDSClient
import io.atlassian.aws.AmazonClientBase

object RDSClient extends AmazonClientBase[AmazonRDSClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonRDSClient(a, b, c)
}
