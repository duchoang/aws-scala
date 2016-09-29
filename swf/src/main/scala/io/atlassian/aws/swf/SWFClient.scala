package io.atlassian.aws.swf

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflowClient
import io.atlassian.aws.AmazonClientBase

object SWFClient extends AmazonClientBase[AmazonSimpleWorkflowClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonSimpleWorkflowClient(a, b, c)
}
