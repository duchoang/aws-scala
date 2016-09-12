package io.atlassian.aws.s3

import com.amazonaws.ClientConfiguration
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.s3.AmazonS3Client
import io.atlassian.aws.AmazonClientBase

object S3Client extends AmazonClientBase[AmazonS3Client] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector) =
    new AmazonS3Client(a, b, c)
}
