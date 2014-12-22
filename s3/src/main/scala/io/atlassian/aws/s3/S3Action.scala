package io.atlassian.aws
package s3

import com.amazonaws.services.s3.AmazonS3

object S3Action extends AwsAction.Functions[AmazonS3] {
  override type Action[A] = S3Action[A]
}
