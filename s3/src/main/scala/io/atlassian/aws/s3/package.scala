package io.atlassian.aws

import com.amazonaws.services.s3.AmazonS3

package object s3 extends s3.Types {
  type S3Action[A] = AwsAction[AmazonS3, A]
}
