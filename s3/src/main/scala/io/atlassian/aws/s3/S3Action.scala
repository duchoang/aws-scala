package io.atlassian.aws
package s3

import com.amazonaws.services.s3.{ AmazonS3Client => SDKS3Client }
import kadai.Attempt

object S3Action extends AwsAction.Functions[SDKS3Client] {
  override type Action[A] = S3Action[A]
}
