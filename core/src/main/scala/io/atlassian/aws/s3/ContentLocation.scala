package io.atlassian.aws.s3

import argonaut._, Argonaut._

case class ContentLocation(bucket: Bucket, key: S3Key)

object ContentLocation {
  implicit val ContentLocationEncodeJson: EncodeJson[ContentLocation] =
    jencode2L { (c: ContentLocation) => (c.bucket, c.key) }("bucket", "key")
}