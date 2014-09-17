package io.atlassian.aws.s3

import argonaut._, Argonaut._

case class ContentLocation(bucket: Bucket, key: S3Key)

object ContentLocation {
  implicit val ContentLocationEncodeJson: EncodeJson[ContentLocation] =
    jencode2L { (c: ContentLocation) => (c.bucket, c.key) }("bucket", "key")

  implicit val ContentLocationDecodeJson: DecodeJson[ContentLocation] =
    DecodeJson { c =>
      for {
        bucket <- (c --\ "bucket").as[String]
        key    <- (c --\ "key").as[String]
      } yield ContentLocation(Bucket(bucket), S3Key(key))
    }
}