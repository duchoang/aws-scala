package io.atlassian.aws.swf

import java.util.concurrent.TimeUnit

import argonaut.Argonaut._
import argonaut.{ DecodeJson, EncodeJson, CodecJson }

import scala.concurrent.duration.FiniteDuration

object JsonCodecs {
  implicit val ActivityCodecJson: CodecJson[Activity] =
    casecodec2(Activity.apply, Activity.unapply)("name", "version")

  implicit val FiniteDurationEncodeJson: EncodeJson[FiniteDuration] =
    implicitly[EncodeJson[Long]].contramap { _.toMillis }

  implicit val FiniteDurationDecodeJson: DecodeJson[FiniteDuration] =
    implicitly[DecodeJson[Long]].map { l => FiniteDuration(l, TimeUnit.MILLISECONDS) }

}
