package io.atlassian.aws

import argonaut.EncodeJson, argonaut.Json._
import com.amazonaws.regions.{ RegionUtils, Region }

object AmazonRegion {
  lazy val default = RegionUtils.getRegion("us-east-1")

  def unapply(regionName: String): Option[Region] =
    Option(RegionUtils.getRegion(regionName))

  def orDefault(regionName: String): Region =
    unapply(regionName).getOrElse(default)

  implicit val RegionEncodeJson: EncodeJson[Region] =
    EncodeJson { r => jString(r.getName) }
}
