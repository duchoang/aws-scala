package io.atlassian.aws

import kadai.config.Configuration
import kadai.config.Configuration.Accessor
import com.amazonaws.regions.Region
import com.typesafe.config.{ ConfigException, Config }
import scalaz.syntax.id._

object AmazonRegionDef {

  /**
   * Extracts Amazon Region from a specified region name.
   */
  implicit object AmazonRegionAccessor extends Accessor[Region] {
    def apply(c: Config, s: String) =
      Configuration(c).apply[String](s) |> {
        case AmazonRegion(region) => region
        case unknown              => throw new ConfigException.BadValue(s, s"Unknown region name $unknown.")
      }
  }
}
