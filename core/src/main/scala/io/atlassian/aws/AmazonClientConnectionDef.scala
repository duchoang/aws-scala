package io.atlassian.aws

import argonaut._, Argonaut._
import kadai.config.{ ConfigReader, Configuration }
import kadai.config.Configuration._
import com.amazonaws.regions.Region
import com.typesafe.config.Config
import scalaz.syntax.id._
import scalaz.Show

case class AmazonClientConnectionDef(socketTimeoutMs: Option[Int],
                                     connectionTimeoutMs: Option[Int],
                                     maxErrorRetry: Option[Int],
                                     maxConnections: Option[Int],
                                     proxyHost: Option[String],
                                     proxyPort: Option[Int],
                                     region: Option[Region],
                                     endpointUrl: Option[String],
                                     credential: Option[Credential]) {

  /**
   * Create a Amazon client configuration definition using settings from this definition, and filling in any gaps using
   * the given fallback definition.
   * @param fallback The fallback definition.
   * @return A new Amazon client configuration definition that using settings from this definition, and filling in any
   *         gaps using the given fallback definition.
   */
  def withFallback(fallback: AmazonClientConnectionDef): AmazonClientConnectionDef = {
    AmazonClientConnectionDef(
      socketTimeoutMs = this.socketTimeoutMs.orElse(fallback.socketTimeoutMs),
      connectionTimeoutMs = this.connectionTimeoutMs.orElse(fallback.connectionTimeoutMs),
      maxErrorRetry = this.maxErrorRetry.orElse(fallback.maxErrorRetry),
      maxConnections = this.maxConnections.orElse(fallback.maxConnections),
      proxyHost = this.proxyHost.orElse(fallback.proxyHost),
      proxyPort = this.proxyPort.orElse(fallback.proxyPort),
      region = this.region.orElse(fallback.region),
      endpointUrl = this.endpointUrl.orElse(fallback.endpointUrl),
      credential = this.credential.orElse(fallback.credential)
    )
  }
}

/**
 * Extracts configuration suitable for Amazon ClientConfiguration from a kadai Config.
 *
 * Just import this, and then you can just do the following to get it from a kadai Config:
 * {{{
 *   val c = config[AmazonClientConnectionDef]("config-section")
 * }}}
 */
object AmazonClientConnectionDef {
  import AmazonRegionDef._
  import AmazonRegion._

  val default: AmazonClientConnectionDef =
    AmazonClientConnectionDef(None, None, None, None, None, None, None, None, None)

  implicit object AmazonClientConnectionAccessor extends Accessor[AmazonClientConnectionDef] {
    def apply(c: Config, s: String) =
      Configuration(c).apply[Configuration](s) |> { config =>
        new AmazonClientConnectionDef(
          socketTimeoutMs = config.option[Int]("socket-timeout-ms"),
          connectionTimeoutMs = config.option[Int]("connection-timeout-ms"),
          maxErrorRetry = config.option[Int]("max-error-retry"),
          maxConnections = config.option[Int]("max-connections"),
          proxyHost = config.option[String]("proxy-host"),
          proxyPort = config.option[Int]("proxy-port"),
          region = config.option[Region]("region"),
          endpointUrl = config.option[String]("endpoint-url"),
          credential = config.option[Credential]("credential")
        )
      }
  }

  val configReader: ConfigReader[Option[AmazonClientConnectionDef]] =
    ConfigReader.option[AmazonClientConnectionDef]("aws-client")

  implicit def AmazonClientConnectionDefShow: Show[AmazonClientConnectionDef] =
    Show.shows(c =>
      s"""Amazon Client Configuration:
         |Socket timeout(ms): ${c.socketTimeoutMs},
         |Connection timeout(ms): ${c.connectionTimeoutMs},
         |Max Error Retry: ${c.maxErrorRetry},
         |Max Connections: ${c.maxConnections},
         |Proxy Host/port: ${c.proxyHost}:${c.proxyPort},
         |Region: ${c.region},
         |Endpoint URL: ${c.endpointUrl}""".stripMargin)

  implicit def AmazonClientConnectionDefEncodeJson: EncodeJson[AmazonClientConnectionDef] =
    jencode8L((a: AmazonClientConnectionDef) => (a.region, a.proxyHost, a.proxyPort, a.socketTimeoutMs, a.connectionTimeoutMs, a.maxErrorRetry, a.maxConnections, a.endpointUrl))(
      "region", "proxy-host", "proxy-port", "socket-timeout-ms", "connection-timeout-ms", "max-error-retry", "max-connections", "endpoint-url"
    )
}
