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
                                     connectionTtl: Option[Long],
                                     useGzip: Option[Boolean],
                                     clientExecutionTimeout: Option[Int],
                                     maxIdleTimeoutMs: Option[Long],
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
      connectionTtl = this.connectionTtl.orElse(fallback.connectionTtl),
      useGzip = this.useGzip.orElse(fallback.useGzip),
      clientExecutionTimeout = this.clientExecutionTimeout.orElse(fallback.clientExecutionTimeout),
      maxIdleTimeoutMs = this.maxIdleTimeoutMs.orElse((fallback.maxIdleTimeoutMs)),
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
    AmazonClientConnectionDef(None, None, None, None, None, None, None, None, None, None, None, None, None)

  implicit object AmazonClientConnectionAccessor extends Accessor[AmazonClientConnectionDef] {
    def apply(c: Config, s: String) =
      Configuration(c).apply[Configuration](s) |> { config =>
        new AmazonClientConnectionDef(
          socketTimeoutMs = config.option[Int]("socket-timeout-ms"),
          connectionTimeoutMs = config.option[Int]("connection-timeout-ms"),
          maxErrorRetry = config.option[Int]("max-error-retry"),
          maxConnections = config.option[Int]("max-connections"),
          connectionTtl = config.option[Long]("connection-expiry-ttl"),
          useGzip = config.option[Boolean]("use-gzip"),
          clientExecutionTimeout = config.option[Int]("client-execution-timeout"),
          maxIdleTimeoutMs = config.option[Long]("max-idle-timeout-ms"),
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
         |Connection TTL" ${c.connectionTtl},
         |Use Gzip" ${c.useGzip},
         |Client execution timeout: ${c.clientExecutionTimeout},
         |Max idle timeout(ms): ${c.maxIdleTimeoutMs},
         |Proxy Host/port: ${c.proxyHost}:${c.proxyPort},
         |Region: ${c.region},
         |Endpoint URL: ${c.endpointUrl}""".stripMargin)

  implicit def AmazonClientConnectionDefEncodeJson: EncodeJson[AmazonClientConnectionDef] =
    jencode12L((a: AmazonClientConnectionDef) => (a.region, a.proxyHost, a.proxyPort, a.socketTimeoutMs, a.connectionTimeoutMs, a.maxErrorRetry, a.maxConnections, a.connectionTtl, a.useGzip, a.clientExecutionTimeout, a.maxIdleTimeoutMs, a.endpointUrl))(
      "region", "proxy-host", "proxy-port", "socket-timeout-ms", "connection-timeout-ms", "max-error-retry", "max-connections", "connection-expiry-ttl", "use-gzip", "client-execution-timeout", "max-idle-timeout-ms", "endpoint-url"
    )
}
