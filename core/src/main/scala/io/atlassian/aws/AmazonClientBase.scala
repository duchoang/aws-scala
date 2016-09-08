package io.atlassian.aws

import scalaz.syntax.id._
import com.amazonaws.{AmazonWebServiceClient, ClientConfiguration}
import com.amazonaws.auth.AWSCredentialsProvider
import com.amazonaws.metrics.RequestMetricCollector

/**
  * Extend this when adding support for a new service.
  * @tparam A The AWS client type
  */
abstract class AmazonClientBase[A <: AmazonWebServiceClient] {
  def constructor(a: AWSCredentialsProvider, b: ClientConfiguration, c: RequestMetricCollector): A
  /**
    * Creates a client of the requested type with the given client configuration options (e.g. region, timeouts)
    *
    * @param config the configuration options
    * @param fallback any gaps in the given config are filled in using the fallback configuration
    * @return An instance of the client
    */
  def withClientConfiguration(config: AmazonClientConnectionDef,
                              fallback: Option[AmazonClientConnectionDef] = None,
                              metricsCollector: Option[RequestMetricCollector] = None): A =
    constructor(
      config.credential.getOrElse(Credential.default).run,
      new ClientConfiguration() <| { c =>
        config.connectionTimeoutMs.foreach { c.setConnectionTimeout }
        config.maxConnections.foreach { c.setMaxConnections }
        config.maxErrorRetry.foreach { c.setMaxErrorRetry }
        config.socketTimeoutMs.foreach { c.setSocketTimeout }
        config.proxyHost.foreach { c.setProxyHost }
        config.proxyPort.foreach { c.setProxyPort }
      },
      metricsCollector.orNull) <| { a => config.region.foreach { a.setRegion } } <| { a => config.endpointUrl.foreach { a.setEndpoint } }

  /**
    * Creates a client of the requested type. Configuration options can be passed in as config parameter. Any gaps in the
    * configuration will be filled in from the fallback configuration. If no configuration is provided, the default client
    * will be created.
    *
    * @param config the configuration options
    * @param fallback any gaps in the given config are filled in using the fallback configuration
    * @return An instance of the client
    */
  def create(config: Option[AmazonClientConnectionDef] = None,
             fallback: Option[AmazonClientConnectionDef] = None,
             metricsCollector: Option[RequestMetricCollector] = None): A =
    withClientConfiguration(config.orElse(fallback).getOrElse(AmazonClientConnectionDef.default), fallback, metricsCollector)

  /**
    * Creates a client of the requested type with default configuration options
    *
    * @return An instance of the client
    */
  def default: A =
    create()

  /**
    * Creates a client of the requested type with a specific endpoint. Typically this is used to connect to a local endpoint
    * e.g. for a local DynamoDB.
    *
    * @param endpoint Endpoint URL to use
    * @return An instance of the client
    */
  def withEndpoint(endpoint: String): A =
    default <| { _.setEndpoint(endpoint) }
}