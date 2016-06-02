package io.atlassian.aws

import com.amazonaws.auth.{ AWSCredentialsProvider, DefaultAWSCredentialsProviderChain }
import com.amazonaws.metrics.RequestMetricCollector
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.rds.AmazonRDSClient
import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflowClient
import com.amazonaws.services.sqs.AmazonSQSClient
import com.amazonaws.{ AmazonWebServiceClient, ClientConfiguration }
import scalaz.syntax.id._
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.regions.ServiceAbbreviations
import com.amazonaws.services.cloudformation.AmazonCloudFormationClient

object AmazonClient extends AmazonClientOps {
  private[aws]type Constructor[A] = (AWSCredentialsProvider, ClientConfiguration, RequestMetricCollector) => A

  /**
   * Creates a client of the requested type with the given client configuration options (e.g. region, timeouts)
   * @param config the configuration options
   * @param fallback any gaps in the given config are filled in using the fallback configuration
   * @tparam A The type of the Amazon client (e.g. AmazonS3Client)
   * @return An instance of the client
   */
  def withClientConfiguration[A <: AmazonWebServiceClient: AmazonClient](config: AmazonClientConnectionDef,
                                                                         fallback: Option[AmazonClientConnectionDef] = None,
                                                                         metricsCollector: Option[RequestMetricCollector] = None): A =
    fromClientConfigurationDef[A] { fallback.fold(config)(config.withFallback) }(metricsCollector)

  /**
   * Creates a client of the requested type. Configuration options can be passed in as config parameter. Any gaps in the
   * configuration will be filled in from the fallback configuration. If no configuration is provided, the default client
   * will be created.
   * @param config the configuration options
   * @param fallback any gaps in the given config are filled in using the fallback configuration
   * @tparam A The type of the Amazon client (e.g. AmazonS3Client)
   * @return An instance of the client
   */
  def create[A <: AmazonWebServiceClient: AmazonClient](config: Option[AmazonClientConnectionDef] = None,
                                                        fallback: Option[AmazonClientConnectionDef] = None,
                                                        metricsCollector: Option[RequestMetricCollector] = None): A =
    withClientConfiguration(config.orElse(fallback).getOrElse(AmazonClientConnectionDef.default), fallback, metricsCollector)

  /**
   * Creates a client of the requested type with default configuration options
   * @tparam A The type of the Amazon client (e.g. AmazonS3Client)
   * @return An instance of the client
   */
  def default[A <: AmazonWebServiceClient: AmazonClient]: A =
    create[A]()

  /**
   * Creates a client of the requested type with a specific endpoint. Typically this is used to connect to a local endpoint
   * e.g. for a local DynamoDB.
   * @param endpoint Endpoint URL to use
   * @tparam A The type of the Amazon client (e.g. AmazonS3Client)
   * @return An instance of the client
   */
  def withEndpoint[A <: AmazonWebServiceClient: AmazonClient](endpoint: String): A =
    default[A] <| { _.setEndpoint(endpoint) }

  def apply[A <: AmazonWebServiceClient: AmazonClient] =
    implicitly[AmazonClient[A]]

  implicit object DynamoDBClient extends AmazonClient[AmazonDynamoDBClient](
    constructor = new AmazonDynamoDBClient(_, _, _),
    serviceName = ServiceAbbreviations.Dynamodb
  )

  implicit object S3Client extends AmazonClient[AmazonS3Client](
    constructor = new AmazonS3Client(_, _, _),
    serviceName = ServiceAbbreviations.S3
  )

  implicit object CFClient extends AmazonClient[AmazonCloudFormationClient](
    constructor = new AmazonCloudFormationClient(_, _, _),
    serviceName = ServiceAbbreviations.CloudFormation
  )

  implicit object SQSClient extends AmazonClient[AmazonSQSClient](
    constructor = new AmazonSQSClient(_, _, _),
    serviceName = ServiceAbbreviations.SQS
  )

  implicit object SWFClient extends AmazonClient[AmazonSimpleWorkflowClient](
    constructor = new AmazonSimpleWorkflowClient(_, _, _),
    serviceName = ServiceAbbreviations.SimpleWorkflow
  )

  implicit object RDSClient extends AmazonClient[AmazonRDSClient](
    constructor = new AmazonRDSClient(_, _, _),
    serviceName = ServiceAbbreviations.RDS
  )
}

class AmazonClient[A <: AmazonWebServiceClient](val constructor: AmazonClient.Constructor[A], serviceName: String)

trait AmazonClientOps {
  def fromClientConfigurationDef[A <: AmazonWebServiceClient: AmazonClient](config: AmazonClientConnectionDef)(metricsCollector: Option[RequestMetricCollector]): A =
    AmazonClient[A].constructor(
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
}
