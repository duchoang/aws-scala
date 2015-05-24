package io.atlassian.aws

import com.amazonaws.auth.{ AWSCredentialsProvider, BasicAWSCredentials }
import com.amazonaws.internal.StaticCredentialsProvider

/**
 * Wraps provision of credentials for AWS clients. Use constructors in `Credential` to create a credential
 */
case class Credential private (run: AWSCredentialsProvider)

object Credential {
  def static(accessKey: String, secretKey: String): Credential =
    Credential { new StaticCredentialsProvider(new BasicAWSCredentials(accessKey, secretKey)) }
}