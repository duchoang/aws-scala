package io.atlassian.aws

import com.amazonaws.auth.{AWSCredentialsProvider, AWSStaticCredentialsProvider, BasicAWSCredentials, DefaultAWSCredentialsProviderChain}
import com.typesafe.config.ConfigException.BadValue
import com.typesafe.config.Config
import kadai.config.{ConfigReader, Configuration}
import Configuration._

import scalaz.syntax.id._

/**
 * Wraps provision of credentials for AWS clients. Use constructors in `Credential` to create a credential.
 * There is also a kadai ConfigReader to configure credentials as either:
 *   * static - configure static access key and secret key like:
 *   {{{
 *     credential {
 *       mode = static   // Use static credential mode
 *       static {
 *         access-key = foo
 *         secret-key = bar
 *       }
 *     }
 *   }}}
 *   * default - i.e. use default credential chain in AWS clients (load from env vars, sys props etc.). Config
 *     file needs to include a 'mode' of 'default'
 */
case class Credential private (run: AWSCredentialsProvider)

object Credential {
  def static(accessKey: String, secretKey: String): Credential =
    Credential { new AWSStaticCredentialsProvider(new BasicAWSCredentials(accessKey, secretKey)) }

  def default: Credential =
    Credential { defaultCredentialsProvider }

  private lazy val defaultCredentialsProvider = new DefaultAWSCredentialsProviderChain

  implicit object CredentialConfigAccessor extends Accessor[Credential] {
    def apply(c: Config, s: String) =
      Configuration(c).apply[Configuration](s) |> { config =>
        config.get[String]("mode") match {
          case "static" =>
            val accessKey = config.get[String]("static.access-key")
            val secretKey = config.get[String]("static.secret-key")
            static(accessKey, secretKey)
          case "default" =>
            default
          case u => throw new BadValue(s, s"Unknown credential mode $u, expect 'static' or 'default'")
        }
      }
  }

  val configReader: ConfigReader[Option[Credential]] =
    ConfigReader.option[Credential]("credential")
}
