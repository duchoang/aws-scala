package io.atlassian.aws
package dynamodb

import java.net.ServerSocket

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import io.atlassian.aws.spec.MoreEqualsInstances
import org.specs2.execute.{ StandardResults, Failure }
import org.specs2.main.{ CommandLine, Arguments }
import org.specs2.specification.core.{ Env, SpecificationStructure }

import scala.sys.process.{ ProcessLogger, stringSeqToProcess }

import scalaz.syntax.id._

/**
 * Mix in this trait if you want to write a spec that depends on DynamoDB and want to optionally spin up a local
 * DynamoDB.
 *
 * To use it, extend this trait, specify (val arguments: org.specs2.main.Arguments) as an argument to your spec class,
 * and add steps to your spec to call startLocalDynamoDB and stopLocalDynamoDB at the appropriate times.
 *
 * You can also optionally override various command line argument names to configure the spec.
 */
trait LocalDynamoDB { self: SpecificationStructure =>
  /**
   * Override this to provide a custom command line argument name that represents 'use AWS resources' mode
   */
  def integration = "aws-integration"

  /**
   * Override this to provide a custom command line argument name for local Dynamo DB port
   */
  def db_port = "db-port"

  /**
   * Override this to provde a custom default DB port programmatically. By default it will try to pick a random port.
   */
  def defaultDbPort = randomPort

  lazy val randomPort = {
    val ss = new ServerSocket(0)
    val port = ss.getLocalPort
    ss.close()
    port
  }

  override def structure = (env: Env) => {
    commandLine = env.arguments.commandLine
    decorate(is, env)
  }

  /**
   * Override this to provide a custom command line argument name for Dynamo DB region
   */
  def region = "region"

  @volatile var commandLine: CommandLine = CommandLine.create()

  /**
   * Override this to specify custom location for start/stop scripts for a local Dynamo instance
   */
  def scriptDirectory = "scripts"

  def IS_LOCAL = !commandLine.contains("aws-integration")
  def REGION = commandLine.value("region").getOrElse(Option(System.getenv("AWS_REGION")).getOrElse("ap-southeast-2"))
  def LOCAL_DB_PORT = commandLine.int("db-port").getOrElse(defaultDbPort)

  def startLocalDynamoDB =
    withLocalDb {
      runAttemptStep(for {
        _ <- runCmd(s"$scriptDirectory/install_dynamodb_local.sh", "Install Local DynamoDB")
        _ <- runCmd(s"$scriptDirectory/run_dynamodb_local.sh ${LOCAL_DB_PORT.toString}", "Start Local DynamoDB")
      } yield ())
    }

  def stopLocalDynamoDB() =
    withLocalDb {
      runAttemptStep {
        runCmd(s"$scriptDirectory/stop_dynamodb_local.sh ${LOCAL_DB_PORT.toString}", "Stop local Dynamo DB")
      }
    }

  def runCmd(command: String, fail: String, success: String = ""): Attempt[String] = {
    val commandList = List("sh", "-x", "-c", s"$command > /dev/null 2> /dev/null")
    if (commandList ! ProcessLogger(System.out.println, System.err.println) != 0)
      Attempt.fail(fail)
    else
      Attempt.ok(success)
  }

  def runAttemptStep[A](attempt: Attempt[A]) =
    attempt.run.fold(i => Failure(i.toString), _ => StandardResults.success)

  def withLocalDb(f: => org.specs2.execute.Result) =
    if (IS_LOCAL)
      f
    else
      StandardResults.success

  def dynamoClient =
    if (IS_LOCAL) {
      // Create a client with dummy credentials pointing to the local DB.
      val dynamoClient = new AmazonDynamoDBClient(new BasicAWSCredentials("FOO", "BAR"))
      dynamoClient.setEndpoint(s"http://localhost:$LOCAL_DB_PORT")
      dynamoClient
    } else {
      AmazonClient.default[AmazonDynamoDBClient] <| { _.setRegion(AmazonRegion.orDefault(REGION)) }
    }
}
