package io.atlassian.aws
package dynamodb

import java.io.File
import java.net.ServerSocket
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import io.atlassian.aws.spec.MoreEqualsInstances
import org.specs2.execute.{ StandardResults, Failure }
import org.specs2.main.Arguments

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
trait LocalDynamoDB {
  /**
   * Override this to provide a custom command line argument name that represents 'use AWS resources' mode
   */
  def integration = "aws-integration"

  /**
   * Override this to provide a custom command line argument name for local Dynamo DB port
   */
  def db_port = "db-port"

  /**
   * Override this to provide a custom default DB port programmatically. By default it will try to pick a random port.
   */
  def defaultDbPort = randomPort

  /**
   * Override this to specify whether to use AWS Local DynamoDB or Dynalite
   * @return
   */
  def useAwsLocalDynamo = true

  private def runDynamoTypeOption = if (useAwsLocalDynamo) "" else "-d"

  lazy val randomPort = {
    val ss = new ServerSocket(0)
    val port = ss.getLocalPort
    ss.close()
    port
  }

  /**
   * Override this to provide a custom command line argument name for Dynamo DB region
   */
  def region = "region"

  def arguments: Arguments

  def IS_LOCAL = !arguments.commandLine.contains("aws-integration")
  def REGION = arguments.commandLine.value("region").getOrElse(Option(System.getenv("AWS_REGION")).getOrElse("ap-southeast-2"))
  def LOCAL_DB_PORT = arguments.commandLine.int("db-port").getOrElse(defaultDbPort)

  private[this] def targetDirectory =
    new File("target")

  private[this] def runCmd(command: String, fail: String, success: String = ""): Attempt[String] = {
    val commandList = List("sh", "-x", "-c", s"$command > /dev/null 2> /dev/null")
    if (commandList ! ProcessLogger(System.out.println, System.err.println) != 0)
      Attempt.fail(fail)
    else
      Attempt.ok(success)
  }

  /**
   * Override this to specify custom location for start/stop scripts for a local Dynamo instance
   */
  def runScript(script: String, args: List[String], name: String) = {
    val target = new File(targetDirectory, script)

    targetDirectory.mkdirs()
    if (target.createNewFile()) {
      val stream = classOf[LocalDynamoDB].getClassLoader.getResourceAsStream("scripts/" + script)
      Files.copy(stream, target.toPath, REPLACE_EXISTING)
      target.setExecutable(true)
    }

    runCmd(target.getCanonicalPath + " " + args.mkString(" "), name)
  }

  def startLocalDynamoDB() =
    withLocalDb {
      runAttemptStep(for {
        _ <- runScript("install_dynamodb_local.sh", List(runDynamoTypeOption), "Install Local DynamoDB")
        _ <- runScript("run_dynamodb_local.sh", List(runDynamoTypeOption, "-p", LOCAL_DB_PORT.toString), "Start Local DynamoDB")
      } yield ())
    }

  def stopLocalDynamoDB() =
    withLocalDb {
      runAttemptStep {
        runScript("stop_dynamodb_local.sh", List(LOCAL_DB_PORT.toString), "Stop local Dynamo DB")
      }
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
      AmazonClient.withClientConfiguration[AmazonDynamoDBClient](
        AmazonClientConnectionDef.default.copy(endpointUrl = Some(s"http://localhost:$LOCAL_DB_PORT"),
          credential = Some(Credential.static("FOO", "BAR"))),
        None,
        None)
    } else {
      val c = AmazonClient.default[AmazonDynamoDBClient]
      c.setRegion(AmazonRegion.orDefault(REGION))
      c
    }
}
