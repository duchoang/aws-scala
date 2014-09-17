package io.atlassian.aws
package sqs

import java.util.concurrent.TimeUnit

import com.amazonaws.services.sqs.AmazonSQSClient
import org.scalacheck.Prop
import org.specs2.main.Arguments
import org.specs2.specification.Step
import spec.ScalaCheckSpec
import org.junit.runner.RunWith

import scalaz.syntax.id._
import scala.concurrent.duration.Duration

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class SQSSpec(val arguments: Arguments) extends ScalaCheckSpec {

  import Examples._, Arbitraries._, SQSSpecOps._

  val IS_LOCAL = !arguments.commandLine.contains("aws-integration")
  val REGION = arguments.commandLine.value("region").getOrElse(Option(System.getenv("AWS_REGION")).getOrElse("ap-southeast-2"))

  def is = skipAllIf(IS_LOCAL) ^ stopOnFail ^
    s2"""
       This specification tests SQS functionality

         Create a test queue          ${Step(createTestQueue(TEST_QUEUE_NAME))}

         Send, receive and delete a message $normalFlow

         Delete test queue            ${Step(deleteTestQueue(TEST_QUEUE_NAME))}

    """

  lazy val TEST_QUEUE_NAME = s"sqs-test-${System.currentTimeMillis}"

  implicit val CLIENT = AmazonClient.default[AmazonSQSClient] <| { _.setRegion(AmazonRegion.orDefault(REGION)) }

  def normalFlow = Prop.forAll {
    req: RetriedMessage[Replicate] =>

      (for {
          url <- SQS.queueURL(TEST_QUEUE_NAME)
          sent <- SQS.send(url, req)
          recv <- SQS.receive[RetriedMessage[Replicate]](url, ReceiveMessageParameters(numMessages = 10, waitTime = Some(Duration(5, TimeUnit.SECONDS))))
          handles = recv.map { a => a.map { _.receiptHandle }.toOption }.flatten
          _ <- SQS.delete(url, handles)
        } yield (sent, recv)) must returnResult { case (sent, recv) =>
          recv.length === 1 and
            (recv.head.toOr.toEither must beRight.like {
              case ReceivedMessage(_, _, _, r) => r === req
            })

        }
  }.set(minTestsOk = 10)
}
