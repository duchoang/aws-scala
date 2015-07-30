package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.AmazonSQSClient
import kadai.Invalid
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }
import org.specs2.matcher.{ Matcher, Expectable }

import scalaz.{ \/-, -\/, \/ }

object SQSSpecOps extends Logging {
  import SQSAction._
  import Logging._

  def createTestQueue(name: String)(implicit client: AmazonSQSClient) =
    runSQSAction(SQS.createQueue(QueueParameters(name))) match {
      case -\/(e) =>
        error(s"Error creating queue: $e")
        Failure(s"Error creating queue: $e")
      case \/-(task) =>
        info(s"Creating queue $name with URL $task")
        Success
    }

  def deleteTestQueue(name: String)(implicit client: AmazonSQSClient) = {
    runSQSAction(
      for {
        queueURL <- SQS.queueURL(name)
        _ <- SQS.deleteQueue(queueURL)
      } yield ())
  }

  def runSQSAction[A](action: SQSAction[A])(implicit client: AmazonSQSClient) =
    action.runAction(client).run

  def returnResult[A](check: A => Boolean)(implicit client: AmazonSQSClient) =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected value, but was failure $f")
      case \/-(v) => (check(v), s"Expected value, but match failed with value $v")
    })

  class ServiceMatcher[A](check: \/[Invalid, A] => (Boolean, String))(implicit client: AmazonSQSClient) extends Matcher[SQSAction[A]] {
    def apply[S <: SQSAction[A]](s: Expectable[S]) = {
      val execResult = runSQSAction(s.value)
      val (comparisonResult, message) = check(execResult)
      result(comparisonResult, message, message, s)
    }
  }

}
