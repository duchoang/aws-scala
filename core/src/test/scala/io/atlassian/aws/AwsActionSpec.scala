package io.atlassian.aws

import org.specs2.{ SpecificationWithJUnit, ScalaCheck }
import org.scalacheck.Prop
import kadai.Invalid
import scalaz.{ \/-, -\/ }
import scalaz.syntax.id._
import com.amazonaws.AmazonServiceException
import AmazonExceptions._

class AwsActionSpec extends SpecificationWithJUnit with ScalaCheck {
  import AwsAction._

  def is = s2"""
  AwsAction should

    not throw in withClient      $withClientThrowsIsHandled
    recover                      $recover
    handle                       $handle
    map Amazon 404               $amazonNotFound
  """

  def withClientThrowsIsHandled =
    Prop.forAll { msg: String =>
      AwsAction.withClient[String, String] {
        s => throw new RuntimeException(s)
      }.run(msg).run should be like { case -\/(Invalid.Err(t)) => t.getMessage === msg }
    }

  def recover =
    Prop.forAll { msg: String =>
      AwsAction.fail[Int, String](msg).recover {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
        case _                  => AwsAction.fail[Int, String](msg)
      }.run(1).run should be like { case \/-(s) => s === msg }
    }

  def handle =
    Prop.forAll { msg: String =>
      AwsAction.fail[Int, String](msg).handle {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
      }.run(1).run should be like { case \/-(s) => s === msg }
    }

  def amazonNotFound =
    Prop.forAll { msg: String =>
      AwsAction.withClient[String, String] {
        s => throw new AmazonServiceException(s) <| { _.setStatusCode(404) }
      }.run(msg).run should be like {
        case -\/(Invalid.Err(AmazonExceptions.ServiceException(ExceptionType.NotFound, t))) => t.getErrorMessage === msg
      }
    }
}