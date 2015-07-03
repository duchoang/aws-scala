package io.atlassian.aws

import org.specs2.{ SpecificationWithJUnit, ScalaCheck }
import org.specs2.matcher.DisjunctionMatchers
import org.scalacheck.Prop
import kadai.Invalid
import scalaz.{ \/-, -\/ }
import scalaz.syntax.id._
import com.amazonaws.AmazonServiceException
import AmazonExceptions._

class AwsActionSpec extends SpecificationWithJUnit with ScalaCheck with DisjunctionMatchers {
  import AwsAction._

  def is = s2"""
  AwsAction should

    not throw in withClient      $withClientThrowsIsHandled
    ask should return input      $askInput
    modify local input           $localChanges
    recover                      $recover
    handle                       $handle
    map Amazon 404               $amazonNotFound
  """

  def withClientThrowsIsHandled =
    Prop.forAll { msg: String =>
      withClient[String, String] {
        s => throw new RuntimeException(s)
      }.run(msg).run should be_-\/ like {
        case -\/(Invalid.Err(t)) => t.getMessage === msg
      }
    }

  def askInput =
    Prop.forAll { msg: String => ask[String].run(msg).run should be_\/-(msg) }

  def localChanges =
    Prop.forAll { i: Int => local[Int, Int](_ + 1)(ask[Int]).run(i).run should be_\/-(i + 1) }

  def recover =
    Prop.forAll { msg: String =>
      fail[Int, String](msg).recover {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
        case _                  => fail[Int, String](msg)
      }.run(1).run should be_\/-(msg)
    }

  def handle =
    Prop.forAll { msg: String =>
      fail[Int, String](msg).handle {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
      }.run(1).run should be_\/-(msg)
    }

  def amazonNotFound =
    Prop.forAll { msg: String =>
      withClient[String, String] {
        s => throw new AmazonServiceException(s) <| { _.setStatusCode(404) }
      }.run(msg).run should be_-\/ like {
        case -\/(Invalid.Err(ServiceException(ExceptionType.NotFound, t))) => t.getErrorMessage === msg
      }
    }
}