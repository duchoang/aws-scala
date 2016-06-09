package io.atlassian.aws

import org.specs2.{ SpecificationWithJUnit, ScalaCheck }
import org.specs2.matcher.DisjunctionMatchers
import org.scalacheck.Prop
import kadai.Invalid
import scalaz.{ Monoid, -\/ }
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

  implicit def Monoid = new Monoid[Unit] {
    override def zero: Unit = ()
    override def append(f1: Unit, f2: => Unit): Unit = ()
  }

  implicit class ActionOps[R, W, A](action: AwsAction[R, W, A]) extends AwsActionOps(action)

  def withClientThrowsIsHandled =
    Prop.forAll { msg: String =>
      withClient[String, Unit, String] {
        s => throw new RuntimeException(s)
      }.runAction(msg).run should be_-\/ like {
        case -\/(Invalid.Err(t)) => t.getMessage === msg
      }
    }

  def askInput =
    Prop.forAll { msg: String => ask[String, Unit].runAction(msg).run should be_\/-(msg) }

  def localChanges =
    Prop.forAll { i: String => local[String, Unit, String](_.hashCode.toString)(ask[String, Unit]).runAction(i).run should be_\/-(i.hashCode.toString) }

  def recover =
    Prop.forAll { msg: String =>
      fail[String, Unit, String](msg).recover {
        case Invalid.Message(s) => AwsAction.ok[String, Unit, String](s)
        case _                  => fail[String, Unit, String](msg)
      }.runAction("1").run should be_\/-(msg)
    }

  def handle =
    Prop.forAll { msg: String =>
      fail[String, Unit, String](msg).handle {
        case Invalid.Message(s) => AwsAction.ok[String, Unit, String](s)
      }.runAction("1").run should be_\/-(msg)
    }

  def amazonNotFound =
    Prop.forAll { msg: String =>
      withClient[String, Unit, String] {
        s =>
          {
            val e = new AmazonServiceException(s)
            e.setStatusCode(404)
            throw e
          }
      }.runAction(msg).run should be_-\/ like {
        case -\/(Invalid.Err(ServiceException(ExceptionType.NotFound, t))) => t.getErrorMessage === msg
      }
    }
}