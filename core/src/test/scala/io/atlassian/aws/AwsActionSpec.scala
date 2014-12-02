package io.atlassian.aws

import org.specs2.{ SpecificationWithJUnit, ScalaCheck }
import org.scalacheck.Prop
import kadai.Invalid
import scalaz.\/-

class AwsActionSpec extends SpecificationWithJUnit with ScalaCheck {
  import AwsAction._

  def is = s2"""
  AwsAction should

    recover       $recover
    handle        $handle
  """

  def recover = 
    Prop.forAll { msg: String =>
      AwsAction.fail[Int, String](msg).recover {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
        case _                  => AwsAction.fail[Int, String](msg)
      }.run(1).run should be like {
        case \/-(s) => s === msg
      }
    }

  def handle = 
    Prop.forAll { msg: String =>
      AwsAction.fail[Int, String](msg).handle {
        case Invalid.Message(s) => AwsAction.ok[Int, String](s)
      }.run(1).run should be like {
        case \/-(s) => s === msg
      }
    }
}