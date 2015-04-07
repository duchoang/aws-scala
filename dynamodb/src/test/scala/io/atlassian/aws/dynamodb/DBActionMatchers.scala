package io.atlassian.aws
package dynamodb

import scalaz.{ Coyoneda, Equal, Free, \/, \/-, -\/, ~> }
import kadai.Invalid
import org.specs2.matcher.{ Expectable, Matcher }
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }
import reflect.ClassTag

trait DBActionMatchers extends Logging {

  import Logging._

  type InvalidOr[A] = Invalid \/ A
  val table: Table

  def run: table.DBOp ~> InvalidOr
  def runFree: table.DBAction ~> InvalidOr =
    table.transform[InvalidOr](run)

  def returnFailure[A] =
    new ServiceMatcher[A]({
      case -\/(f) => (true, s"Expected failure $f")
      case \/-(v) => (false, s"Expected failure but got value $v")
    })

  private def checkType(klass: Class[_], e: Any): (Boolean, String) =
    (klass.isAssignableFrom(e.getClass), s"Expected class of type $klass, got ${e.getClass}")

  def returnException[A, B <: Throwable](implicit m: ClassTag[B]) =
    new ServiceMatcher[A]({
      case -\/(Invalid.Err(e)) =>
        m.toString match {
          case "Nothing" => checkType(classOf[Throwable], e)
          case _         => checkType(m.runtimeClass, e)
        }
      case -\/(e) => (false, s"Unexpected failure $e")
      case \/-(v) => (false, s"Expected failure but got value $v")
    })

  def returnSuccess[A] =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected success but got failure: $f")
      case \/-(v) => (true, s"Expected success $v")
    })

  def returnValue[A](expected: A)(implicit E: Equal[A]): Matcher[table.DBAction[A]] =
    returnResult[A](a => E.equal(a, expected))

  def returnResult[A](check: A => Boolean) =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected value, but was failure $f")
      case \/-(v) => (check(v), s"Expected value, but match failed with value $v")
    })

  class ServiceMatcher[A](check: Invalid \/ A => (Boolean, String)) extends Matcher[table.DBAction[A]] {
    def apply[S <: table.DBAction[A]](s: Expectable[S]) = {
      val (comparisonResult, message) = check(runFree(s.value))
      result(comparisonResult, message, message, s)
    }
  }
}
