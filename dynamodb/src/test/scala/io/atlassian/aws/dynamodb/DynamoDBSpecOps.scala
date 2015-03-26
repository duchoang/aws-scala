package io.atlassian.aws
package dynamodb

import scalaz.{ Equal, \/, \/-, -\/ }
import kadai.Invalid
import org.specs2.matcher.{ Expectable, Matcher }
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }
import reflect.ClassTag

trait DynamoDBSpecOps extends Logging {

  import Logging._

  def createTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDBClient) = {
    runDynamoDBAction(DynamoDB.createTable[K, V, H, R](table)) match {
      case -\/(e) =>
        error(s"Error creating table: $e")
        Failure(s"Error creating table: $e")
      case \/-(task) =>
        debug(s"Creating table ${table.name}")
        task.run
        debug(s"Created table ${table.name}")
        Success
    }
  }

  def deleteTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDBClient) =
    runDynamoDBAction(DynamoDB.deleteTable[K, V, H, R](table))

  def runDynamoDBAction[A](action: DynamoDBAction[A])(implicit client: AmazonDynamoDBClient): Invalid \/ A =
    action.run(client).run

  def returnFailure[A](implicit client: AmazonDynamoDBClient) =
    new ServiceMatcher[A]({
      case -\/(f) => (true, s"Expected failure $f")
      case \/-(v) => (false, s"Expected failure but got value $v")
    })

  private def checkType(klass: Class[_], e: Any): (Boolean, String) =
    (klass.isAssignableFrom(e.getClass), s"Expected class of type $klass, got ${e.getClass}")

  def returnException[A, B <: Throwable](implicit client: AmazonDynamoDBClient, m: ClassTag[B]) =
    new ServiceMatcher[A]({
      case -\/(Invalid.Err(e)) =>
        m.toString match {
          case "Nothing" => checkType(classOf[Throwable], e)
          case _         => checkType(m.runtimeClass, e)
        }
      case -\/(e) => (false, s"Unexpected failure $e")
      case \/-(v) => (false, s"Expected failure but got value $v")
    })

  def returnSuccess[A](implicit client: AmazonDynamoDBClient) =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected success but got failure: $f")
      case \/-(v) => (true, s"Expected success $v")
    })

  def returnValue[A](expected: A)(implicit client: AmazonDynamoDBClient, E: Equal[A]) =
    returnResult[A](a => E.equal(a, expected))

  def returnResult[A](check: A => Boolean)(implicit client: AmazonDynamoDBClient) =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected value, but was failure $f")
      case \/-(v) => (check(v), s"Expected value, but match failed with value $v")
    })

  class ServiceMatcher[A](check: \/[Invalid, A] => (Boolean, String))(implicit client: AmazonDynamoDBClient) extends Matcher[DynamoDBAction[A]] {
    def apply[S <: DynamoDBAction[A]](s: Expectable[S]) = {
      val execResult = runDynamoDBAction(s.value)
      val (comparisonResult, message) = check(execResult)
      result(comparisonResult, message, message, s)
    }
  }
}
