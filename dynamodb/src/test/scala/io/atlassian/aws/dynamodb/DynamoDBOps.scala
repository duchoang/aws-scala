package io.atlassian.aws
package dynamodb

import scalaz.{ Equal, \/, \/-, -\/, ~> }
import kadai.Invalid
import org.specs2.matcher.{ Expectable, Matcher }
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }
import reflect.ClassTag

object DynamoDBOps extends Logging {
  import Logging._

  def runAction(implicit client: AmazonDynamoDB): DynamoDBAction ~> (Invalid \/ ?) =
    new (DynamoDBAction ~>(Invalid \/ ?)) {
      def apply[A](action: DynamoDBAction[A]) =
        action.run(client).run
    }

  def createSimpleKeyTable[K, V](table: HashKeyTableDefinition[K, V])(implicit client: AmazonDynamoDB) = {
    runAction.apply(DynamoDB.createHashKeyTable[K, V](table)) match {
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

  def createComplexKeyTable[H, R, V](table: HashRangeKeyTableDefinition[H, R, V])(implicit client: AmazonDynamoDB) = {
    runAction.apply(DynamoDB.createHashAndRangeKeyTable[H, R, V](table)) match {
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

  def deleteSimpleKeyTable[K, V](table: HashKeyTableDefinition[K, V])(implicit client: AmazonDynamoDB) =
    runAction.apply(DynamoDB.deleteTable(table))

  def deleteComplexKeyTable[H, R, V](table: HashRangeKeyTableDefinition[H, R, V])(implicit client: AmazonDynamoDB) =
    runAction.apply(DynamoDB.deleteTable(table))
}
