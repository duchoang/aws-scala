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
  import AwsAction._
  import Logging._

  def runAction(implicit client: AmazonDynamoDB): DynamoDBAction ~> (Invalid \/ ?) =
    new (DynamoDBAction ~>(Invalid \/ ?)) {
      def apply[A](action: DynamoDBAction[A]) =
        action.runAction(client).run
    }

  def createTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDB) = {
    runAction.apply(DynamoDB.createTable[K, V, H, R](table)) match {
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

  def deleteTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDB) =
    runAction.apply(DynamoDB.deleteTable(table))
}
