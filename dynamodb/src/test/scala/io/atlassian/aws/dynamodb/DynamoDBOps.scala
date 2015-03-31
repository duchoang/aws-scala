package io.atlassian.aws
package dynamodb

import scalaz.{ Equal, \/, \/-, -\/, ~> }
import kadai.Invalid
import org.specs2.matcher.{ Expectable, Matcher }
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }
import reflect.ClassTag

object DynamoDBOps extends Logging {
  import Logging._

  def runAction(implicit client: AmazonDynamoDBClient): DynamoDBAction ~> (Invalid \/ ?) =
    new (DynamoDBAction ~>(Invalid \/ ?)) {
      def apply[A](action: DynamoDBAction[A]) =
        action.run(client).run
    }

  def createTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDBClient) = {
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

  def deleteTable[K, V, H, R](table: TableDefinition[K, V, H, R])(implicit client: AmazonDynamoDBClient) =
    runAction.apply(DynamoDB.deleteTable(table))
}
