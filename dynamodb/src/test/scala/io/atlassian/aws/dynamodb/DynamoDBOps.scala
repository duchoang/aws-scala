package io.atlassian.aws
package dynamodb

import scalaz.{ \/, \/-, -\/, ~> }
import kadai.Invalid
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB
import kadai.log.Logging
import org.specs2.execute.{ Success, Failure }

object DynamoDBOps extends Logging {
  import Logging._

  def runAction(implicit client: AmazonDynamoDB): DynamoDBAction ~> (Invalid \/ ?) =
    new (DynamoDBAction ~>(Invalid \/ ?)) {
      def apply[A](action: DynamoDBAction[A]) =
        action.run(client).run
    }

  def createTable[K, V, H, R](create: Schema.Create.CreateTable[K, V, H, R])(implicit client: AmazonDynamoDB) = {
    runAction.apply(DynamoDB.createTable(create)) match {
      case -\/(e) =>
        error(s"Error creating table: $e")
        Failure(s"Error creating table: $e")
      case \/-(task) =>
        debug(s"Creating table ${create.tableSchema.kv.name}")
        task.run
        debug(s"Created table ${create.tableSchema.kv.name}")
        Success
    }
  }

  def deleteTable[K, V, H, R](table: Schema.Standard[K, V, H, R])(implicit client: AmazonDynamoDB) =
    runAction.apply(DynamoDB.deleteTable(table))
}
