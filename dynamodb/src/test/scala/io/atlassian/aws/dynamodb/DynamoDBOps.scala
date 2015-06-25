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

  def createTable[K, V, H: Decoder, R: Decoder](create: Schema.Create[K, V, H, R])(implicit client: AmazonDynamoDB) = {
    runAction.apply(DynamoDB.createHashKeyTable(create)) match {
      case -\/(e) =>
        error(s"Error creating table: $e")
        Failure(s"Error creating table: $e")
      case \/-(task) =>
        debug(s"Creating table ${create.std.kv.name}")
        task.run
        debug(s"Created table ${create.std.kv.name}")
        Success
    }
  }

  def deleteTable[K, V](table: Schema.KeyValue[K, V])(implicit client: AmazonDynamoDB) =
    runAction.apply(DynamoDB.deleteTable(table))
}
