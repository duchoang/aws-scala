package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.{ QueryRequest, Condition, AttributeValue }
import DynamoDB.ReadConsistency
import collection.JavaConverters._
import scalaz.syntax.id._

private[dynamodb] object QueryImpl {
  def forHash[K](
    hashKey: K,
    exclusiveStartKey: Option[Map[String, AttributeValue]] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(tableName: String, keyCol: NamedColumn[K]): QueryImpl =
    QueryImpl(
      tableName,
      Map(keyCol.name -> condition(hashKey, Comparison.Eq)(keyCol)),
      exclusiveStartKey,
      scanDirection,
      consistency,
      limit
    )

  def forHashAndRange[K, O](
    hashKey: K,
    rangeKey: O,
    rangeComparison: Comparison,
    exclusiveStartKey: Option[Map[String, AttributeValue]] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(tableName: String, keyCol: NamedColumn[K], ordCol: NamedColumn[O]): QueryImpl =
    QueryImpl(
      tableName,
      Map(
        keyCol.name -> condition(hashKey, Comparison.Eq)(keyCol),
        ordCol.name -> condition(rangeKey, rangeComparison)(ordCol)
      ),
      exclusiveStartKey,
      scanDirection,
      consistency,
      limit
    )

  def nextFromQuery[A](query: QueryImpl, exclusiveStartKey: Map[String, AttributeValue]): QueryImpl =
    QueryImpl(query.table, query.keyConditions, Some(exclusiveStartKey), query.scanDirection, query.consistency, query.limit)

  private def condition[K](key: K, comparator: Comparison)(kc: Column[K]) =
    new Condition().withComparisonOperator(Comparison.asAWS(comparator)).withAttributeValueList(
      kc.marshall.toFlattenedMap(key).values.asJavaCollection)
}

private[dynamodb] case class QueryImpl(table: String,
  keyConditions: Map[String, Condition],
  exclusiveStartKey: Option[Map[String, AttributeValue]],
  scanDirection: ScanDirection,
  consistency: ReadConsistency,
  limit: Option[Int]) {
  def asQueryRequest: QueryRequest = {
    new QueryRequest()
      .withTableName(table)
      .withKeyConditions(keyConditions.asJava)
      .withScanIndexForward(ScanDirection.asBool(scanDirection))
      .withConsistentRead(ReadConsistency.asBool(consistency)) <|
      { req =>
        limit.foreach { req.setLimit(_) }
        exclusiveStartKey.foreach { esk => req.setExclusiveStartKey(esk.asJava) }
      }
  }
}
