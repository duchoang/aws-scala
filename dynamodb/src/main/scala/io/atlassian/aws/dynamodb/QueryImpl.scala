package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.{ QueryRequest, Condition }
import DynamoDB.ReadConsistency
import collection.JavaConverters._
import scalaz.syntax.id._

private[dynamodb] object QueryImpl {
  def forHash[K](
    hashKey: K,
    exclusiveStartKey: Option[DynamoMap] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    indexName: Option[String] = None,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(tableName: String, keyCol: NamedColumn[K]): QueryImpl =
    QueryImpl(
      tableName,
      indexName,
      Map(keyCol.name -> condition(hashKey, Comparison.Eq)(keyCol.column)),
      exclusiveStartKey,
      scanDirection,
      consistency,
      limit
    )

  def forHashAndRange[K, O](
    hashKey: K,
    rangeKey: O,
    rangeComparison: Comparison,
    exclusiveStartKey: Option[DynamoMap] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    indexName: Option[String] = None,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(tableName: String, keyCol: NamedColumn[K], ordCol: NamedColumn[O]): QueryImpl =
    QueryImpl(
      tableName,
      indexName,
      Map(
        keyCol.name -> condition(hashKey, Comparison.Eq)(keyCol.column),
        ordCol.name -> condition(rangeKey, rangeComparison)(ordCol.column)
      ),
      exclusiveStartKey,
      scanDirection,
      consistency,
      limit
    )

  def nextFromQuery[A](query: QueryImpl, exclusiveStartKey: DynamoMap): QueryImpl =
    QueryImpl(query.table, query.indexName, query.keyConditions, Some(exclusiveStartKey), query.scanDirection, query.consistency, query.limit)

  private def condition[K](key: K, comparator: Comparison)(kc: Column[K]) =
    new Condition().withComparisonOperator(Comparison.asAWS(comparator)).withAttributeValueList(
      kc.marshall.toFlattenedMap(key).values.asJavaCollection)
}

private[dynamodb] case class QueryImpl(table: String,
                                       indexName: Option[String],
                                       keyConditions: Map[String, Condition],
                                       exclusiveStartKey: Option[DynamoMap],
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
        indexName.foreach { req.setIndexName }
        limit.foreach { req.setLimit(_) }
        exclusiveStartKey.foreach { esk => req.setExclusiveStartKey(esk.asJava) }
      }
  }
}
