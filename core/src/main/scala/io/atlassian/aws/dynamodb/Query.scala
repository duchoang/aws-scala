package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model.{ QueryRequest, Condition, AttributeValue }
import DynamoDB.ReadConsistency
import collection.JavaConverters._
import scalaz.syntax.id._

object Query {
  def forHash[HK, K, V](
    hashKey: HK,
    exclusiveStartKey: Option[Map[String, AttributeValue]] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(
      implicit table: TableDefinition[K, V],
      evKeyMarshaller: Marshaller[HK],
      evKeyColumn: Column[HK]): Query[V] = {

    val keyConditions = Map(
      evKeyColumn.name -> condition(hashKey, Comparison.Eq)
    )
    Query(table.name, keyConditions, exclusiveStartKey, scanDirection, consistency, limit)
  }

  def forHashAndRange[HK, RK, K, V](
    hashKey: HK,
    rangeKey: RK,
    rangeComparison: Comparison,
    exclusiveStartKey: Option[Map[String, AttributeValue]] = None,
    scanDirection: ScanDirection = ScanDirection.Ascending,
    consistency: ReadConsistency = ReadConsistency.Eventual,
    limit: Option[Int] = None)(
      implicit table: TableDefinition[K, V],
      evHashKeyMarshaller: Marshaller[HK],
      evRangeKeyMarshaller: Marshaller[RK],
      evHashKeyColumn: Column[HK],
      evRangeKeyColumn: Column[RK]): Query[V] = {

    val keyConditions = Map(
      evHashKeyColumn.name -> condition(hashKey, Comparison.Eq),
      evRangeKeyColumn.name -> condition(rangeKey, rangeComparison)
    )

    Query(table.name, keyConditions, exclusiveStartKey, scanDirection, consistency, limit)
  }

  def nextFromQuery[A](query: Query[A], exclusiveStartKey: Map[String, AttributeValue]): Query[A] =
    Query(query.table, query.keyConditions, Some(exclusiveStartKey), query.scanDirection, query.consistency, query.limit)

  private def condition[K](key: K, comparator: Comparison)(implicit evKeyMarshaller: Marshaller[K], evKeyColumn: Column[K]) =
    new Condition().withComparisonOperator(Comparison.asAWS(comparator)).withAttributeValueList(
      evKeyMarshaller.toFlattenedMap(key).values.asJavaCollection)

}

case class Query[A](table: String,
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
