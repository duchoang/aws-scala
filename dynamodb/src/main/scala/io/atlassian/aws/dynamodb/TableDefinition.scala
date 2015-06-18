package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._

import scalaz.State
import scalaz.Isomorphism.<=>

sealed trait BaseTableDefinition[K, V] {
  val name: String
  val key: Column[K]
  val value: Column[V]
}

sealed trait TableQueryDefinition[K, V, H, R] extends BaseTableDefinition[K, V] {
  val hash: NamedColumn[H]
  val range: NamedColumn[R]
}

case class SecondaryIndexDefinition[K, V, H, R](
  indexName: String,
  name: String,
  key: Column[K],
  hash: NamedColumn[H],
  range: NamedColumn[R],
  value: Column[V]) extends TableQueryDefinition[K, V, H, R]

case class LocalSecondaryIndexDefinition(
  indexName: String,
  schemaElements: List[KeySchemaElement],
  projection: Projection)

case class GlobalSecondaryIndexDefinition(
  indexName: String,
  schemaElements: List[KeySchemaElement],
  projection: Projection,
  provisionedThroughput: ProvisionedThroughput)

sealed abstract class IndexProjection[VV](protected val projectionType: ProjectionType) {
  def value: Column[VV]
  protected def nonKeyAttributes: List[String]
  def toProjection: Projection =
    new Projection().withProjectionType(projectionType)
      .withNonKeyAttributes(if (nonKeyAttributes.isEmpty) null else nonKeyAttributes.asJavaCollection)
}
case class KeyOnlyProjection[K](value: Column[K]) extends IndexProjection[K](ProjectionType.KEYS_ONLY) {
  val nonKeyAttributes = List.empty[String]
}
case class AllProjection[V](value: Column[V]) extends IndexProjection[V](ProjectionType.ALL) {
  val nonKeyAttributes = List.empty[String]
}
case class IncludeProjection[IV](value: Column[IV], nonKeyAttributes: List[String]) extends IndexProjection[IV](ProjectionType.INCLUDE)

object IndexProjection {
  def keyOnlyProjection[K, V]: TableDefinition[K, V] => IndexProjection[K] =
    tableDefinition => KeyOnlyProjection(tableDefinition.key)
  def allProjection[K, V]: TableDefinition[K, V] => IndexProjection[V] =
    tableDefinition => AllProjection(tableDefinition.value)
  def includeProjection[K, V, VV](value: Column[VV], nonKeyAttributeNames: List[String]): TableDefinition[K, V] => IndexProjection[VV] =
    _ => IncludeProjection(value, nonKeyAttributeNames)
}

sealed trait TableDefinition[K, V] extends BaseTableDefinition[K, V] { table =>
  val name: String
  val value: Column[V]
  val globalSecondaryIndexes: List[GlobalSecondaryIndexDefinition]
}

case class HashKeyTableDefinition[K, V] private[HashKeyTableDefinition] (
  name: String,
  key: Column[K],
  value: Column[V],
  globalSecondaryIndexes: List[GlobalSecondaryIndexDefinition] = List.empty,
  attributeDefinitions: List[AttributeDefinition],
  // Schema definition representing this key
  schemaElements: List[KeySchemaElement],
  provisionedThroughput: ProvisionedThroughput) extends TableDefinition[K, V] {

  def withGlobalSecondaryIndex(globalIndexDef: GlobalSecondaryIndexDefinition): HashKeyTableDefinition[K, V] =
    copy(globalSecondaryIndexes = globalIndexDef :: globalSecondaryIndexes)
}

case class HashRangeKeyTableDefinition[H, R, V] private[HashRangeKeyTableDefinition] (
  name: String,
  hash: NamedColumn[H],
  range: NamedColumn[R],
  value: Column[V],
  localSecondaryIndexes: List[LocalSecondaryIndexDefinition] = List.empty,
  globalSecondaryIndexes: List[GlobalSecondaryIndexDefinition] = List.empty,
  attributeDefinitions: List[AttributeDefinition],
  // Schema definition representing this key
  schemaElements: List[KeySchemaElement],
  provisionedThroughput: ProvisionedThroughput) extends TableDefinition[(H, R), V] with TableQueryDefinition[(H, R), V, H, R] { self =>

  val key: Column[(H, R)] = Column.compose2(hash, range)(identity[(H, R)])(Tuple2.apply)

  def withLocalSecondaryIndex(localIndexDef: LocalSecondaryIndexDefinition): HashRangeKeyTableDefinition[H, R, V] =
    copy(localSecondaryIndexes = localIndexDef :: localSecondaryIndexes)

  def withGlobalSecondaryIndex(globalIndexDef: GlobalSecondaryIndexDefinition): HashRangeKeyTableDefinition[H, R, V] =
    copy(globalSecondaryIndexes = globalIndexDef :: globalSecondaryIndexes)
}

trait TableDefinitionFunctions {
  def hashSchemaElement(name: String) = new KeySchemaElement(name, KeyType.HASH)

  def rangeSchemaElement(name: String) = new KeySchemaElement(name, KeyType.RANGE)


}

object HashKeyTableDefinition extends TableDefinitionFunctions {
  def from[K: Decoder, V](
                                   tableName: String,
                                   key: NamedColumn[K],
                                   value: Column[V],
                                   provisionedReadCapacity: Long = 5,
                                   provisionedWriteCapacity: Long = 5) =
    HashKeyTableDefinition[K, V](
      tableName,
      key,
      value,
      List.empty,
      List(Decoder[K].dynamoType(key.name)),
      List(hashSchemaElement(key.name)),
      new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
    )

  def withGlobalSecondaryIndexWithIncludeProjection[K, V, HH, RR, VV](
                                                               indexName: String, indexHash: NamedColumn[HH], indexRange: NamedColumn[RR], toIndexProjection: TableDefinition[K, V] => IndexProjection[VV],
                                                               provisionedReadCapacity: Long = 5,
                                                               provisionedWriteCapacity: Long = 5): State[HashKeyTableDefinition[K, V], TableQueryDefinition[K, VV, HH, RR]] =
    State { tableDef =>
      val projection: IndexProjection[VV] = toIndexProjection(tableDef)
      val tableQueryDef = SecondaryIndexDefinition(indexName, tableDef.name, tableDef.key, indexHash, indexRange, projection.value)
      (tableDef.withGlobalSecondaryIndex(
        GlobalSecondaryIndexDefinition(indexName,
          List(hashSchemaElement(tableQueryDef.hash.name), rangeSchemaElement(tableQueryDef.range.name)),
          projection.toProjection,
          new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
        )), tableQueryDef)
    }
}

object HashRangeKeyTableDefinition extends TableDefinitionFunctions {
  def from[H: Decoder, R: Decoder, V](
                                                          tableName: String,
                                                          hash: NamedColumn[H],
                                                          range: NamedColumn[R],
                                                          value: Column[V],
                                                          provisionedReadCapacity: Long = 5,
                                                          provisionedWriteCapacity: Long = 5) =
    HashRangeKeyTableDefinition[H, R, V](
      tableName,
      hash,
      range,
      value,
      List.empty,
      List.empty,
      List(Decoder[H].dynamoType(hash.name), Decoder[R].dynamoType(range.name)),
      List(hashSchemaElement(hash.name), rangeSchemaElement(range.name)),
      new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
    )

  def withLocalSecondaryIndex[K, H, R, V, RR, VV](indexName: String, indexRange: NamedColumn[RR],
      toIndexProjection: TableDefinition[(H, R), V] => IndexProjection[VV])(implicit ev: K <=> (H, R), RR: Decoder[RR]): State[HashRangeKeyTableDefinition[H, R, V], TableQueryDefinition[K, V, H, RR]] =
    State { tableDef =>
      val projection: IndexProjection[VV] = toIndexProjection(tableDef)
      val tableQueryDef = SecondaryIndexDefinition(indexName, tableDef.name, tableDef.key.xmap(ev.from, ev.to), tableDef.hash, indexRange, tableDef.value)
      (tableDef.withLocalSecondaryIndex(
        LocalSecondaryIndexDefinition(indexName,
          List(hashSchemaElement(tableQueryDef.hash.name), rangeSchemaElement(tableQueryDef.range.name)),
          projection.toProjection
        )).copy(attributeDefinitions = (tableDef.attributeDefinitions :+ RR.dynamoType(indexRange.name)).distinct), tableQueryDef)
    }

  def withGlobalSecondaryIndex[K, H, R, V, HH, RR, VV](indexName: String, indexHash: NamedColumn[HH], indexRange: NamedColumn[RR],
      toIndexProjection: TableDefinition[(H, R), V] => IndexProjection[VV],
      provisionedReadCapacity: Long = 5, provisionedWriteCapacity: Long = 5)(implicit ev: K <=> (H, R), HH: Decoder[HH], RR: Decoder[RR]):
        State[HashRangeKeyTableDefinition[H, R, V], TableQueryDefinition[K, V, HH, RR]] =
    State { tableDef =>
      val projection: IndexProjection[VV] = toIndexProjection(tableDef)
      val tableQueryDef = SecondaryIndexDefinition(indexName, tableDef.name, tableDef.key.xmap(ev.from, ev.to), indexHash, indexRange, tableDef.value)
      (tableDef.withGlobalSecondaryIndex(
        GlobalSecondaryIndexDefinition(indexName,
          List(hashSchemaElement(tableQueryDef.hash.name), rangeSchemaElement(tableQueryDef.range.name)),
          projection.toProjection,
          new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
        )).copy(attributeDefinitions = (tableDef.attributeDefinitions :+ HH.dynamoType(indexHash.name) :+ RR.dynamoType(indexRange.name)).distinct), tableQueryDef)
    }

}
