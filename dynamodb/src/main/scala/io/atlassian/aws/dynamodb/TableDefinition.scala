package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model._

case class TableDefinition[K, V, H, R] private[TableDefinition] (
  name: String,
  key: Column[K],
  hash: NamedColumn[H],
  range: NamedColumn[R],
  value: Column[V],
  attributeDefinitions: List[AttributeDefinition],
  // Schema definition representing this key
  schemaElements: List[KeySchemaElement],
  provisionedThroughput: ProvisionedThroughput)

object TableDefinition {
  def hashSchemaElement(name: String) = new KeySchemaElement(name, KeyType.HASH)

  def rangeSchemaElement(name: String) = new KeySchemaElement(name, KeyType.RANGE)

  def from[K, V, H: Decoder, R: Decoder](
    tableName: String,
    key: Column[K],
    value: Column[V],
    hash: NamedColumn[H],
    range: NamedColumn[R],
    provisionedReadCapacity: Long = 5,
    provisionedWriteCapacity: Long = 5) =
    TableDefinition[K, V, H, R](
      tableName,
      key,
      hash,
      range,
      value,
      List(Decoder[H].dynamoType(hash.name), Decoder[R].dynamoType(range.name)),
      List(hashSchemaElement(hash.name), rangeSchemaElement(range.name)),
      new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
    )
}
