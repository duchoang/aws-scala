package io.atlassian.aws
package dynamodb

import com.amazonaws.services.dynamodbv2.model._

case class TableDefinition[A, B](
  name: String,
  attributeDefinitions: List[AttributeDefinition],
  // Schema definition representing this key
  schemaElements: List[KeySchemaElement],
  provisionedThroughput: ProvisionedThroughput)

object TableDefinition {
  def hashSchemaElement(name: String) = new KeySchemaElement(name, KeyType.HASH)

  def rangeSchemaElement(name: String) = new KeySchemaElement(name, KeyType.RANGE)

  def from[A, B](
    tableName: String,
    hashKey: String,
    rangeKey: Option[AttributeDefinition] = None,
    provisionedReadCapacity: Long = 5,
    provisionedWriteCapacity: Long = 5) =
    TableDefinition[A, B](
      name = tableName,
      attributeDefinitions =
        rangeKey.foldLeft(List(AttributeDefinition.string(hashKey))) {
          (list, keyDef) => list :+ keyDef
        },
      schemaElements =
        rangeKey.foldLeft(List(hashSchemaElement(hashKey))) {
          (list, keyDef) => list :+ rangeSchemaElement(keyDef.getAttributeName)
        },
      provisionedThroughput = new ProvisionedThroughput().withReadCapacityUnits(provisionedReadCapacity).withWriteCapacityUnits(provisionedWriteCapacity)
    )

}
