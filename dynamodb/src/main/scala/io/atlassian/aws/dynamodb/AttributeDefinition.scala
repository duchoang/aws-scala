package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.{ ScalarAttributeType, AttributeDefinition => AttDef }

/**
 * Convenience functions to construct an AttributeDefinition
 */
object AttributeDefinition {
  def apply(name: String, attributeType: ScalarAttributeType): AttDef =
    new AttDef(name, attributeType)

  def string(name: String): AttDef =
    apply(name, ScalarAttributeType.S)

  def number(name: String): AttDef =
    apply(name, ScalarAttributeType.N)
}
