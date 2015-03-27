package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.{AttributeDefinition => AttrDef}


object Key {
  sealed trait Type {
    def apply(name: String): AttrDef
  }
  case object StringType extends Type {
    def apply(name: String) = AttributeDefinition.string(name)
  }
  case object NumberType extends Type {
    def apply(name: String) = AttributeDefinition.number(name)
  }
  case object BinaryType extends Type {
    def apply(name: String) = AttributeDefinition.binary(name)
  }
}
