package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.{ UpdateItemRequest, AttributeValue, AttributeAction, AttributeValueUpdate }
import scala.collection.JavaConverters._

/**
 * Type class for things that can be stored as 'values' in a DynamoDB table.
 * @tparam A The thing that can be stored as 'values' in a DynamoDB table
 */
trait StoreValue[A] {
  def asNew(a: A)(implicit ev: Marshaller[A]): UpdateItemRequestEndo
  def asUpdated(o: A, a: A)(implicit ev: Marshaller[A]): UpdateItemRequestEndo
}

object StoreValue {
  def delete: AttributeValueUpdate = new AttributeValueUpdate().withAction(AttributeAction.DELETE)

  def put(value: AttributeValue): AttributeValueUpdate = new AttributeValueUpdate().withAction(AttributeAction.PUT).withValue(value)

  def updatedFromMap(map: Map[String, AttributeValueUpdate]): UpdateItemRequestEndo =
    scalaz.Endo[UpdateItemRequest] {
      _.withAttributeUpdates(map.asJava)
    }

  /**
   * Create an [[UpdateItemRequestEndo]] from an object's field values using what the object's [[Marshaller]]
   * provides. If marshalling the field generates None then we assume we need to delete the field, otherwise the
   * new value is just put.
   */
  def newFromValues[A](a: A)(implicit ev: Marshaller[A]): UpdateItemRequestEndo = {
    val updates = ev.toMap(a).mapValues {
      case None                 => delete
      case Some(attributeValue) => put(attributeValue)
    }
    scalaz.Endo[UpdateItemRequest](_.withAttributeUpdates(updates.asJava))
  }

  /**
   * Create a StoreValue assuming that 'asNew' is just putting all the attribute values into DynamoDB.
   * @param update Function that takes an original value, and an new value and generates an [[UpdateItemRequestEndo]] that represents the update function.
   */
  def withUpdated[A](update: (A, A) => UpdateItemRequestEndo) = new StoreValue[A] {
    def asNew(a: A)(implicit ev: Marshaller[A]) = newFromValues(a)
    def asUpdated(o: A, a: A)(implicit ev: Marshaller[A]) = update(o, a)
  }
}
