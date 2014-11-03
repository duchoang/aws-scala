package io.atlassian.aws
package sqs

/**
 * Header field wraps access to a message header (as opposed to the body). In SQS terms, a header field is what
 * would be stored in the message attributes.
 */
case class HeaderField[A](name: String) {
  def apply(a: A)(implicit ev: MessageAttributeEncoder[A]): HeaderFieldTuple[A] =
    name -> ev(a)

  def get(implicit ev: MessageAttributeDecoder[A]): Unmarshaller.Operation[A] =
    Unmarshaller.Operation.msgAttr(name)
}

case class FieldType(main: FieldMainType, subType: Option[String]) {
  private[sqs] def awsFieldType: String =
    subType.fold(main.name) { s => s"${main.name}.$s" }
}

sealed trait FieldMainType {
  def name: String
}
object FieldMainType {
  case object String extends FieldMainType {
    val name = "String"
  }
  case object Number extends FieldMainType {
    val name = "Number"
  }
  case object Binary extends FieldMainType {
    val name = "Binary"
  }
}

object HeaderTypes {
  val MsSinceEpoch: FieldType = FieldType(FieldMainType.Number, Some(HeaderSubType("msSinceEpoch")))
}
