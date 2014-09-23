package io.atlassian.aws
package sqs

case class DeleteResult(handles: List[ReceiptHandle], failures: List[FailedDelete]) {
  val errors: Boolean = failures.nonEmpty
}

case class FailedDelete(handle: Option[ReceiptHandle], senderFault: Boolean, error: String)
