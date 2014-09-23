package io.atlassian.aws
package sqs

import org.joda.time.DateTime

case class StandardAttributes(approxFirstReceived: DateTime,
                              approxReceiveCount: Int,
                              senderId: String,
                              sentTime: DateTime)

case class ReceivedMessage[A](messageId: MessageId,
                              receiptHandle: ReceiptHandle,
                              attributes: StandardAttributes,
                              message: A)
