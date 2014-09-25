package io.atlassian.aws

import com.amazonaws.services.sqs.AmazonSQS
import com.amazonaws.services.sqs.model.MessageAttributeValue

package object sqs extends sqs.Types {
  type SQSAction[A] = AwsAction[AmazonSQS, A]
  type Value = Option[MessageAttributeValue]
  type KeyValue = Map[String, Value]

}
