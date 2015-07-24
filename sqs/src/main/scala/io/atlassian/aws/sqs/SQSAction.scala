package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.AmazonSQS

object SQSAction extends Functions[AmazonSQS, MetaData] {
  override type Action[A] = SQSAction[A]
}