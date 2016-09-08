package io.atlassian.aws

import com.amazonaws.services.sns.AmazonSNS
//import com.amazonaws.services.sqs.model.MessageAttributeValue

package object sns extends sns.Types {
  type SNSAction[A] = AwsAction[AmazonSNS, MetaData, A]

//  type Value = Option[MessageAttributeValue]
//  type KeyValue = Map[String, Value]
}

