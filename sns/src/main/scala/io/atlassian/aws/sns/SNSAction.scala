package io.atlassian.aws.sns

import com.amazonaws.services.sns.AmazonSNS
import io.atlassian.aws.{Functions, MetaData}

object SNSAction extends Functions[AmazonSNS, MetaData] {
  override type Action[A] = SNSAction[A]
}
