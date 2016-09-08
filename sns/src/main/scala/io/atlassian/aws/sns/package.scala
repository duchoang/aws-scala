package io.atlassian.aws

import com.amazonaws.services.sns.AmazonSNS

package object sns extends sns.Types {
  type SNSAction[A] = AwsAction[AmazonSNS, MetaData, A]
}

