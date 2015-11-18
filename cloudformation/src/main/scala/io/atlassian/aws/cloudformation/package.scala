package io.atlassian.aws

import com.amazonaws.services.cloudformation.AmazonCloudFormationClient

package object cloudformation extends cloudformation.Types {
  type CFAction[A] = AwsAction[AmazonCloudFormationClient, MetaData, A]
}
