package io.atlassian.aws

import com.amazonaws.services.cloudformation.AmazonCloudFormationClient

package object cloudformation extends Types {
  type CFAction[A] = AwsAction[AmazonCloudFormationClient, A]
}
