package io.atlassian.aws
package cloudformation

import com.amazonaws.services.cloudformation.AmazonCloudFormationClient
import kadai.Attempt

object CFAction extends AwsAction.Functions[AmazonCloudFormationClient, MetaData] {
  override type Action[A] = CFAction[A]
  override implicit def WMonoid = MetaDataMonoid
}