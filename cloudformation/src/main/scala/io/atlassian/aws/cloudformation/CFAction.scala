package io.atlassian.aws
package cloudformation

import com.amazonaws.services.cloudformation.AmazonCloudFormationClient

object CFAction extends Functions[AmazonCloudFormationClient, MetaData] {
  override type Action[A] = CFAction[A]
  override implicit def WMonoid = MetaDataMonoid
}