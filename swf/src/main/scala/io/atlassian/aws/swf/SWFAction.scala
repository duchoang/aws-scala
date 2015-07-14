package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow

object SWFAction extends Functions[AmazonSimpleWorkflow, MetaData] {
  override type Action[A] = SWFAction[A]
  override implicit def WMonoid = MetaDataMonoid
}
