package io.atlassian.aws
package swf

import com.amazonaws.services.simpleworkflow.AmazonSimpleWorkflow

object SWFAction extends AwsAction.Functions[AmazonSimpleWorkflow] {
  override type Action[A] = SWFAction[A]
}
