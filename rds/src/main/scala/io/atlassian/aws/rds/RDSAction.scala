package io.atlassian.aws
package rds

import com.amazonaws.services.rds.AmazonRDS

object RDSAction extends AwsAction.Functions[AmazonRDS] {
  override type Action[A] = RDSAction[A]
}