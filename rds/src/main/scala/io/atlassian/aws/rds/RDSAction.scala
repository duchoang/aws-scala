package io.atlassian.aws
package rds

import com.amazonaws.services.rds.AmazonRDS

object RDSAction extends AwsAction.Functions[AmazonRDS, MetaData] {
  override type Action[A] = RDSAction[A]
  override implicit def WMonoid = MetaDataMonoid
}