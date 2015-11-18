package io.atlassian.aws
package rds

import com.amazonaws.services.rds.AmazonRDS

object RDSAction extends Functions[AmazonRDS, MetaData] {
  override type Action[A] = RDSAction[A]
}