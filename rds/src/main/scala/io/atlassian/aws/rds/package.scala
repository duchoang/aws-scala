package io.atlassian.aws

import com.amazonaws.services.rds.AmazonRDS
import com.amazonaws.services.rds.model.CreateDBInstanceRequest

package object rds extends rds.Types {
  type RDSAction[A] = AwsAction[AmazonRDS, A]

  implicit class AvailabilityZoneSyntax(val a: AvailabilityZone) extends AnyVal {
    def aws(request: CreateDBInstanceRequest): CreateDBInstanceRequest =
      a.fold(
      { request.withMultiAZ(true).withAvailabilityZone(null) },
      { zone => request.withMultiAZ(false).withAvailabilityZone(zone) })
  }
}
