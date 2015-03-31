package io.atlassian.aws
package rds

import com.amazonaws.services.rds.model.{ DescribeDBInstancesRequest }

import scalaz.syntax.id._

object RDS {
  import RDSAction._

  def createInstance(req: CreateInstance): RDSAction[DbId] =
    withClient { _.createDBInstance(req.aws).getDBInstanceIdentifier |> DbId.apply }

  def createReadReplica(req: CreateReadReplica): RDSAction[DbId] =
    withClient { _.createDBInstanceReadReplica(req.aws).getDBInstanceIdentifier |> DbId.apply }

  def instanceStatus(id: DbId): RDSAction[DbInstanceStatus] =
    withClient {
      _.describeDBInstances(
        new DescribeDBInstancesRequest().withDBInstanceIdentifier(id.unwrap)
      ).getDBInstances.get(0).getDBInstanceStatus |> DbInstanceStatus.unapply
    }
      .flatMap { _.fold(RDSAction.fail[DbInstanceStatus]("Unknown status"))(RDSAction.ok) }
}
