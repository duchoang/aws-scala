package io.atlassian.aws
package rds

import com.amazonaws.services.rds.model.{Tag, CreateDBInstanceReadReplicaRequest}
import scala.collection.JavaConverters._

case class CreateReadReplica(id: DbId,
                             instanceClass: DbInstanceClass,
                             availabilityZone: Option[AvailabilityZone] = None,
                             optionGroupName: Option[String] = None,
                             subnetGroupName: Option[String] = None,
                             iops: Option[Int] = None,
                             autoMinorVersionUpgrade: Boolean = true,
                             publiclyAccessible: Boolean = true,
                             port: Option[Int] = None,
                             storageType: Option[StorageType] = None,
                             tags: Map[String, String] = Map.empty) {
  def aws: CreateDBInstanceReadReplicaRequest = {
    val req =
      new CreateDBInstanceReadReplicaRequest().withAutoMinorVersionUpgrade(autoMinorVersionUpgrade)
        .withDBInstanceClass(instanceClass.name)
        .withDBInstanceIdentifier(id.unwrap)
        .withPubliclyAccessible(publiclyAccessible)
        .withTags(tags.map { case (k, v) => new Tag().withKey(k).withValue(v) }.asJavaCollection)

    availabilityZone.foreach { z => z.fold((), req.setAvailabilityZone) }
    subnetGroupName.foreach { req.setDBSubnetGroupName }
    iops.foreach { i => req.setIops(Integer.valueOf(i)) }
    optionGroupName.foreach { req.setOptionGroupName }
    port.foreach { p => req.setPort(Integer.valueOf(p)) }
    storageType.foreach { s => req.setStorageType(s.name) }

    req
  }
}