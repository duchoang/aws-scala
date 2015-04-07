package io.atlassian.aws
package rds

import com.amazonaws.services.rds.model.{ Tag, CreateDBInstanceRequest }
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scalaz.syntax.id._

case class CreateInstance(engine: Engine,
                          id: DbId,
                          allocatedStorage: Int,
                          instanceClass: DbInstanceClass,
                          masterUsername: String,
                          masterPassword: String,
                          availabilityZone: AvailabilityZone = AvailabilityZone.Multiple,
                          backupRetentionPeriod: Option[Duration] = None,
                          characterSetName: Option[String] = None,
                          optionGroupName: Option[String] = None,
                          dbParameterGroupName: Option[String] = None,
                          securityGroupName: List[String] = Nil,
                          subnetGroupName: Option[String] = None,
                          iops: Option[Int] = None,
                          kmsKeyId: Option[String] = None,
                          licenceModel: Option[LicenceModel] = None,
                          dbName: Option[String] = None,
                          autoMinorVersionUpgrade: Boolean = true,
                          publiclyAccessible: Boolean = true,
                          backupWindow: Option[String] = None, // TODO - Probably should be duration that formats as expected
                          maintenanceWindow: Option[String] = None,
                          port: Option[Int] = None,
                          storageEncrypted: Boolean = false,
                          storageType: Option[StorageType] = None,
                          tags: Map[String, String] = Map.empty,
                          tdeCredentialArn: Option[String] = None,
                          tdeCredentialPassword: Option[String] = None,
                          vpcSecurityGroupId: List[String] = Nil) {
  def aws: CreateDBInstanceRequest = {
    val req = new CreateDBInstanceRequest()
      .withDBInstanceIdentifier(id.unwrap)
      .withAllocatedStorage(allocatedStorage)
      .withAutoMinorVersionUpgrade(autoMinorVersionUpgrade)
      .withPubliclyAccessible(publiclyAccessible)
      .withStorageEncrypted(storageEncrypted)
      .withVpcSecurityGroupIds(vpcSecurityGroupId.asJava)
      .withTags(tags.map { case (k, v) => new Tag().withKey(k).withValue(v) }.asJavaCollection)
      .withDBSecurityGroups(securityGroupName.asJava) |> availabilityZone.aws

    backupRetentionPeriod.foreach { d => req.setBackupRetentionPeriod(d.toDays.toInt) }
    characterSetName.foreach { req.setCharacterSetName }
    optionGroupName.foreach { req.setOptionGroupName }
    dbParameterGroupName.foreach { req.setDBParameterGroupName }
    subnetGroupName.foreach { req.setDBSubnetGroupName }
    iops.foreach { i => req.setIops(Integer.valueOf(i)) }
    kmsKeyId.foreach { req.setKmsKeyId }
    licenceModel.foreach { l => req.setLicenseModel(l.name) }
    dbName.foreach { req.setDBName }
    backupWindow.foreach { req.setPreferredBackupWindow }
    maintenanceWindow.foreach { req.setPreferredMaintenanceWindow }
    port.foreach { p => req.setPort(Integer.valueOf(p)) }
    storageType.foreach { s => req.setStorageType(s.name) }
    tdeCredentialArn.foreach { req.setTdeCredentialArn }
    tdeCredentialPassword.foreach { req.setTdeCredentialPassword }

    req
  }
}
