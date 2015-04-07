package io.atlassian.aws
package rds

import com.amazonaws.services.rds.model.{ Tag, CreateDBInstanceRequest }
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scalaz.syntax.id._

case class BackupParameters(backupRetentionPeriod: Option[Duration] = None,
                            backupWindow: Option[String] = None) // TODO - Probably should be duration that formats as expected

case class SecurityParameters(securityGroupName: List[String] = Nil,
                              publiclyAccessible: Boolean = true,
                              storageEncrypted: Boolean = false,
                              kmsKeyId: Option[String] = None,
                              tdeCredentialArn: Option[String] = None,
                              tdeCredentialPassword: Option[String] = None,
                              vpcSecurityGroupId: List[String] = Nil)
case class CreateInstance(engine: Engine,
                          id: DbId,
                          allocatedStorage: Int,
                          instanceClass: DbInstanceClass,
                          masterUsername: String,
                          masterPassword: String,
                          backupParameters: BackupParameters,
                          securityParameters: SecurityParameters,
                          availabilityZone: AvailabilityZone = AvailabilityZone.Multiple,
                          characterSetName: Option[String] = None,
                          optionGroupName: Option[String] = None,
                          dbParameterGroupName: Option[String] = None,
                          subnetGroupName: Option[String] = None,
                          iops: Option[Int] = None,
                          licenceModel: Option[LicenceModel] = None,
                          dbName: Option[String] = None,
                          autoMinorVersionUpgrade: Boolean = true,
                          maintenanceWindow: Option[String] = None,
                          port: Option[Int] = None,
                          storageType: Option[StorageType] = None,
                          tags: Map[String, String] = Map.empty) {
  def aws: CreateDBInstanceRequest = {
    val req = new CreateDBInstanceRequest()
      .withDBInstanceIdentifier(id.unwrap)
      .withAllocatedStorage(allocatedStorage)
      .withAutoMinorVersionUpgrade(autoMinorVersionUpgrade)
      .withPubliclyAccessible(securityParameters.publiclyAccessible)
      .withStorageEncrypted(securityParameters.storageEncrypted)
      .withVpcSecurityGroupIds(securityParameters.vpcSecurityGroupId.asJava)
      .withTags(tags.map { case (k, v) => new Tag().withKey(k).withValue(v) }.asJavaCollection)
      .withDBSecurityGroups(securityParameters.securityGroupName.asJava) |> availabilityZone.aws

    backupParameters.backupRetentionPeriod.foreach { d => req.setBackupRetentionPeriod(d.toDays.toInt) }
    characterSetName.foreach { req.setCharacterSetName }
    optionGroupName.foreach { req.setOptionGroupName }
    dbParameterGroupName.foreach { req.setDBParameterGroupName }
    subnetGroupName.foreach { req.setDBSubnetGroupName }
    iops.foreach { i => req.setIops(Integer.valueOf(i)) }
    securityParameters.kmsKeyId.foreach { req.setKmsKeyId }
    licenceModel.foreach { l => req.setLicenseModel(l.name) }
    dbName.foreach { req.setDBName }
    backupParameters.backupWindow.foreach { req.setPreferredBackupWindow }
    maintenanceWindow.foreach { req.setPreferredMaintenanceWindow }
    port.foreach { p => req.setPort(Integer.valueOf(p)) }
    storageType.foreach { s => req.setStorageType(s.name) }
    securityParameters.tdeCredentialArn.foreach { req.setTdeCredentialArn }
    securityParameters.tdeCredentialPassword.foreach { req.setTdeCredentialPassword }

    req
  }
}
