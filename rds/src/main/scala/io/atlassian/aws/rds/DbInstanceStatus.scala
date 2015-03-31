package io.atlassian.aws.rds

import scalaz.syntax.std.option._

sealed trait DbInstanceStatus

object DbInstanceStatus {
  case object Available extends DbInstanceStatus
  case object BackingUp extends DbInstanceStatus
  case object Creating extends DbInstanceStatus
  case object Deleting extends DbInstanceStatus
  case object Failed extends DbInstanceStatus
  case object IncompatibleNetwork extends DbInstanceStatus
  case object IncompatibleOptionGroup extends DbInstanceStatus
  case object IncompatibleParameters extends DbInstanceStatus
  case object IncompatibleRestore extends DbInstanceStatus
  case object Modifying extends DbInstanceStatus
  case object Rebooting extends DbInstanceStatus
  case object Renaming extends DbInstanceStatus
  case object ResettingMasterCredentials extends DbInstanceStatus
  case object StorageFull extends DbInstanceStatus

  def unapply(s: String): Option[DbInstanceStatus] =
    s match {
      case "available"                    => Available.some
      case "backing-up"                   => BackingUp.some
      case "creating"                     => Creating.some
      case "deleting"                     => Deleting.some
      case "failed"                       => Failed.some
      case "incompatible-network"         => IncompatibleNetwork.some
      case "incompatible-option-group"    => IncompatibleOptionGroup.some
      case "incompatible-parameters"      => IncompatibleParameters.some
      case "incompatible-restore"         => IncompatibleRestore.some
      case "modifying"                    => Modifying.some
      case "rebooting"                    => Rebooting.some
      case "renaming"                     => Renaming.some
      case "resetting-master-credentials" => ResettingMasterCredentials.some
      case "storage-full"                 => StorageFull.some
      case _                              => None
    }
}
