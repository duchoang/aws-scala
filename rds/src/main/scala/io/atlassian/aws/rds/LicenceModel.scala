package io.atlassian.aws
package rds

sealed case class LicenceModel(name: String) {
  override def toString = name
}

object LicenceModel {
  val included = LicenceModel("included")
  val byo = LicenceModel("bring-your-own-license")
  val gpl = LicenceModel("general-public-license")
}
