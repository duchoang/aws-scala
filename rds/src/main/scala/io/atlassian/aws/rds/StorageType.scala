package io.atlassian.aws
package rds

sealed case class StorageType(name: String) {
  override def toString = name
}

object StorageType {
  val standard = StorageType("standard")
  val gp2 = StorageType("gp2")
  val io1 = StorageType("io1")
}
