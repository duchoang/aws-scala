package io.atlassian.aws.rds

sealed case class Engine(name: String, version: String)

object Engine {
  val mysql5619 = Engine("mysql", "5.6.19")

  // TODO - Add more as required
}
