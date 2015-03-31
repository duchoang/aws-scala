package io.atlassian.aws
package rds

sealed case class DbInstanceClass(name: String) {
  override def toString = name
}

object DbInstanceClass {
  val t1micro = DbInstanceClass("db.t1.micro")
  val m1small = DbInstanceClass("db.m1.small")
  val m1medium = DbInstanceClass("db.m1.medium")
  val m1large = DbInstanceClass("db.m1.large")
  val m1xlarge = DbInstanceClass("db.m1.xlarge")
  val m2xlarge = DbInstanceClass("db.m2.xlarge")
  val m24xlarge = DbInstanceClass("db.m2.4xlarge")
  val m3medium = DbInstanceClass("db.m3.medium")
  val m3large = DbInstanceClass("db.m3.large")
  val m3xlarge = DbInstanceClass("db.m3.xlarge")
  val m32xlarge = DbInstanceClass("db.m3.2xlarge")
  val r3large = DbInstanceClass("db.r3.large")
  val r3xlarge = DbInstanceClass("db.r3.xlarge")
  val r32xlarge = DbInstanceClass("db.r3.2xlarge")
  val r34xlarge = DbInstanceClass("db.r3.4xlarge")
  val r38xlarge = DbInstanceClass("db.r3.8xlarge")
  val t2micro = DbInstanceClass("db.t2.micro")
  val t2small = DbInstanceClass("db.t2.small")
  val t2medium = DbInstanceClass("db.t2.medium")
}
