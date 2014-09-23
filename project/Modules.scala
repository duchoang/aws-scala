import sbt._
import sbt.Keys._
import Settings._

trait Modules {

  val depTest = "test->test;compile->compile"

  lazy val forkTests =
    Seq[Def.Setting[_]] (
      fork in Test := true,
      javaOptions += "-Xmx1G"
    )

  lazy val core =
    Project(id = "core", base = file("core"), settings = standardSettingsAndDependencies)

  lazy val cloudformation =
    Project(id = "cloudformation", base = file("cloudformation"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val dynamodb =
    Project(id = "dynamodb", base = file("dynamodb"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val s3 =
    Project(id = "s3", base = file("s3"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val sqs =
    Project(id = "sqs", base = file("sqs"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val all =
    Project(id = "all", base = file("."), settings = standardSettings) dependsOn ( // needs both dependsOn and aggregate to produce dependencies in the pom
      core, cloudformation, dynamodb, s3, sqs
    ) aggregate (
      core, cloudformation, dynamodb, s3, sqs
    )
}
