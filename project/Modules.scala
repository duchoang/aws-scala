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

  lazy val rds =
    Project(id = "rds", base = file("rds"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val s3 =
    Project(id = "s3", base = file("s3"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val sqs =
    Project(id = "sqs", base = file("sqs"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val sns =
    Project(id = "sns", base = file("sns"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)

  lazy val swf =
    Project(id = "swf", base = file("swf"), settings = standardSettingsAndDependencies) dependsOn (core % depTest)
  
  lazy val swfAkka =
    Project(id = "swf-akka", base = file("swf-akka"), settings = standardSettingsAndDependencies) dependsOn (core % depTest, swf)
  
  lazy val swfScalazStream =
    Project(id = "swf-scalazstream", base = file("swf-scalazstream"), settings = standardSettingsAndDependencies) dependsOn (core % depTest, swf)

  lazy val all =
    Project(id = "all", base = file("."), settings = standardSettings) dependsOn ( // needs both dependsOn and aggregate to produce dependencies in the pom
      core, cloudformation, dynamodb, rds, s3, sqs, sns, swf, swfAkka, swfScalazStream
    ) aggregate (
      core, cloudformation, dynamodb, rds, s3, sqs, sns, swf, swfAkka, swfScalazStream
    )
}
