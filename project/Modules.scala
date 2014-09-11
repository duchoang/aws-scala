import sbt._
import sbt.Keys._
import Settings._

trait Modules {

  lazy val forkTests =
    Seq[Def.Setting[_]] (
      fork in Test := true,
      javaOptions += "-Xmx1G"
    )

  lazy val core =
    Project(
      id = "core",
      base = file("core"),
      settings = standardSettingsAndDependencies
    )

  lazy val all =
    Project(id = "all", base = file("."), settings = standardSettings) aggregate (
      core
    )
}
