import sbt.Keys._
import sbt._
import sbtrelease._

object Settings {

  val testSettings = testOptions in Test += Tests.Argument("console", "junitxml")

  val scala210 = "2.10.4"
  val scala211 = "2.11.2"

  lazy val standardSettings =
    Defaults.coreDefaultSettings ++
      testSettings ++
      Release.customReleaseSettings ++ // sbt-release
      net.virtualvoid.sbt.graph.Plugin.graphSettings ++ // dependency plugin settings
      Seq[Def.Setting[_]] (
        organization := "io.atlassian.aws-scala"
      , scalaVersion := scala211
      , crossScalaVersions := Seq(scala210, scala211)
      , scalacOptions := Seq(
          "-deprecation"
        , "-unchecked"
        , "-feature"
        , "-language:_"
        , "-Xfatal-warnings"
        , "-Xlog-free-terms"
        , "-target:jvm-1.6"
        , "-Xlint"
        , "-Yno-adapted-args"
        , "-Ywarn-dead-code"
        , "-Ywarn-numeric-widen"
        , "-Ywarn-value-discard"
      )
      , javacOptions ++= Seq("-encoding", "UTF-8", "-source", "1.6", "-target", "1.6")
      , javacOptions in doc := Seq("-encoding", "UTF-8")
      , resolvers ++= Seq(
          Resolver.defaultLocal
        , Resolver.mavenLocal
        , "atlassian-public"   at "https://maven.atlassian.com/content/groups/atlassian-public/"
        , "atlassian-internal" at "https://maven.atlassian.com/content/groups/internal/"
        , "Tools Releases"     at "http://oss.sonatype.org/content/repositories/releases"
        , "Tools Snapshots"    at "http://oss.sonatype.org/content/repositories/snapshots"
        )
      , credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
      , mappings in (Compile, packageBin) ++= Seq(
        file("LICENSE") -> "META-INF/LICENSE"
        , file("NOTICE")  -> "META-INF/NOTICE"
        )
      )

  lazy val standardSettingsAndDependencies =
    standardSettings ++
      Dependencies.common ++
      Dependencies.test

}
