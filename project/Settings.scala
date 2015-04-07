import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

object Settings {

  val testSettings = testOptions in Test += Tests.Argument("console", "junitxml")

  val scala211 = "2.11.6"

  lazy val standardSettings =
    Defaults.coreDefaultSettings ++
      testSettings ++
      Release.customReleaseSettings ++ // sbt-release
      net.virtualvoid.sbt.graph.Plugin.graphSettings ++ // dependency plugin settings
      defaultScalariformSettings ++
      Seq[Def.Setting[_]] (
        organization := "io.atlassian.aws-scala"
      , scalaVersion := scala211
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
        , Resolver.sonatypeRepo("public")
        , Resolver.sonatypeRepo("releases")
        , Resolver.sonatypeRepo("snapshots")
        , Resolver.bintrayRepo("non", "maven")
        )
      , credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
      , mappings in (Compile, packageBin) ++= Seq(
        file("LICENSE") -> "META-INF/LICENSE"
        , file("NOTICE")  -> "META-INF/NOTICE"
        )
      , incOptions := incOptions.value.withNameHashing(true) // SBT 0.13.2 name hashing
      , updateOptions := updateOptions.value.withCachedResolution(true)
      , ScalariformKeys.preferences := ScalariformKeys.preferences.value
          .setPreference(AlignSingleLineCaseStatements, true)
          .setPreference(AlignParameters, true)
      , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.2")
      )

  lazy val standardSettingsAndDependencies =
    standardSettings ++
      Dependencies.common ++
      Dependencies.test

}
