import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

object Settings {

  val testSettings = testOptions in Test += Tests.Argument("console", "junitxml")

  val scala210 = "2.10.5"
  val scala211 = "2.11.8"

  lazy val standardSettings =
    Defaults.coreDefaultSettings ++
      testSettings ++
      Release.customReleaseSettings ++ // sbt-release
      defaultScalariformSettings ++
      Seq[Def.Setting[_]] (
        organization := "io.atlassian.aws-scala"
      , scalaVersion := scala211
      , crossScalaVersions := Seq(scala211, scala210)
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
      , ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
      , javacOptions ++= Seq("-encoding", "UTF-8", "-source", "1.6", "-target", "1.6")
      , javacOptions in doc := Seq("-encoding", "UTF-8")
      , credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
      , mappings in (Compile, packageBin) ++= Seq(
        file("LICENSE.txt") -> "META-INF/LICENSE"
        , file("NOTICE")  -> "META-INF/NOTICE"
        )
      , incOptions := incOptions.value.withNameHashing(true) // SBT 0.13.2 name hashing
      , updateOptions := updateOptions.value.withCachedResolution(true)
      , ScalariformKeys.preferences := ScalariformKeys.preferences.value
          .setPreference(AlignSingleLineCaseStatements, true)
          .setPreference(AlignParameters, true)
      , addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
      , resolvers ++= Seq(
          "atlassian-public" at "https://maven.atlassian.com/content/groups/public/"
        )
      , licenses := Seq("Apache2" -> url("https://bitbucket.org/atlassian/aws-scala/raw/master/LICENSE"))
      , homepage := Some(url("https://bitbucket.org/atlassian/aws-scala"))
      , pomExtra := (
        <scm>
            <url>git@bitbucket.org:atlassian/aws-scala.git</url>
            <connection>scm:git:git@bitbucket.org:atlassian/aws-scala.git</connection>
            <developerConnection>scm:git:git@bitbucket.org:atlassian/aws-scala.git</developerConnection>
        </scm>
        <developers>
            <developer>
                <id>jwesleysmith</id>
                <name>Jed Wesley-Smith</name>
                <email>jwesleysmith@atlassian.com</email>
                <organization>Atlassian</organization>
                <organizationUrl>http://www.atlassian.com</organizationUrl>
            </developer>
            <developer>
                <id>sshek</id>
                <name>Sidney Shek</name>
                <email>sshek@atlassian.com</email>
                <organization>Atlassian</organization>
                <organizationUrl>http://www.atlassian.com</organizationUrl>
            </developer>
        </developers>
        <issueManagement>
            <system>Bitbucket</system>
            <url>https://bitbucket.org/atlassian/aws-scala/issues</url>
        </issueManagement>)
      , pomIncludeRepository := { (repo: MavenRepository) => false } // no repositories in the pom
     )

  lazy val standardSettingsAndDependencies =
    standardSettings ++
      Dependencies.common ++
      Dependencies.test

}
