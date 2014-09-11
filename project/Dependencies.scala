import sbt._
import Keys._

object Dependencies {

  lazy val SCALAZ_VERSION = "7.0.6"

  lazy val ARGONAUT_VERSION = "6.0.3"

  lazy val AWS_SDK_VERSION = "1.8.9"

  lazy val KADAI_VERSION = "2.0.4"

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"       % SCALAZ_VERSION
    , "org.scalaz"        %% "scalaz-concurrent" % SCALAZ_VERSION
    , "io.argonaut"       %% "argonaut"          % ARGONAUT_VERSION
    , "com.amazonaws"     %  "aws-java-sdk"      % AWS_SDK_VERSION
    , "io.atlassian"      %% "kadai"             % KADAI_VERSION
    )
  )

  lazy val test = libraryDependencies ++= Seq(
    "org.specs2"     %% "specs2"     % "2.3.11"   % "test"
  , "org.scalacheck" %% "scalacheck" % "1.11.3"   % "test"
  , "junit"          %  "junit"      % "4.11"     % "test"
  )
}
