import sbt._
import Keys._

object Dependencies {

  lazy val SCALAZ_VERSION = "7.1.0"

  lazy val ARGONAUT_VERSION = "6.1-M4"

  lazy val AWS_SDK_VERSION = "1.9.3"

  lazy val KADAI_VERSION = "3.0.0"

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"       % SCALAZ_VERSION
    , "org.scalaz"        %% "scalaz-concurrent" % SCALAZ_VERSION
    , "io.argonaut"       %% "argonaut"          % ARGONAUT_VERSION
    , "com.amazonaws"     %  "aws-java-sdk"      % AWS_SDK_VERSION
    , "io.atlassian"      %% "kadai-core"        % KADAI_VERSION
    , "io.atlassian"      %% "kadai-config"      % KADAI_VERSION
    )
  )

  lazy val test = libraryDependencies ++= Seq(
    "org.specs2"     %% "specs2"     % "2.4.9"    % "test"
  , "org.scalacheck" %% "scalacheck" % "1.11.6"   % "test"
  , "junit"          %  "junit"      % "4.11"     % "test"
  )
}
