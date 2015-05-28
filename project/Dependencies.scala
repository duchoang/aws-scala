import sbt._
import Keys._

object Dependencies {

  object Version {
    val scalaz        = "7.1.2"
    val scalazStream  = "0.7a"
    val argonaut      = "6.1"
    val aws_sdk       = "1.9.16"
    val kadai         = "3.3.0"
    val akka          = "2.3.9"
    val specs2        = "3.6"
    val scalacheck    = "1.12.2"
    val junit         = "4.11"
    val scodecBits    = "1.0.6"

  }

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"        % Version.scalaz
    , "org.scalaz"        %% "scalaz-concurrent"  % Version.scalaz
    , "io.argonaut"       %% "argonaut"           % Version.argonaut
    , "com.amazonaws"     %  "aws-java-sdk"       % Version.aws_sdk
    , "io.atlassian"      %% "kadai-core"         % Version.kadai
    , "io.atlassian"      %% "kadai-config"       % Version.kadai
    , "io.atlassian"      %% "kadai-logging-json" % Version.kadai
    , "io.atlassian"      %% "kadai-concurrent"   % Version.kadai
    )
  )

  lazy val scalazStream = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz.stream" %% "scalaz-stream"      % Version.scalazStream
    )
  )
  
  lazy val akka = Seq(
    libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor"       % Version.akka exclude("com.chuusai", "shapeless_2.10.4")
    )
  )

  lazy val test = libraryDependencies ++= Seq(
      "org.specs2"          %% "specs2-core"        % Version.specs2     % "test"
    , "org.specs2"          %% "specs2-junit"       % Version.specs2     % "test"
    , "org.specs2"          %% "specs2-scalacheck"  % Version.specs2     % "test"
    , "org.specs2"          %% "specs2-mock"        % Version.specs2     % "test"
    , "org.scalacheck"      %% "scalacheck"         % Version.scalacheck % "test"
    , "junit"                % "junit"              % Version.junit      % "test"
  )

  lazy val scodecBits = libraryDependencies ++= Seq(
    "org.scodec"            %% "scodec-bits"        % Version.scodecBits
  )
}
