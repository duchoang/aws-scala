import sbt._
import Keys._

object Dependencies {

  lazy val SCALAZ_VERSION = "7.1.1"
  
  lazy val SCALAZ_STREAM_VERSION = "0.7a"

  lazy val ARGONAUT_VERSION = "6.1-M6"

  lazy val AWS_SDK_VERSION = "1.9.16"

  lazy val KADAI_VERSION = "3.1.1"
  
  lazy val AKKA_VERSION = "2.3.9"

  lazy val SPECS2_VERSION = "3.5"

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"        % SCALAZ_VERSION
    , "org.scalaz"        %% "scalaz-concurrent"  % SCALAZ_VERSION
    , "io.argonaut"       %% "argonaut"           % ARGONAUT_VERSION
    , "com.amazonaws"     %  "aws-java-sdk"       % AWS_SDK_VERSION
    , "io.atlassian"      %% "kadai-core"         % KADAI_VERSION
    , "io.atlassian"      %% "kadai-config"       % KADAI_VERSION
    , "io.atlassian"      %% "kadai-logging-json" % KADAI_VERSION
    )
  )

  lazy val scalazStream = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz.stream" %% "scalaz-stream"      % SCALAZ_STREAM_VERSION
    )
  )
  
  lazy val akka = Seq(
    libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-actor"       % AKKA_VERSION exclude("com.chuusai", "shapeless_2.10.4")
    )
  )

  lazy val test = libraryDependencies ++= Seq(
    "org.specs2"                 %% "specs2-core"        % SPECS2_VERSION  % "test"
    , "org.specs2"               %% "specs2-junit"       % SPECS2_VERSION  % "test"
    , "org.specs2"               %% "specs2-scalacheck"  % SPECS2_VERSION  % "test"
    , "org.specs2"               %% "specs2-mock"        % SPECS2_VERSION  % "test"
    , "org.scalacheck"           %% "scalacheck"         % "1.12.2"        % "test"
    , "junit"                    %  "junit"              % "4.11"          % "test"
  )

  lazy val scodecBits = libraryDependencies ++= Seq(
    "org.scodec" %% "scodec-bits" % "1.0.6"
  )
}
