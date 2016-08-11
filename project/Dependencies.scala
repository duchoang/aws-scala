import sbt._
import Keys._

object Dependencies {

  object Version {
    val scalaz         = "7.1.9"
    val scalazStream   = "0.8.2"
    val argonaut       = "6.1"
    val aws_sdk        = "1.10.5.1"
    val kadai          = "4.0.1"
    val kadai_log      = "5.1.2"
    val akka           = "2.3.9"
    val specs2         = "3.8.4-scalaz-7.1"
    val scalacheck     = "1.13.2"
    val scalazcheck    = "0.0.4"
    val junit          = "4.12"
    val scodecBits     = "1.1.0"

  }

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"                 % Version.scalaz
    , "org.scalaz"        %% "scalaz-concurrent"           % Version.scalaz
    , "io.atlassian"      %% "scalazcheck"                 % Version.scalazcheck
    , "io.argonaut"       %% "argonaut"                    % Version.argonaut
    , "com.amazonaws"     %  "aws-java-sdk-core"           % Version.aws_sdk
    , "io.atlassian"      %% "kadai-core"                  % Version.kadai
    , "io.atlassian"      %% "kadai-config"                % Version.kadai
    , "io.atlassian"      %% "kadai-logging-json"          % Version.kadai_log
    , "io.atlassian"      %% "kadai-concurrent"            % Version.kadai
    )
  )

  lazy val s3 = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-s3"       % Version.aws_sdk
    )
  )

  lazy val dynamodb = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-dynamodb" % Version.aws_sdk
    )
  )

  lazy val sqs = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-sqs"      % Version.aws_sdk
    )
  )

  lazy val cloudformation = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-cloudformation"      % Version.aws_sdk
    )
  )

  lazy val swf = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-simpleworkflow"      % Version.aws_sdk
    )
  )

  lazy val rds = Seq(
    libraryDependencies ++= Seq(
    "com.amazonaws"     %  "aws-java-sdk-rds"      % Version.aws_sdk
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
      "org.specs2"          %% "specs2-core"        % Version.specs2     % "test" excludeAll(ExclusionRule(organization ="org.scalaz"))
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
