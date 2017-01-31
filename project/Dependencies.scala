import sbt._
import Keys._

object Dependencies {

  object Version {
    val scalaz        = "7.2.7"
    val scalazStream  = "0.8.6"
    val argonaut      = "6.2-RC2"
    val aws_sdk       = "1.11.32"
    val kadai         = "6.0.0"
    val kadai_log     = "7.0.0"
    val akka          = "2.4.16"
    val specs2        = "3.8.7"
    val scalacheck    = "1.13.4"
    val junit         = "4.12"
    val scodecBits    = "1.1.4"

  }

  lazy val common = Seq(
    libraryDependencies ++= Seq(
      "org.scalaz"        %% "scalaz-core"        % Version.scalaz
    , "org.scalaz"        %% "scalaz-concurrent"  % Version.scalaz
    , "com.amazonaws"     %  "aws-java-sdk-core"  % Version.aws_sdk
    , "io.atlassian"      %% "kadai-core"         % Version.kadai
    )
  )

  lazy val core = Seq(
    libraryDependencies ++= Seq(
      "io.argonaut"       %% "argonaut"           % Version.argonaut
    , "io.atlassian"      %% "kadai-config"       % Version.kadai
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
    , "io.argonaut"       %% "argonaut"           % Version.argonaut
    )
  )

  lazy val sqs = Seq(
    libraryDependencies ++= Seq(
      "com.amazonaws"     %  "aws-java-sdk-sqs"      % Version.aws_sdk
    , "io.argonaut"       %% "argonaut"           % Version.argonaut
    )
  )

  lazy val sns = Seq(
    libraryDependencies ++= Seq(
      "com.amazonaws"     %  "aws-java-sdk-sns"      % Version.aws_sdk
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
    , "io.atlassian"      %% "kadai-logging-json" % Version.kadai_log
    , "io.atlassian"      %% "kadai-concurrent"   % Version.kadai % "test"
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
