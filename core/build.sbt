name := "aws-scala-core"

libraryDependencies ++= Seq(
  "org.typelevel"            %% "scalaz-specs2"             % "0.4.0"        % "test",
  "org.scalaz"               %% "scalaz-scalacheck-binding" % Dependencies.Version.scalaz % "test"
)
