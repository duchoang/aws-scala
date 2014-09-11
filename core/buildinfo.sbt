buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](name,
  version,
  scalaVersion,
  sbtVersion,
  BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  },
  GitKeys.gitHeadCommit
)

buildInfoPackage := "io.atlassian.aws"