buildInfoKeys := Seq[BuildInfoKey](name,
  version,
  scalaVersion,
  sbtVersion,
  GitKeys.gitHeadCommit
)

buildInfoPackage := "io.atlassian.aws"