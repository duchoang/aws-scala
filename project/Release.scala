import sbt._
import sbt.Process
import sbtrelease.ReleasePlugin.{ReleaseKeys, releaseSettings}
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease._
import sbt.Keys._

object Release {
  /* --- Release Steps --- */
  private lazy val customVcsMessages = Seq(
    tagComment    <<= (version in ThisBuild) map { v => "[sbt-release] Releasing %s" format v }
    , commitMessage <<= (version in ThisBuild) map { v => "[sbt-release] Setting version to %s" format v }
  )

  lazy val customReleaseSettings =
    releaseSettings ++
      Seq(
        ReleaseKeys.crossBuild := true
        , nextVersion    := { ver => Version(ver).map(_.bumpBugfix.asSnapshot.string).getOrElse(versionFormatError) } // bump patch numbers
      ) ++
      customVcsMessages
}