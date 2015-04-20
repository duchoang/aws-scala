import sbt._
import sbt.Process
import sbtrelease.ReleasePlugin.{ReleaseKeys, releaseSettings}
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease._
import sbt.Keys._

object Release {

  private val mavenCommand = "mvn3"

  private lazy val releaseProcess = Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,

    setReleaseVersion,
    stageFilesWithModifiedVersions,
    commitReleaseVersion,

    tagRelease,

    publishArtifacts,

    setNextVersion,
    stageFilesWithModifiedVersions,
    commitNextVersion,

    pushChanges
  )

  /* --- Release Steps --- */

  private def vcs(st: State): Vcs = {
    import Utilities._
    st.extract.get(versionControlSystem).getOrElse(sys.error("Aborting release. Working directory is not a repository of a recognized VCS."))
  }

  private lazy val stageFilesWithModifiedVersions = ReleaseStep { st =>
    vcs(st).cmd("add", "-u") !! st.log
    st
  }

  private lazy val customVcsMessages = Seq(
    tagComment    <<= (version in ThisBuild) map { v => "[sbt-release] Releasing %s" format v }
    , commitMessage <<= (version in ThisBuild) map { v => "[sbt-release] Setting version to %s" format v }
  )

  lazy val customReleaseSettings =
    releaseSettings ++
    Seq(
      ReleaseKeys.releaseProcess := Release.releaseProcess
      , ReleaseKeys.crossBuild := true
      , nextVersion    := { ver => Version(ver).map(_.bumpBugfix.asSnapshot.string).getOrElse(versionFormatError) } // bump patch numbers
    ) ++
    customVcsMessages
}