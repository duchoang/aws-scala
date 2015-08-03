import sbt._
import sbtrelease._
import ReleasePlugin.autoImport._
import ReleaseStateTransformations._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object Release {
  /**
   * RELEASE PROCESS
   */
  lazy val customReleaseSettings =
    Seq(
      releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies,
        inquireVersions,
        runTest,
        setReleaseVersion,
        commitReleaseVersion,
        tagRelease,
        publishSignedArtifacts,
        setNextVersion,
        commitNextVersion,
        pushChanges
      )
      , releaseCrossBuild  := true
      , releaseNextVersion := { ver => Version(ver).map(_.bumpBugfix.asSnapshot.string).getOrElse(versionFormatError) } // bump patch numbers
    ) ++
      customVcsMessages


  /**
   * PUBLICATION
   */
  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val runner = if (st.get(ReleaseKeys.versions) map { _._1 } exists candidate) {
        // a milestone, or RC
        executeAggregateTask(releasePublishArtifactsAction, "Publishing non-signed artifacts")
      } else {
        // a proper release
        executeAggregateTask(publishSigned, "Publishing signed artifacts")
      }
      runner(st)
    },
    check = identity,
    enableCrossBuild = true
  )

  def candidate(version: String) =
   version contains "-"

  /**
   * UTILITIES
   */
  private def executeStepTask(task: TaskKey[_], info: String) = ReleaseStep { st: State =>
    executeTask(task, info)(st)
  }

  private def executeTask(task: TaskKey[_], info: String) = (st: State) => {
    st.log.info(info)
    val extracted = Project.extract(st)
    val ref: ProjectRef = extracted.get(thisProjectRef)
    extracted.runTask(task in ref, st)._1
  }

  private def executeAggregateTask(task: TaskKey[_], info: String) = (st: State) => {
    st.log.info(info)
    val extracted = Project.extract(st)
    val ref: ProjectRef = extracted.get(thisProjectRef)
    extracted.runAggregated(task in ref, st)
  }

  private lazy val customVcsMessages = Seq(
    releaseTagComment    <<= (version in ThisBuild) map { v => "[sbt-release] Releasing %s" format v }
    , releaseCommitMessage <<= (version in ThisBuild) map { v => "[sbt-release] Setting version to %s" format v }
  )
}