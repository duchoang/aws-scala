import com.typesafe.sbt.pgp.PgpKeys._
import sbt._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease._
import sbt.Keys._
import com.typesafe.sbt.pgp.PgpKeys._

object Release {
  /**
   * RELEASE PROCESS
   */
  lazy val customReleaseSettings =
    ReleasePlugin.releaseSettings ++ Seq(
      releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies,
        inquireVersions,
        runTest,
        setReleaseVersion,
        commitReleaseVersion,
        tagRelease,
        ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true),
        setNextVersion,
        commitNextVersion,
        pushChanges
      )
      , crossBuild := true
      , nextVersion    := { ver => Version(ver).map(_.bumpBugfix.asSnapshot.string).getOrElse(versionFormatError) } // bump patch numbers
    ) ++
      customVcsMessages


  /**
   * PUBLICATION
   */
  lazy val publishSignedArtifacts = executeAggregateTask(publishSigned, "Publishing signed artifacts")

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
    tagComment    <<= (version in ThisBuild) map { v => "[sbt-release] Releasing %s" format v }
    , commitMessage <<= (version in ThisBuild) map { v => "[sbt-release] Setting version to %s" format v }
  )
}