package io.atlassian.aws.swf

import scalaz.Monad
import scalaz.syntax.monad._

trait ActivityTypes {
  sealed trait Result {
    import Result._
    def fold[X](fail: (String, String) => X, success: String => X, empty: => X): X =
      this match {
        case FailedActivity(r, d)  => fail(r, d)
        case SuccessfulActivity(o) => success(o)
        case Empty => empty
      }
  }

  object Result {
    case class FailedActivity(reason: String, detail: String) extends Result
    case class SuccessfulActivity(output: String) extends Result
    object Empty extends Result

    /**
      * If the activity returns this result, AWS-Scala will report to SWF with an ActivityTaskFailed
      * using the same reason and detail (note: only the first 256 chars of reason will be used).
      */
    def failed(reason: String, detail: String): Result =
      FailedActivity(reason, detail)

    /**
      * If the activity returns this result, AWS-Scala will report to SWF with en ActivityTaskCompleted
      * using the same output.
      */
    def success(output: String): Result =
      SuccessfulActivity(output)

    /**
      * Use this result if you don't want AWS-Scala to report activity completion (or success) back
      * to SWF. For example, use this if your activity function spawns a long-running task in another thread pool,
      * and you will take care of reporting back to SWF when that task completes.
      */
    def empty: Result = Empty
  }

  type ActivityResult[F[_]] = F[Result]
  type ActivityFunction[F[_]] = ActivityInstance => ActivityResult[F]

  object ActivityResult {
    def apply[F[_]: Monad](f: Result): ActivityResult[F] =
      f.point[F]

    def failed[F[_]: Monad](reason: String, detail: String): ActivityResult[F] =
      Result.failed(reason, detail).point[F]

    def success[F[_]: Monad](output: String): ActivityResult[F] =
      Result.success(output).point[F]

    def empty[F[_]: Monad]: ActivityResult[F] =
      Result.empty.point[F]
  }
}
