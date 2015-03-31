package io.atlassian.aws.swf

import scalaz.Monad
import scalaz.syntax.monad._

trait ActivityTypes {
  sealed trait Result {
    import Result._
    def fold[X](f: (String, String) => X, s: String => X): X =
      this match {
        case FailedActivity(r, d)  => f(r, d)
        case SuccessfulActivity(o) => s(o)
      }
  }

  object Result {
    case class FailedActivity(reason: String, detail: String) extends Result
    case class SuccessfulActivity(output: String) extends Result

    def failed(reason: String, detail: String): Result =
      FailedActivity(reason, detail)

    def success(output: String): Result =
      SuccessfulActivity(output)
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
  }
}
