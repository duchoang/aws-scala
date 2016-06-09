package io.atlassian.aws

import scalaz.{ Monad, Monoid }
import scalaz.syntax.id._
import scalaz.syntax.monad._
import kadai.Invalid

class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {

  def recover(f: Invalid => AwsAction[R, W, A])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    M.handleError(action)(f)

  def runAction(r: R): Attempt[A] =
    runActionWithMetaData(r)._2

  def runActionWithMetaData(r: R): (W, Attempt[A]) =
    action.run(r).run.run.unsafePerformSync match {
      case (w, result) => (w, Attempt(result))
    }

  def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    recover { f orElse { case i => M.raiseError(i) } }

  // the original one only logs on the right path. We also want to log on lefts.
  final def :++>>(w: => W)(implicit W: Monoid[W]): AwsAction[R, W, A] =
    M.tell(w) >> action

  private implicit def monad(implicit W: Monoid[W]): Monad[AwsAction[R, W, ?]] =
    M

  private def M(implicit W: Monoid[W]) =
    AwsActionMonad[R, W]
}