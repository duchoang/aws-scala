package io.atlassian.aws

import scalaz.{MonadError, MonadListen, Monad, Monoid}
import scalaz.syntax.id._
import scalaz.syntax.monad._
import kadai.Invalid

class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {

  def recover(f: Invalid => AwsAction[R, W, A])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    MonadError[AwsAction[R, W, ?], Invalid].handleError(action)(f)

  def runAction(r: R): Attempt[A] =
    runActionWithMetaData(r)._2

  def runActionWithMetaData(r: R): (W, Attempt[A]) =
    action.run.run(r).run.run.unsafePerformSync match {
      case (w, result) => (w, Attempt(result))
    }

  def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    recover {
      f orElse {
        case i => MonadError[AwsAction[R, W, ?], Invalid].raiseError(i)
      }
    }

  // the original one only logs on the right path. We also want to log on lefts.
  final def :++>>(w: => W)(implicit W: Monoid[W]): AwsAction[R, W, A] =
    MonadListen[AwsAction[R, W, ?], W].tell(w) >> action
}
