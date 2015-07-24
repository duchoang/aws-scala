package io.atlassian.aws

import scalaz.{ Monad, Monoid, Writer, \/ }
import scalaz.syntax.all._
import kadai.Invalid

class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {

  def recover(f: Invalid => AwsAction[R, W, A])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    M.handleError(action)(f)

  def runAction(r: R): Attempt[A] =
    Attempt(action.run(r).run.value)

  def runActionWithMetaData(r: R): (W, Attempt[A]) =
    action.run(r).run |> { w => (w.written, Attempt(w.value)) }

  def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    recover { f orElse { case i => M.raiseError(i) } }

  // the original one only logs on the right path. We also want to log on lefts.
  final def :++>>(w: => W)(implicit W: Monoid[W]): AwsAction[R, W, A] =
    M.bind(M.tell(w)) { _ => action }

  private def M(implicit W: Monoid[W]) = 
    new AwsActionMonad[R, W]()
}