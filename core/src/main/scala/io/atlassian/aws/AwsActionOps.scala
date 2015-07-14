package io.atlassian.aws

import scalaz.{ Monoid, Writer, \/ }
import kadai.Invalid

class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {

  private def actionMonad(implicit M: Monoid[W]) = new AwsActionMonad[R, W]()

  def recover(f: Invalid => AwsAction[R, W, A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
    actionMonad.handleError(action)(f)

  def runAction(r: R)(implicit M: Monoid[W]): Attempt[A] =
    Attempt(action.run(r).run.value)

  def runActionWithMetaData(r: R)(implicit M: Monoid[W]): (W, Attempt[A]) = {
    val run: WriterAttempt[W, A] = action.run(r)
    val writer: Writer[W, Invalid \/ A] = run.run
    (writer.written, Attempt(writer.value))
  }

  def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit M: Monoid[W]): AwsAction[R, W, A] =
    recover { f orElse { case i => AwsAction.invalid(i) } }

  // the original one only logs on the right path. We also want to log on lefts.
  final def :++>>(w: => W)(implicit M: Monoid[W]): AwsAction[R, W, A] = {
    val monad = actionMonad
    monad.bind(monad.tell(w))(_ => action)
  }
}