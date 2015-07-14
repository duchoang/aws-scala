package io.atlassian.aws

import scalaz.Monoid
import kadai.Invalid

object AwsAction {

  def apply[R, W, A](f: R => Attempt[A])(implicit M: Monoid[W]): AwsAction[R, W, A] = {
    implicit val monad = actionMonad[R, W]
    import monad.monadSyntax._
    monad.ask >>= {
      f(_).fold(monad.raiseError, a => monad.point(a))
    }
  }

  def value[R, W, A](v: => A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    actionMonad.point(v)

  def ask[R, W](implicit M: Monoid[W]): AwsAction[R, W, R] =
    actionMonad.ask

  def local[R, W, A](f: R => R)(fa: AwsAction[R, W, A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
    actionMonad.local(f)(fa)

  def ok[R, W, A](strict: A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    value(strict)

  def safe[R, W, A](f: R => A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    apply { r: R => Attempt.safe { f(r) } }

  def withClient[R, W, A](f: R => A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    actionMonad.handleError(safe(f)) {
      AmazonExceptions.transformException andThen invalid[R, W, A]
    }

  def attempt[R, W, A](aa: Attempt[A])(implicit M: Monoid[W]): AwsAction[R, W, A] = {
    implicit val monad = actionMonad[R, W]
    import monad.monadSyntax._
    aa.fold(monad.raiseError, _.point)
  }

  def fail[R, W, A](msg: String)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    invalid(Invalid.Message(msg))

  def invalid[R, W, A](i: Invalid)(implicit M: Monoid[W]): AwsAction[R, W, A] =
    actionMonad.raiseError(i)

  private def actionMonad[R, W](implicit M: Monoid[W]) = new AwsActionMonad[R, W]()
}