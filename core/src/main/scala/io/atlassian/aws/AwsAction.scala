package io.atlassian.aws

import scalaz.Monoid
import kadai.Invalid
import scalaz.syntax.all._

object AwsAction {

  def apply[R, W: Monoid, A](f: R => Attempt[A]): AwsAction[R, W, A] =
    M[R, W] |> { implicit m => m.ask >>= { f(_).fold(m.raiseError, a => m.point(a)) } }

  def value[R, W: Monoid, A](v: => A): AwsAction[R, W, A] =
    M.point(v)

  def ask[R, W: Monoid]: AwsAction[R, W, R] =
    M.ask

  def local[R, W: Monoid, A](f: R => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    M.local(f)(fa)

  def ok[R, W: Monoid, A](strict: A): AwsAction[R, W, A] =
    value(strict)

  def safe[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] =
    apply { r: R => Attempt.safe { f(r) } }

  def withClient[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] =
    M.handleError(safe(f)) {
      AmazonExceptions.transformException andThen invalid[R, W, A]
    }

  def attempt[R, W: Monoid, A](aa: Attempt[A]): AwsAction[R, W, A] = {
    implicit val monad = M[R, W]
    import monad.monadSyntax._
    aa.fold(monad.raiseError, _.point)
  }

  def fail[R, W: Monoid, A](msg: String): AwsAction[R, W, A] =
    invalid(Invalid.Message(msg))

  def invalid[R, W: Monoid, A](i: Invalid): AwsAction[R, W, A] =
    M.raiseError(i)

  private def M[R, W: Monoid] = new AwsActionMonad[R, W]()
}