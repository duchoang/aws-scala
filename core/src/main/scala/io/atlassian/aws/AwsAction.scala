package io.atlassian.aws

import scalaz.{ Monoid, MonadReader, MonadError }
import kadai.Invalid
import scalaz.syntax.id._
import scalaz.syntax.monad._
import scalaz.syntax.monadError._

object AwsAction {

  private implicit def M[R, W: Monoid] =
    AwsActionMonad[R, W]

  def apply[R, W: Monoid, A](f: R => Attempt[A]): AwsAction[R, W, A] = {
    ask[R, W] >>= { f(_) |> attempt[R, W, A] }
  }

  def value[R, W: Monoid, A](v: => A): AwsAction[R, W, A] =
    v.point[AwsAction[R, W, ?]]

  def ask[R, W: Monoid]: AwsAction[R, W, R] =
    MonadReader[AwsAction[?, W, ?], R].ask

  def local[R, W: Monoid, A](f: R => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    MonadReader[AwsAction[?, W, ?], R].local(f)(fa)

  def ok[R, W: Monoid, A](strict: A): AwsAction[R, W, A] =
    value(strict)

  def safe[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] =
    apply { r => Attempt.safe { f(r) } }

  def withClient[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] = {
    val ME = MonadError[ReaderEitherAction[R, W, ?, ?], Invalid]
    import ME.monadErrorSyntax._
    safe(f) handleError {
      AmazonExceptions.transformInvalid andThen invalid[R, W, A]
    }
  }

  def attempt[R, W: Monoid, A](a: Attempt[A]): AwsAction[R, W, A] = 
    a.fold(invalid[R, W, A], ok[R, W, A])

  def fail[R, W: Monoid, A](msg: String): AwsAction[R, W, A] =
    invalid(Invalid.Message(msg))

  def invalid[R, W: Monoid, A](i: Invalid): AwsAction[R, W, A] =
    i.raiseError[ReaderEitherAction[R, W, ?, ?], A]
}
