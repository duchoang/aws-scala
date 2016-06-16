package io.atlassian.aws

import scalaz._
import scalaz.syntax.id._
import scalaz.syntax.monadError._
import kadai.Invalid

case class AwsAction[R, W, A](run: ReaderT[EitherWriter[W, Invalid, ?], R, A]) {
  def map[B](f: A => B)(implicit W: Monoid[W]): AwsAction[R, W, B] =
    Functor[AwsAction[R, W, ?]].map(this)(f)

  def flatMap[B](f: A => AwsAction[R, W, B])(implicit W: Monoid[W]) =
    Monad[AwsAction[R, W, ?]].bind(this)(f)

  def recover(f: Invalid => AwsAction[R, W, A])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    AwsAction.handleError(this)(f)

  def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit W: Monoid[W]): AwsAction[R, W, A] =
    recover {
      f orElse {
        case i => AwsAction.raiseError[R, W, A](i)
      }
    }

  // the original one only logs on the right path. We also want to log on lefts.
  final def :++>>(w: => W)(implicit W: Monoid[W]): AwsAction[R, W, A] =
    MonadListen[AwsAction[R, W, ?], W].tell(w) *> this

  def unsafePerform(r: R): Attempt[A] =
    unsafePerformWithMetaData(r)._2

  def unsafePerformWithMetaData(r: R): (W, Attempt[A]) =
    run.run(r).run.run.unsafePerformSync match {
      case (w, result) => (w, Attempt(result))
    }
}

object AwsAction {

  def apply[R, W: Monoid, A](f: R => Attempt[A]): AwsAction[R, W, A] = {
    ask[R, W] >>= { f(_) |> attempt[R, W, A] }
  }

  def value[R, W: Monoid, A](v: => A): AwsAction[R, W, A] =
    v.point[AwsAction[R, W, ?]]

  def ok[R, W: Monoid, A](strict: A): AwsAction[R, W, A] =
    value(strict)

  def ask[R, W: Monoid]: AwsAction[R, W, R] =
    MonadReader[AwsAction[R, W, ?], R].ask

  def local[R, W: Monoid, A](f: R => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    MonadReader[AwsAction[R, W, ?], R].local(f)(fa)

  def tell[R, W: Monoid](w: W): AwsAction[R, W, Unit] =
    MonadListen[AwsAction[R, W, ?], W].tell(w)

  def safe[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] =
    apply { r => Attempt.safe { f(r) } }

  def withClient[R, W: Monoid, A](f: R => A): AwsAction[R, W, A] = {
    val ME = MonadError[AwsAction[R, W, ?], Invalid]
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
    i.raiseError[AwsAction[R, W, ?], A]

  def handleError[R, W: Monoid, A](a: AwsAction[R, W, A])(f: Invalid => AwsAction[R, W, A]): AwsAction[R, W, A] =
    MonadError[AwsAction[R, W, ?], Invalid].handleError(a)(f)

  def raiseError[R, W: Monoid, A](i: Invalid): AwsAction[R, W, A] =
    i.raiseError[AwsAction[R, W, ?], A]

  implicit def AwsActionMonad[R, W: Monoid]: Monad[AwsAction[R, W, ?]]
    with MonadReader[AwsAction[R, W, ?], R]
    with MonadListen[AwsAction[R, W, ?], W] // MonadTell+
    with MonadPlus[AwsAction[R, W, ?]]
    with MonadError[AwsAction[R, W, ?], Invalid] = new AwsActionMonad[R, W]
}
