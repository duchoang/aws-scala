package io.atlassian.aws

import scalaz.{ Kleisli, Monad, ReaderT, \/-, -\/ }
import scalaz.syntax.either._
import scalaz.syntax.monad._
import kadai.Invalid

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {
    def apply[R, A](f: R => Attempt[A]): AwsAction[R, A] =
      Kleisli { f }

    def safe[R, A](f: R => Attempt[A]): AwsAction[R, A] =
      this.apply { r => Attempt.safe(f(r)).join }

    def value[R, A](v: => A): AwsAction[R, A] =
      AwsAction { _ => Attempt.ok(v) }

    def ask[R]: AwsAction[R, R] =
      AwsAction(Attempt.ok)

    @deprecated("use ask instead", "2.0")
    def config[R]: AwsAction[R, R] =
      ask

    def ok[R, A](strict: A): AwsAction[R, A] =
      value(strict)

    def withClient[R, A](f: R => A): AwsAction[R, A] =
      AwsAction { r => Attempt.safe { f(r) } }

    def attempt[R, A](a: Attempt[A]): AwsAction[R, A] =
      this.apply { _ => a }

    def fail[R, A](msg: String): AwsAction[R, A] =
      AwsAction { _ => Attempt.fail(msg) }

    implicit def AwsActionMonad[R]: Monad[({ type L[A] = AwsAction[R, A] })#L] =
      new Monad[({ type L[A] = AwsAction[R, A] })#L] {
        def point[A](v: => A) = AwsAction.ok(v)
        def bind[A, B](m: AwsAction[R, A])(f: A => AwsAction[R, B]) = m flatMap f
        override def map[A, B](m: AwsAction[R, A])(f: A => B) = m map f
      }

    implicit class AwsActionOps[R, A](action: AwsAction[R, A]) {
      private[AwsAction] def unsafeRun(r: R): Attempt[A] = ???

      def run(r: R): Attempt[A] =
        Attempt.safe(unsafeRun(r)).join.lift { _.leftMap(AmazonExceptions.transformException) }

      def recover(f: Invalid => Attempt[A]): AwsAction[R, A] =
        AwsAction[R, A] { run(_: R).run.fold(f, Attempt.ok) }

      def handle(f: PartialFunction[Invalid, Attempt[A]]): AwsAction[R, A] =
        recover { f orElse { case i => Attempt(i.left) } }
    }

    trait Functions[C] {
      type Action[A] = AwsAction[C, A]

      def value[A](v: => A): Action[A] =
        AwsAction.value(v)

      def safe[A](v: => A): Action[A] =
        AwsAction.attempt { Attempt.safe(v) }

      def config: Action[C] =
        AwsAction { c => Attempt.ok(c) }

      def ok[A](strict: A): Action[A] =
        value(strict)

      def attempt[A](a: Attempt[A]): Action[A] =
        AwsAction.attempt(a)

      def withClient[A](f: C => A): Action[A] =
        AwsAction.withClient(f)

      def apply[A](run: C => Attempt[A]): Action[A] =
        AwsAction.safe(run)

      def fail[A](msg: String): Action[A] =
        attempt(Attempt.fail(msg))

      def fail[A](t: Throwable): Action[A] =
        attempt(Attempt.exception(t))
    }
  }
}
