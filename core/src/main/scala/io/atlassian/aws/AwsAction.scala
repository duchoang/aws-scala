package io.atlassian.aws

import scalaz.{ Kleisli, Monad, ReaderT, \/-, -\/ }
import scalaz.syntax.either._
import scalaz.syntax.monad._
import kadai.Invalid

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {
    def apply[R, A](f: R => Attempt[A]): AwsAction[R, A] =
      Kleisli { f }

    def value[R, A](v: => A): AwsAction[R, A] =
      AwsAction { _ => Attempt.ok(v) }

    def ask[R]: AwsAction[R, R] =
      AwsAction(Attempt.ok)

    def ok[R, A](strict: A): AwsAction[R, A] =
      value(strict)

    def withClient[R, A](f: R => A): AwsAction[R, A] =
      AwsAction { r: R => Attempt.safe { f(r) } }.recover {
        AmazonExceptions.transformException andThen invalid[R, A]
      }

    def attempt[R, A](a: Attempt[A]): AwsAction[R, A] =
      this.apply { _ => a }

    def fail[R, A](msg: String): AwsAction[R, A] =
      AwsAction { _ => Attempt.fail(msg) }

    def invalid[R, A](i: Invalid): AwsAction[R, A] =
      AwsAction { _ => Attempt(i.left) }

    implicit class AwsActionOps[R, A](action: AwsAction[R, A]) {
      def recover(f: Invalid => AwsAction[R, A]): AwsAction[R, A] =
        AwsAction[R, A] { r => action.run(r).run.fold(f, ok[R, A](_)).run(r) }

      def handle(f: PartialFunction[Invalid, AwsAction[R, A]]): AwsAction[R, A] =
        recover { f orElse { case i => invalid(i) } }
    }

    trait Functions[C] {
      type Action[A] = AwsAction[C, A]

      def apply[A](run: C => Attempt[A]): Action[A] =
        AwsAction(run)

      def safe[A](v: => A): Action[A] =
        AwsAction.attempt { Attempt.safe(v) }

      def value[A](v: => A): Action[A] =
        AwsAction.value(v)

      def attempt[A](a: Attempt[A]): Action[A] =
        AwsAction.attempt(a)

      def withClient[A](f: C => A): Action[A] =
        AwsAction.withClient(f)

      def ok[A](strict: A): Action[A] =
        value(strict)

      def fail[A](msg: String): Action[A] =
        attempt(Attempt.fail(msg))

      def fail[A](t: Throwable): Action[A] =
        attempt(Attempt.exception(t))
    }
  }
}
