package io.atlassian.aws

import scalaz.{Catchable, Monad, Kleisli, ReaderT, WriterT}
import scalaz.syntax.either._
import scalaz.syntax.catchable._
import scalaz.Kleisli._
import kadai.Invalid
import Attempt._

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {

    def apply[R, A](f: R => Attempt[A]): AwsAction[R, A] =
      WriterT.put[ReaderT[Attempt, R, ?], MetaData, A](Kleisli { f })(MetaData.none)

    def value[R, A](v: => A): AwsAction[R, A] =
      AwsAction { _ => Attempt.ok(v) }

    def ask[R]: AwsAction[R, R] =
      WriterT.put[ReaderT[Attempt, R, ?], MetaData, R](Kleisli.ask)(MetaData.none)

    def local[R, A](f: R => R)(fa: AwsAction[R, A]): AwsAction[R, A] =
      WriterT.writerT[ReaderT[Attempt, R, ?], MetaData, A](fa.run.local(f))

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
      import MetaData._
      def recover(f: Invalid => AwsAction[R, A]): AwsAction[R, A] =
        WriterT[ReaderT[Attempt, R, ?], MetaData, A](Kleisli {
          r => action.run.run(r).run.fold(f, {case (md, a) => ok[R, A](a) :++> md}).run(r)
        })

      def runAction(r: R): Attempt[A] = action.value.run(r)

      def handle(f: PartialFunction[Invalid, AwsAction[R, A]]): AwsAction[R, A] =
        recover { f orElse { case i => invalid(i) } }
    }

    implicit def AwsActionMonad[R]: Monad[AwsAction[R, ?]] = WriterT.writerTMonad[ReaderT[Attempt, R, ?], MetaData]

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

      def fail[A](i: Invalid): Action[A] =
        attempt(Attempt.apply(i.left))
    }
  }
}
