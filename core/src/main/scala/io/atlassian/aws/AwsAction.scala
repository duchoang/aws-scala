package io.atlassian.aws

import scalaz.{ Monad, Kleisli, Writer, \/ }
import scalaz.syntax.either._
import kadai.{ Invalid, result }

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {
    import result.ResultT

    def apply[R, A](f: R => Attempt[A]): AwsAction[R, A] =
      Kleisli[WriterAttempt, R, A] { r: R => ResultT[Writer[MetaData, ?], A](Writer(MetaData.none, f(r).run)) }

    def withMetaData[R, A](f: R => (MetaData, Attempt[A])): AwsAction[R, A] =
      Kleisli[WriterAttempt, R, A] { r: R => ResultT[Writer[MetaData, ?], A](f(r) match { case (w, a) => Writer(w, a.run) }) }

    def value[R, A](v: => A): AwsAction[R, A] =
      AwsAction { _ => Attempt.ok(v) }

    def ask[R]: AwsAction[R, R] =
      Kleisli.ask[WriterAttempt, R]

    def local[R, A](f: R => R)(fa: AwsAction[R, A]): AwsAction[R, A] =
      Kleisli.local[WriterAttempt, A, R](f)(fa)

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
      import scalaz.syntax.semigroup._
      def recover(f: Invalid => AwsAction[R, A]): AwsAction[R, A] =
        Kleisli[WriterAttempt, R, A] { r: R =>
          val run: WriterAttempt[A] = action.run(r)
          val newWriter: Writer[MetaData, Invalid \/ A] = run.run.mapValue {
            case (metaData, or) =>
              val newAction: AwsAction[R, A] = or.fold(f, ok)
              val newRun1 = newAction.run(r).run
              (metaData |+| newRun1.written, newRun1.value)
          }
          ResultT[Writer[MetaData, ?], A](newWriter)
        }

      def runAction(r: R): Attempt[A] =
        Attempt(action.run(r).run.value)

      def runActionWithMetaData(r: R): (MetaData, Attempt[A]) = {
        val run: WriterAttempt[A] = action.run(r)
        val writer: Writer[MetaData, Invalid \/ A] = run.run
        (writer.written, Attempt(writer.value))
      }

      def :++>(w: MetaData): AwsAction[R, A] =
        Kleisli[WriterAttempt, R, A] { r: R =>
          ResultT[Writer[MetaData, ?], A](action.run(r).run :++> w)
        }

      def :++>>(f: Invalid \/ A => MetaData) =
        Kleisli[WriterAttempt, R, A] { r: R =>
          ResultT[Writer[MetaData, ?], A](action.run(r).run :++>> f)
        }

      def :++>>>(f: A => MetaData) =
        Kleisli[WriterAttempt, R, A] { r: R =>
          ResultT[Writer[MetaData, ?], A](action.run(r).run :++>> { or =>
            or.fold(invalid => MetaData.none, f)
          })
        }

      def handle(f: PartialFunction[Invalid, AwsAction[R, A]]): AwsAction[R, A] =
        recover { f orElse { case i => invalid(i) } }
    }

    implicit def AwsActionMonad[R]: Monad[AwsAction[R, ?]] = Kleisli.kleisliMonadReader[WriterAttempt, R]

    trait Functions[C] {
      import scalaz.syntax.std.option._
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
        AWSRequestIdRetriever.withClient(f)

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
