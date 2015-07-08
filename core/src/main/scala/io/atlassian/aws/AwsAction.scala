package io.atlassian.aws

import com.amazonaws.AmazonServiceException

import scalaz.{ Monad, Monoid, Kleisli, Writer, \/ }
import scalaz.syntax.either._
import kadai.{ Invalid, result }

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {
    import result.ResultT
    private implicit def writerAttemptMonad[W](implicit M: Monoid[W]) = WriterAttemptMonad

    def apply[R, W, A](f: R => Attempt[A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
      Kleisli[WriterAttempt[W, ?], R, A] { r: R => ResultT[Writer[W, ?], A](Writer(M.zero, f(r).run)) }

    def withMetaData[R, W, A](f: R => (W, Attempt[A])): AwsAction[R, W, A] =
      Kleisli[WriterAttempt[W, ?], R, A] { r: R => ResultT[Writer[W, ?], A](f(r) match { case (w, a) => Writer(w, a.run) }) }

    def value[R, W, A](v: => A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      AwsAction { _ => Attempt.ok(v) }

    def ask[R, W](implicit M: Monoid[W]): AwsAction[R, W, R] =
      Kleisli.ask[WriterAttempt[W, ?], R]

    def local[R, W, A](f: R => R)(fa: AwsAction[R, W, A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
      Kleisli.local[WriterAttempt[W, ?], A, R](f)(fa)

    def ok[R, W, A](strict: A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      value(strict)

    def withClient[R, W, A](f: R => A)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      AwsAction { r: R => Attempt.safe { f(r) } }.recover {
        AmazonExceptions.transformException andThen invalid[R, W, A]
      }

    def attempt[R, W, A](a: Attempt[A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
      this.apply { _ => a }

    def fail[R, W, A](msg: String)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      AwsAction { _ => Attempt.fail(msg) }

    def invalid[R, W, A](i: Invalid)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      AwsAction { _ => Attempt(i.left) }

    implicit class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {
      import scalaz.syntax.semigroup._
      def recover(f: Invalid => AwsAction[R, W, A])(implicit M: Monoid[W]): AwsAction[R, W, A] =
        Kleisli[WriterAttempt[W, ?], R, A] { r: R =>
          val run: WriterAttempt[W, A] = action.run(r)
          val newWriter: Writer[W, Invalid \/ A] = run.run.mapValue {
            case (metaData, or) =>
              val newAction: AwsAction[R, W, A] = or.fold(f, { a => ok(a) })
              val newRun1 = newAction.run(r).run
              (metaData |+| newRun1.written, newRun1.value)
          }
          ResultT[Writer[W, ?], A](newWriter)
        }

      def runAction(r: R)(implicit M: Monoid[W]): Attempt[A] =
        Attempt(action.run(r).run.value)

      def runActionWithMetaData(r: R): (W, Attempt[A]) = {
        val run: WriterAttempt[W, A] = action.run(r)
        val writer: Writer[W, Invalid \/ A] = run.run
        (writer.written, Attempt(writer.value))
      }

      def :++>(w: W)(implicit M: Monoid[W]): AwsAction[R, W, A] =
        Kleisli[WriterAttempt[W, ?], R, A] { r: R =>
          ResultT[Writer[W, ?], A](action.run(r).run :++> w)
        }

      def :++>>(f: Invalid \/ A => W)(implicit M: Monoid[W]) =
        Kleisli[WriterAttempt[W, ?], R, A] { r: R =>
          ResultT[Writer[W, ?], A](action.run(r).run :++>> f)
        }

      def :++>>>(f: A => W)(implicit M: Monoid[W]) =
        Kleisli[WriterAttempt[W, ?], R, A] { r: R =>
          ResultT[Writer[W, ?], A](action.run(r).run :++>> { or =>
            or.fold(invalid => M.zero, f)
          })
        }

      def handle(f: PartialFunction[Invalid, AwsAction[R, W, A]])(implicit M: Monoid[W]): AwsAction[R, W, A] =
        recover { f orElse { case i => invalid(i) } }
    }

    private implicit def AwsActionMonad[R, W](implicit M: Monoid[W]): Monad[AwsAction[R, W, ?]] = Kleisli.kleisliMonadReader[WriterAttempt[W, ?], R](WriterAttemptMonad)

    trait Functions[C, W] {
      import scalaz.syntax.std.option._

      implicit def WMonoid: Monoid[W]
      implicit def WAMonad: Monad[WriterAttempt[W, ?]] = WriterAttemptMonad
      implicit def ActionMonad: Monad[Action] = AwsActionMonad[C, W]

      implicit class ActionOps[A](action: AwsAction[C, W, A]) extends AwsActionOps[C, W, A](action)

      def extractRequestIds: Option[HttpHeaders => Option[W]] = None
      def extractRequestIdsFromException: Option[AmazonServiceException => Option[W]] = None

      type Action[A] = AwsAction[C, W, A]

      def apply[A](run: C => Attempt[A]): Action[A] =
        AwsAction(run)

      def safe[A](v: => A): Action[A] =
        AwsAction.attempt { Attempt.safe(v) }

      def value[A](v: => A): Action[A] =
        AwsAction.value(v)

      def attempt[A](a: Attempt[A]): Action[A] =
        AwsAction.attempt(a)

      def withClient[A](f: C => A): Action[A] =
        AWSRequestIdRetriever.withClient(f)(extractRequestIds, extractRequestIdsFromException)

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
