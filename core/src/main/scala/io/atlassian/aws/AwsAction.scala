package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import kadai.Invalid.Message

import scalaz.{ Catchable, EitherT, Id, Monad, MonadError, MonadListen, MonadPlus, MonadReader, Monoid, Kleisli, ReaderT, Writer, WriterT, \/ }
import scalaz.syntax.either._
import kadai.{ Invalid, result }

trait AwsActionTypes { // https://issues.scala-lang.org/browse/SI-9025
  object AwsAction {
    import result.ResultT

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
      safe(f) recover {
        AmazonExceptions.transformException andThen invalid[R, W, A]
      }

    def attempt[R, W, A](aa: Attempt[A])(implicit M: Monoid[W]): AwsAction[R, W, A] = {
      implicit val monad = actionMonad[R, W]
      import monad.monadSyntax._
      aa.fold(monad.raiseError, _.point)
    }

    def fail[R, W, A](msg: String)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      invalid(Message(msg))

    def invalid[R, W, A](i: Invalid)(implicit M: Monoid[W]): AwsAction[R, W, A] =
      actionMonad.raiseError(i)

    private def actionMonad[R, W](implicit M: Monoid[W]) = new AwsActionMonad[R, W]()

    implicit class AwsActionOps[R, W, A](action: AwsAction[R, W, A]) {

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
        recover { f orElse { case i => invalid(i) } }

      // the original one only logs on the right path. We also want to log on lefts.
      final def :++>>(w: => W)(implicit M: Monoid[W]): AwsAction[R, W, A] = {
        val monad = actionMonad[R, W]
        monad.bind(monad.tell(w))(_ => action)
      }

    }

    private type WriterAttemptWithLeftSide[W, E, A] = EitherT[Writer[W, ?], E, A]

    class AwsActionMonad[R, W](implicit M: Monoid[W]) extends MonadReader[({ type λ[α, β] = AwsAction[α, W, β] })#λ, R] with MonadListen[({ type λ[α, β] = AwsAction[R, α, β] })#λ, W] with MonadError[({ type λ[α, β] = ReaderT[WriterAttemptWithLeftSide[W, α, ?], R, β] })#λ, Invalid] with MonadPlus[({ type λ[β] = AwsAction[R, W, β] })#λ] with Monad[({ type λ[β] = AwsAction[R, W, β] })#λ] {

      private type TypedWriter[A] = Writer[W, A]
      private type TypedWriterAttempt[A] = WriterAttempt[W, A]
      private val wame = WriterAttemptMonadError[W]
      private val waml = EitherT.monadListen[Writer, W, Invalid](WriterT.writerTMonadListen[Id.Id, W])
      private val wamp = EitherT.eitherTMonadPlus[TypedWriter, Invalid](WriterT.writerMonad, Invalid.InvalidMonoid)

      private def kleisli[A](f: R => TypedWriterAttempt[A]): AwsAction[R, W, A] =
        Kleisli.kleisli[TypedWriterAttempt, R, A](f)

      override def ask: AwsAction[R, W, R] =
        kleisli(r => wame.point(r))

      override def local[A](f: (R) => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
        kleisli(r => fa.run(f(r)))

      override def point[A](a: => A): AwsAction[R, W, A] =
        kleisli(_ => wame.point(a))

      override def bind[A, B](fa: AwsAction[R, W, A])(f: (A) => AwsAction[R, W, B]): AwsAction[R, W, B] =
        kleisli(r => wame.bind(fa.run(r)) { a => f(a).run(r) })

      override def listen[A](ma: AwsAction[R, W, A]): AwsAction[R, W, (A, W)] =
        kleisli(r => waml.listen(ma.run(r)))

      override def writer[A](w: W, v: A): AwsAction[R, W, A] =
        kleisli(r => waml.writer(w, v))

      override def raiseError[A](e: Invalid): AwsAction[R, W, A] =
        kleisli(r => wame.raiseError(e))

      override def handleError[A](fa: AwsAction[R, W, A])(f: Invalid => AwsAction[R, W, A]): AwsAction[R, W, A] =
        kleisli(r => wame.handleError(fa.run(r)) { e => f(e).run(r) })

      override def empty[A]: AwsAction[R, W, A] =
        kleisli(R => wamp.empty[A])

      override def plus[A](f1: AwsAction[R, W, A], f2: => AwsAction[R, W, A]): AwsAction[R, W, A] =
        kleisli(r => wamp.plus[A](f1.run(r), f2.run(r)))
    }

    trait Functions[C, W] {
      import scalaz.syntax.std.option._

      implicit def WMonoid: Monoid[W]
      implicit def WAMonad: Monad[WriterAttempt[W, ?]] =
        WriterAttemptMonadError[W]
      implicit def ActionMonad: Monad[Action] =
        new AwsActionMonad[C, W]
      implicit def ActionCatchable: Catchable[Action] =
        Kleisli.kleisliCatchable[WriterAttempt[W, ?], C](ResultT.CatachableResultT[Writer[W, ?]])

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
