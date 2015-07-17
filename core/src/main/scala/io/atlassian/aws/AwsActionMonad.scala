package io.atlassian.aws

import scalaz.{ EitherT, Id, Monad, MonadError, MonadListen, MonadPlus, MonadReader, Monoid, Kleisli, ReaderT, Writer, WriterT }
import kadai.Invalid

class AwsActionMonad[R, W](implicit M: Monoid[W]) extends MonadReader[AwsAction[?, W, ?], R]
    with MonadListen[AwsAction[R, ?, ?], W]
    with MonadError[λ[(α, β) => ReaderT[λ[∂ => EitherT[λ[π => Writer[W, π]], α, ∂]], R, β]], Invalid]
    with MonadPlus[AwsAction[R, W, ?]]
    with Monad[AwsAction[R, W, ?]] {

  private type TypedWriter[A] = Writer[W, A]
  private type TypedWriterAttempt[A] = WriterAttempt[W, A]
  private val wame = EitherT.eitherTMonadError[TypedWriter, Invalid]
  private val waml = EitherT.monadListen[Writer, W, Invalid](WriterT.writerTMonadListen[Id.Id, W])
  private val wamp = EitherT.eitherTMonadPlus[TypedWriter, Invalid](WriterT.writerMonad, Invalid.InvalidMonoid)
  private val kmr = Kleisli.kleisliMonadReader[TypedWriterAttempt, R](wame)
  private val kmp = Kleisli.kleisliMonadPlus[TypedWriterAttempt, R](wamp)

  private def kleisli[A](f: R => TypedWriterAttempt[A]): AwsAction[R, W, A] =
    Kleisli.kleisli[TypedWriterAttempt, R, A](f)

  override def ask: AwsAction[R, W, R] =
    kmr.ask

  override def local[A](f: (R) => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    kmr.local(f)(fa)

  override def point[A](a: => A): AwsAction[R, W, A] =
    kmr.point(a)

  override def bind[A, B](fa: AwsAction[R, W, A])(f: (A) => AwsAction[R, W, B]): AwsAction[R, W, B] =
    kmr.bind(fa)(f)

  override def listen[A](fa: AwsAction[R, W, A]): AwsAction[R, W, (A, W)] =
    kleisli(r => waml.listen(fa.run(r)))

  override def writer[A](w: W, v: A): AwsAction[R, W, A] =
    kleisli(_ => waml.writer(w, v))

  override def raiseError[A](e: Invalid): AwsAction[R, W, A] =
    kleisli(_ => wame.raiseError(e))

  override def handleError[A](fa: AwsAction[R, W, A])(f: Invalid => AwsAction[R, W, A]): AwsAction[R, W, A] =
    kleisli(r => wame.handleError(fa.run(r)) { e => f(e).run(r) })

  override def empty[A]: AwsAction[R, W, A] =
    kmp.empty

  override def plus[A](f1: AwsAction[R, W, A], f2: => AwsAction[R, W, A]): AwsAction[R, W, A] =
    kmp.plus(f1, f2)
}
