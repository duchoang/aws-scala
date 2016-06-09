package io.atlassian.aws

import scalaz.{ EitherT, Monad, MonadError, MonadListen, MonadPlus, MonadReader, Monoid, Kleisli, Plus, PlusEmpty, ReaderT }
import kadai.Invalid

import scalaz.syntax.all._

class AwsActionMonad[R, W: Monoid] extends Monad[AwsAction[R, W, ?]]
    with MonadReader[AwsAction[R, W, ?], R]
    with MonadListen[AwsAction[R, W, ?], W] // MonadTell+
    with MonadPlus[AwsAction[R, W, ?]]
    with MonadError[AwsAction[R, W, ?], Invalid] {

  override def ask: AwsAction[R, W, R] =
    MonadReader[Action, R].ask

  override def local[A](f: (R) => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    MonadReader[Action, R].local(f)(fa)

  override def point[A](a: => A): AwsAction[R, W, A] =
    a.point[Action]

  override def bind[A, B](fa: AwsAction[R, W, A])(f: A => AwsAction[R, W, B]): AwsAction[R, W, B] =
    Monad[Action].bind(fa)(f)

  override def listen[A](fa: AwsAction[R, W, A]): AwsAction[R, W, (A, W)] =
    kleisli(r => MonadListen[ResultWriter[W, ?], W].listen(fa.run(r)))

  override def writer[A](w: W, v: A): AwsAction[R, W, A] =
    kleisli(MonadListen[ResultWriter[W, ?], W].writer(w, v))

  override def raiseError[A](e: Invalid): AwsAction[R, W, A] =
    kleisli(e.raiseError[EitherWriterW[Invalid, ?], A])

  override def handleError[A](fa: AwsAction[R, W, A])(f: Invalid => AwsAction[R, W, A]): AwsAction[R, W, A] =
    kleisli(r => eitherMonadError.handleError(fa.run(r)) { e => f(e).run(r) })

  override def empty[A]: AwsAction[R, W, A] =
    mempty[Action, A]

  override def plus[A](f1: AwsAction[R, W, A], f2: => AwsAction[R, W, A]): AwsAction[R, W, A] =
    f1 <+> f2

  override def tell(w: W): AwsAction[R, W, Unit] =
    kleisli(MonadListen[ResultWriter[W, ?], W].tell(w))

  //
  // private
  //

  // private helper types
  private type Action[A] = AwsAction[R, W, A]

  private type WriterW[A] = WriterF[W, A]
  private type EitherWriterW[L, A] = EitherT[WriterW, L, A]
  private type ResultWriter[X, A] = EitherWriter[X, Invalid, A]
  private type ResultWriterW[A] = ResultWriter[W, A]

  private implicit val eitherMonadListen: MonadListen[ResultWriter[W, ?], W] =
    EitherT.monadListen[WriterW, W, Invalid] // EitherT.monadListen isn't implicit!

  private implicit val eitherMonadError: MonadError[ResultWriterW, Invalid] =
    EitherT.eitherTMonadError[WriterW, Invalid] //(implicitly[Monad[WriterW]])

  private implicit val eitherMonadPlus: PlusEmpty[ResultWriterW] = // TODO: implicit resolution of PlusEmpty
    EitherT.eitherTMonadPlus[WriterW, Invalid]

  private implicit val kleisliMonadReader: MonadReader[Action, R] =
    Kleisli.kleisliMonadReader[ResultWriterW, R](eitherMonadError) // have both monadError and monadListen in scope. Need to choose one

  private implicit val kleisliMonadPlus: PlusEmpty[Action] = // TODO: implicit resolution of PlusEmpty
    Kleisli.kleisliPlusEmpty[ResultWriterW, R]

  private def kleisli[A](f: R => ResultWriterW[A]): AwsAction[R, W, A] =
    Kleisli.kleisli(f)

  private def kleisli[A](a: => ResultWriterW[A]): AwsAction[R, W, A] =
    kleisli(_ => a)
}

object AwsActionMonad {
  def apply[R, W: Monoid]: AwsActionMonad[R, W] = new AwsActionMonad[R, W]
}