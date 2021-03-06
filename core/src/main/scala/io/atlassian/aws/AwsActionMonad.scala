package io.atlassian.aws

import scalaz._
import kadai.Invalid

import scalaz.syntax.all._

class AwsActionMonad[R, W: Monoid] extends Monad[AwsAction[R, W, ?]]
    with MonadReader[AwsAction[R, W, ?], R]
    with MonadListen[AwsAction[R, W, ?], W] // MonadTell+
    with MonadPlus[AwsAction[R, W, ?]]
    with MonadError[AwsAction[R, W, ?], Invalid] {

  override def ask: AwsAction[R, W, R] =
    AwsAction {
      MonadReader[ResultReaderR, R].ask
    }

  override def local[A](f: (R) => R)(fa: AwsAction[R, W, A]): AwsAction[R, W, A] =
    AwsAction {
      MonadReader[ResultReaderR, R].local(f)(fa.run)
    }

  override def point[A](a: => A): AwsAction[R, W, A] =
    AwsAction {
      a.point[ResultReaderR]
    }

  override def bind[A, B](fa: AwsAction[R, W, A])(f: A => AwsAction[R, W, B]): AwsAction[R, W, B] =
    AwsAction {
      Monad[ResultReaderR].bind(fa.run) { f(_).run }
    }

  override def map[A, B](fa: AwsAction[R, W, A])(f: A => B): AwsAction[R, W, B] =
    AwsAction {
      Monad[ResultReaderR].map(fa.run) { f }
    }

  override def listen[A](fa: AwsAction[R, W, A]): AwsAction[R, W, (A, W)] =
    kleisli(r => MonadListen[ResultWriter[W, ?], W].listen(fa.run.run(r)))

  override def writer[A](w: W, v: A): AwsAction[R, W, A] =
    kleisli(MonadListen[ResultWriterW, W].writer(w, v))

  override def raiseError[A](e: Invalid): AwsAction[R, W, A] =
    kleisli(e.raiseError[EitherWriterW[Invalid, ?], A])

  override def handleError[A](fa: AwsAction[R, W, A])(f: Invalid => AwsAction[R, W, A]): AwsAction[R, W, A] =
    kleisli(r => eitherMonadError.handleError(fa.run.run(r)) { e => f(e).run.run(r) })

  override def empty[A]: AwsAction[R, W, A] =
    mempty[AwsAction[R, W, ?], A]

  override def plus[A](f1: AwsAction[R, W, A], f2: => AwsAction[R, W, A]): AwsAction[R, W, A] =
    f1 <+> f2

  override def tell(w: W): AwsAction[R, W, Unit] =
    kleisli(MonadListen[ResultWriter[W, ?], W].tell(w))

  //
  // private
  //

  // private helper types

  private type WriterW[A] = WriterF[W, A]
  private type EitherWriterW[L, A] = EitherT[WriterW, L, A]
  private type ResultWriter[WW, A] = EitherWriter[WW, Invalid, A]
  private type ResultWriterW[A] = ResultWriter[W, A]
  private type ResultReader[RR, A] = ReaderT[ResultWriterW, RR, A]
  private type ResultReaderR[A] = ResultReader[R, A]

  private implicit val eitherMonadListen: MonadListen[ResultWriter[W, ?], W] =
    EitherT.monadListen[WriterW, W, Invalid] // EitherT.monadListen isn't implicit!

  private implicit val eitherMonadError: MonadError[ResultWriterW, Invalid] =
    EitherT.eitherTMonadError[WriterW, Invalid] //(implicitly[Monad[WriterW]])

  private implicit val eitherMonadPlus: PlusEmpty[ResultWriterW] = // TODO: implicit resolution of PlusEmpty
    EitherT.eitherTMonadPlus[WriterW, Invalid]

  private implicit val kleisliMonadReader: MonadReader[ResultReaderR, R] =
    Kleisli.kleisliMonadReader[ResultWriterW, R](eitherMonadError) // have both monadError and monadListen in scope. Need to choose one

  private implicit val kleisliMonadPlus: PlusEmpty[AwsAction[R, W, ?]] = // TODO: implicit resolution of PlusEmpty
    new PlusEmpty[AwsAction[R, W, ?]] {
      val plusEmpty = Kleisli.kleisliPlusEmpty[ResultWriterW, R]

      def plus[A](a: AwsAction[R, W, A], b: => AwsAction[R, W, A]): AwsAction[R, W, A] =
        AwsAction {
          plusEmpty.plus(a.run, b.run)
        }

      def empty[A]: AwsAction[R, W, A] =
        AwsAction {
          plusEmpty.empty[A]
        }
    }

  private def kleisli[A](f: R => ResultWriterW[A]): AwsAction[R, W, A] =
    AwsAction {
      Kleisli.kleisli(f)
    }

  private def kleisli[A](a: => ResultWriterW[A]): AwsAction[R, W, A] =
    kleisli(_ => a)
}
