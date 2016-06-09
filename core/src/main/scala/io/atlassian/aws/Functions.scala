package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import kadai.Invalid
import kadai.result.ResultT

import scalaz.syntax.monadError._
import scalaz.{ Catchable, EitherT, Kleisli, Monad, Monoid }

abstract class Functions[C, W: Monoid] {

  type Action[A] = AwsAction[C, W, A]
  //  private type Error[L, A] = ReaderEitherAction[C, W, L, A]

  def extractRequestIds: Option[HttpHeaders => Option[W]] =
    None

  def extractRequestIdsFromException: Option[AmazonServiceException => Option[W]] =
    None

  def apply[A](f: C => Attempt[A]): Action[A] =
    AwsAction(f)

  def safe[A](a: => A): Action[A] =
    AwsAction.safe(_ => a)

  def safe[A](f: C => A): Action[A] =
    AwsAction.safe(f)

  def value[A](a: A): Action[A] =
    AwsAction.ok(a)

  def ok[A](a: A): Action[A] =
    value(a)

  def attempt[A](a: Attempt[A]): Action[A] =
    a.fold(invalid, value)

  def withClient[A](f: C => A): Action[A] =
    AWSRequestIdRetriever.withClient(f)(extractRequestIds, extractRequestIdsFromException)

  def fail[A](msg: String): Action[A] =
    attempt(Attempt.fail(msg))

  def raise[A](t: Throwable): Action[A] =
    attempt(Attempt.exception(t))

  /** don't use overloaded version, use raise instead */
  def fail[A](t: Throwable): Action[A] =
    raise(t)

  def invalid[A](i: Invalid): Action[A] =
    i.raiseError[AwsAction[C, W, ?], A]

  /** don't use overloaded version, use invalid instead */
  def fail[A](i: Invalid): Action[A] =
    invalid(i)

  //
  // private
  //

  implicit val MonadAction: AwsActionMonad[C, W] =
    AwsActionMonad[C, W]

  type WriterW[A] = WriterF[W, A]
  type ResultWriterW[A] = ResultT[WriterW, A]

  import ResultT._

  implicit val CatchableAction: Catchable[Action] =
    Kleisli.kleisliCatchable[ResultWriterW, C]

  implicit class ActionOps[A](action: Action[A]) extends AwsActionOps[C, W, A](action)

}