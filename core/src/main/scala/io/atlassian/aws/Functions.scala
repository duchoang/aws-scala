package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import kadai.Invalid
import kadai.result.ResultT
import scalaz.concurrent.Future
import scalaz.syntax.all._

import scalaz.{ Catchable, EitherT, Monad, Monoid, Kleisli, WriterT }

abstract class Functions[C, W: Monoid] {

  type Action[A] = AwsAction[C, W, A]

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
    MonadAction.raiseError(i)

  /** don't use overloaded version, use invalid instead */
  def fail[A](i: Invalid): Action[A] =
    invalid(i)

  //
  // private
  //

  implicit val MonadAction =
    new AwsActionMonad[C, W]

  implicit val MonadWriterAttempt: Monad[ResultWriter[W, ?]] =
    EitherT.eitherTMonadError[WriterT[Future, W, ?], Invalid]

  implicit val CatchableAction: Catchable[Action] =
    Kleisli.kleisliCatchable[ResultWriter[W, ?], C](ResultT.CatachableResultT[WriterT[Future, W, ?]])

  implicit class ActionOps[A](action: Action[A]) extends AwsActionOps[C, W, A](action)

}