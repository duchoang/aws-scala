package io.atlassian.aws

import com.amazonaws.AmazonServiceException
import kadai.Invalid
import kadai.result.ResultT
import scalaz.syntax.either._

import scalaz.{ Catchable, EitherT, Monad, Monoid, Kleisli, Writer }

trait Functions[C, W] {

  implicit def WMonoid: Monoid[W]
  implicit def WAMonad: Monad[WriterAttempt[W, ?]] =
    EitherT.eitherTMonadError[Writer[W, ?], Invalid]
  implicit val ActionMonad = new AwsActionMonad[C, W]
  implicit val ActionCatchable: Catchable[Action] =
    Kleisli.kleisliCatchable[WriterAttempt[W, ?], C](ResultT.CatachableResultT[Writer[W, ?]])

  implicit class ActionOps[A](action: Action[A]) extends AwsActionOps[C, W, A](action)

  def extractRequestIds: Option[HttpHeaders => Option[W]] = None
  def extractRequestIdsFromException: Option[AmazonServiceException => Option[W]] = None

  type Action[A] = AwsAction[C, W, A]

  import ActionMonad.monadSyntax._

  def apply[A](f: C => Attempt[A]): Action[A] =
    ActionMonad.ask >>= {
      f(_).fold(ActionMonad.raiseError, a => ActionMonad.point(a))
    }

  def safe[A](v: => A): Action[A] =
    attempt { Attempt.safe(v) }

  def safe[A](f: C => A): Action[A] =
    apply { c: C => Attempt.safe { f(c) } }

  def value[A](v: => A): Action[A] =
    ActionMonad.point(v)

  def attempt[A](a: Attempt[A]): Action[A] =
    a.fold(ActionMonad.raiseError, _.point)

  def withClient[A](f: C => A): Action[A] =
    AWSRequestIdRetriever.withClient(f)(extractRequestIds, extractRequestIdsFromException)(ActionMonad, WMonoid)

  def ok[A](strict: A): Action[A] =
    value(strict)

  def fail[A](msg: String): Action[A] =
    attempt(Attempt.fail(msg))

  def fail[A](t: Throwable): Action[A] =
    attempt(Attempt.exception(t))

  def fail[A](i: Invalid): Action[A] =
    attempt(Attempt.apply(i.left))
}