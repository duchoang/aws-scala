package io.atlassian.aws

import scalaz.{ Monad, \/-, -\/ }
import scalaz.syntax.either._
import scalaz.syntax.monad._
import kadai.Invalid

case class AwsAction[R, A](private val unsafeRun: R => Attempt[A]) {
  def map[B](f: A => B): AwsAction[R, B] =
    AwsAction[R, B] {
      run(_).map(f)
    }

  def flatMap[B](f: A => AwsAction[R, B]): AwsAction[R, B] =
    AwsAction[R, B] {
      r => run(r).flatMap { a => f(a).run(r) }
    }

  def run(r: R): Attempt[A] =
    Attempt.safe(unsafeRun(r)).join.lift { _.leftMap(AmazonExceptions.transformException) }

  def recover(f: Invalid => Attempt[A]): AwsAction[R, A] =
    AwsAction[R, A] {
      run(_).run.fold(f, Attempt.ok)
    }

  def handle(f: PartialFunction[Invalid, Attempt[A]]): AwsAction[R, A] =
    recover { f orElse { case i => Attempt(i.left) } }
}

object AwsAction {
  def value[R, A](v: => A): AwsAction[R, A] =
    AwsAction(_ => Attempt.ok(v))

  def config[R]: AwsAction[R, R] =
    AwsAction(Attempt.ok)

  def ok[R, A](strict: A): AwsAction[R, A] =
    value(strict)

  def withClient[R, A](f: R => A): AwsAction[R, A] =
    config.map(f)

  def fail[R, A](msg: String): AwsAction[R, A] =
    AwsAction { _ => Attempt.fail(msg) }

  implicit def AwsActionMonad[R]: Monad[({ type L[A] = AwsAction[R, A] })#L] =
    new Monad[({ type L[A] = AwsAction[R, A] })#L] {
      def point[A](v: => A) = AwsAction.ok(v)
      def bind[A, B](m: AwsAction[R, A])(f: A => AwsAction[R, B]) = m flatMap f
      override def map[A, B](m: AwsAction[R, A])(f: A => B) = m map f
    }
}
