package io.atlassian

import kadai.Invalid
import kadai.result.ResultT
import scalaz._
import scalaz.std.list.listMonoid

package object aws extends AwsActionTypes with Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt

  implicit class AwsTaggedOps[A, T](val a: A @@ T) extends AnyVal {
    def unwrap: A = Tag.unwrap(a)

    override def toString =
      unwrap.toString
  }

  type WriterAttempt[W, A] = ResultT[Writer[W, ?], A]
  def WriterAttemptMonad[R, W](implicit M: Monoid[W]): Monad[WriterAttempt[W, ?]] = EitherT.eitherTMonad[Writer[W, ?], Invalid]

  type AwsAction[R, W, A] = ReaderT[WriterAttempt[W, ?], R, A]
}