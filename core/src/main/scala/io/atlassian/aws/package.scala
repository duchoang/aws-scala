package io.atlassian

import kadai.Invalid
import kadai.result.ResultT
import scalaz._

package object aws extends AwsActionTypes with Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt

  implicit class AwsTaggedOps[A, T](val a: A @@ T) extends AnyVal {
    def unwrap: A = Tag.unwrap(a)

    override def toString =
      unwrap.toString
  }

  type WriterAttempt[W, A] = ResultT[Writer[W, ?], A]
  def WriterAttemptMonadError[W](implicit M: Monoid[W]) = EitherT.eitherTMonadError[Writer[W, ?], Invalid]

  type AwsAction[R, W, A] = ReaderT[WriterAttempt[W, ?], R, A]
}