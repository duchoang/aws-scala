package io.atlassian

import kadai.Invalid

import scalaz._
import scalaz.concurrent.Future

package object aws extends Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt

  implicit class AwsTaggedOps[A, T](val a: A @@ T) extends AnyVal {
    def unwrap: A = Tag.unwrap(a)

    override def toString =
      unwrap.toString
  }

  private[aws]type WriterF[W, A] = WriterT[Future, W, A]
  private[aws]type EitherWriter[W, L, A] = EitherT[WriterF[W, ?], L, A]

  type AwsAction[R, W, A] = ReaderT[EitherWriter[W, Invalid, ?], R, A]
}