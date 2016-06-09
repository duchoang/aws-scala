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

  private[aws]type WriterF[W, A] = WriterT[Future, W, A] // Future[(W, A)]
  private[aws]type EitherWriter[W, L, A] = EitherT[WriterF[W, ?], L, A] // Future[ (W, Invalid \/ A) ]
  //  private[aws]type ReaderEitherAction[R, W, L, A] = ReaderT[EitherWriter[W, L, ?], R, A] // R => Future[(W, Invalid \/ A)]

  type AwsAction[R, W, A] = ReaderT[EitherWriter[W, Invalid, ?], R, A]
  // R => Future[(W, Invalid \/ A)]
  //ReaderEitherAction[R, W, Invalid, A]
  // ReaderT[EitherWriter[W, L, ?], R, A]
  // ReaderT[EitherT[WriterF[W, ?], L, A], R, A]
}