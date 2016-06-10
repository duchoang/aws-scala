package io.atlassian

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

  private[aws] type WriterF[W, A] = WriterT[Future, W, A] // Future[(W, A)]
  private[aws] type EitherWriter[W, L, A] = EitherT[WriterF[W, ?], L, A] // Future[ (W, L \/ A) ]

  implicit class ActionOps[R, W, A](a: AwsAction[R, W, A]) extends AwsActionOps[R, W, A](a)
}
