package io.atlassian

import scalaz.{ ReaderT, Tag, @@ }

package object aws extends Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt

  implicit class AwsTaggedOps[A, T](val a: A @@ T) extends AnyVal {
    def unwrap: A = Tag.unwrap(a)

    override def toString =
      unwrap.toString
  }

  type AwsAction[R, A] = ReaderT[Attempt, R, A]
}