package io.atlassian

import kadai.result.ResultT
import scalaz._

package object aws extends Types {
  type Attempt[A] = kadai.Attempt[A]
  val Attempt = kadai.Attempt

  implicit class AwsTaggedOps[A, T](val a: A @@ T) extends AnyVal {
    def unwrap: A = Tag.unwrap(a)

    override def toString =
      unwrap.toString
  }

  type ResultWriter[W, A] = ResultT[Writer[W, ?], A]

  type AwsAction[R, W, A] = ReaderT[ResultWriter[W, ?], R, A]
}