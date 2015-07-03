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

  type MetaData = Map[String, List[String]]
  object MetaData {
    val none: MetaData = Map.empty
    def apply(key: String, value: String): MetaData = Map(key -> List(value))
  }
  implicit def MetaDataMonoid: Monoid[MetaData] = scalaz.std.map.mapMonoid[String, List[String]]

  type WriterAttempt[A] = ResultT[Writer[MetaData, ?], A]
  implicit val WriterAttemptMonad: Monad[WriterAttempt] = EitherT.eitherTMonad[Writer[MetaData, ?], Invalid]

  type AwsAction[R, A] = ReaderT[WriterAttempt, R, A]
}