package io.atlassian.aws

import scalaz.Monoid

case class MetaData(requestIds: List[String])
object MetaData {
  implicit object MetaDataMonoid extends Monoid[MetaData] {
    override def zero = MetaData(Nil)
    override def append(f1: MetaData, f2: => MetaData) = MetaData(f1.requestIds ++ f2.requestIds)
  }
}

