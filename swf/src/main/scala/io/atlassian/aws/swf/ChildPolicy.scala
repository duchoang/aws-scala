package io.atlassian.aws.swf

import scalaz.syntax.std.option._

sealed abstract class ChildPolicy(val aws: String)

object ChildPolicy {
  case object Terminate extends ChildPolicy("TERMINATE")
  case object RequestCancel extends ChildPolicy("REQUEST_CANCEL")
  case object Abandon extends ChildPolicy("ABANDON")

  def unapply(s: String): Option[ChildPolicy] =
    s match {
      case "TERMINATE" => Terminate.some
      case "REQUEST_CANCEL" =>  RequestCancel.some
      case "ABANDON" => Abandon.some
      case _ => None
    }
}
