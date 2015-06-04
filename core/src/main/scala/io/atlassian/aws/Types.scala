package io.atlassian.aws

import scalaz.{ @@, Tag }
import scalaz.syntax.std.option._

import argonaut._, Argonaut._

trait Types {
  private[aws] trait Tagger[A] {
    sealed trait Marker
    def apply(a: A): A @@ Marker = Tag(a)
    def unapply(tagged: A @@ Marker): Option[A] = Tag.unwrap(tagged).some
  }

  sealed trait OverwriteMode
  object OverwriteMode {
    case object Overwrite extends OverwriteMode
    case object NoOverwrite extends OverwriteMode

    implicit val OverwriteModeEncodeJson: EncodeJson[OverwriteMode] =
      EncodeJson {
        case Overwrite   => jString("overwrite")
        case NoOverwrite => jString("no-overwrite")
      }

    implicit val OverwriteModeDecodeJson: DecodeJson[OverwriteMode] =
      optionDecoder(_.string flatMap {
        case "overwrite"    => Some(Overwrite)
        case "no-overwrite" => Some(NoOverwrite)
        case _              => None
      }, "OverwriteMode")
  }
}
