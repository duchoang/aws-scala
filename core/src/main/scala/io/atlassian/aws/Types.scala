package io.atlassian.aws

import scalaz.{ @@, Tag }

import argonaut._, Argonaut._

trait Types {
  private[aws] trait Tagger[A, T] {
    def apply(a: A): A @@ T = Tag(a)
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
