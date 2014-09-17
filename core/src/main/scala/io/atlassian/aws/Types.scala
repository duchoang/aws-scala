package io.atlassian.aws

import scalaz.{ @@, Tag }

trait Types {
  private[aws] trait Tagger[A, T] {
    def apply(a: A): A @@ T = Tag(a)
  }

  sealed trait OverwriteMode
  object OverwriteMode {
    case object Overwrite extends OverwriteMode
    case object NoOverwrite extends OverwriteMode
  }
}
