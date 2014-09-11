package io.atlassian.aws

trait Types {
  sealed trait OverwriteMode
  object OverwriteMode {
    case object Overwrite extends OverwriteMode
    case object NoOverwrite extends OverwriteMode
  }
}
