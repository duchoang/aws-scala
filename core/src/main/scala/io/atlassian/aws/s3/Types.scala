package io.atlassian.aws.s3

import scalaz.{ @@, Tag }
import scalaz.syntax.std.list._

trait Types {
  sealed trait BucketMarker
  type Bucket = String @@ BucketMarker
  object Bucket {
    def apply(name: String): Bucket =
      Tag(name)
  }

  sealed trait S3KeyMarker
  type S3Key = String @@ S3KeyMarker
  object S3Key {
    def apply(key: String): S3Key =
      Tag(key)

    def apply(folders: List[String], key: S3Key): S3Key =
      Tag(s"${folders.mkString("/")}/$key")

    implicit class S3KeySyntax(k: S3Key) {
      lazy val isFolder: Boolean = k.endsWith("/")
      lazy val prefix: String = {
        if (k.endsWith("/"))
          k
        else {
          val lastSlash = k.lastIndexOf('/')
          if (lastSlash > 0)
            k.substring(0, lastSlash + 1)
          else
            ""
        }
      }
      lazy val folders: List[String] = {
        val pathComponents = k.split("/").toList

        if (pathComponents.isEmpty) Nil
        else if (isFolder) pathComponents
        else pathComponents.init
      }

      lazy val foldersWithLeadingPaths: List[String] =
        folders.initz.tail.map { l => l.mkString("/") }
    }
  }

  sealed trait CopyResult
  object CopyResult {
    case object Copied extends CopyResult
    case object NotCopied extends CopyResult
  }
}
