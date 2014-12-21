package io.atlassian.aws
package s3

import scalaz.{ @@, Tag }
import scalaz.syntax.std.list._

trait Types {
  sealed trait ContentLengthMarker
  type ContentLength = Long @@ ContentLengthMarker
  object ContentLength extends Tagger[Long, ContentLengthMarker]

  sealed trait BucketMarker
  type Bucket = String @@ BucketMarker
  object Bucket extends Tagger[String, BucketMarker]

  sealed trait S3KeyMarker
  type S3Key = String @@ S3KeyMarker
  object S3Key extends Tagger[String, S3KeyMarker] {
    def apply(folders: List[String], key: S3Key): S3Key =
      Tag(s"${folders.mkString("/")}/$key")

    implicit class S3KeySyntax(k: S3Key) {
      lazy val isFolder: Boolean = k.unwrap.endsWith("/")
      lazy val prefix: String = {
        if (k.unwrap.endsWith("/"))
          k.unwrap
        else {
          val lastSlash = k.unwrap.lastIndexOf('/')
          if (lastSlash > 0)
            k.unwrap.substring(0, lastSlash + 1)
          else
            ""
        }
      }
      lazy val folders: List[String] = {
        val pathComponents = k.unwrap.split("/").toList

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
