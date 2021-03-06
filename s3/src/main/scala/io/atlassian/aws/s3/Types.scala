package io.atlassian.aws
package s3

import scalaz.{ @@, Monoid, Tag }
import scalaz.syntax.std.list._

trait Types {
  type ContentLength = Long @@ ContentLength.Marker
  object ContentLength extends Tagger[Long]

  type Bucket = String @@ Bucket.Marker
  object Bucket extends Tagger[String]

  type S3Key = String @@ S3Key.Marker
  object S3Key extends Tagger[String] {
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

  case class S3RequestId(regular: String, extended: String)
  case class S3MetaData(requestIds: List[S3RequestId])
  object S3MetaData {
    implicit object S3MetaDataMonoid extends Monoid[S3MetaData] {
      override def zero = S3MetaData(Nil)
      override def append(f1: S3MetaData, f2: => S3MetaData) = S3MetaData(f1.requestIds ++ f2.requestIds)
    }
  }
}
