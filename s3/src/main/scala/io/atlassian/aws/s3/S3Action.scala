package io.atlassian.aws
package s3

import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.AmazonS3Exception

import scalaz.Monoid

object S3Action extends Functions[AmazonS3, S3MetaData] {
  override type Action[A] = S3Action[A]

  override implicit def WMonoid = new Monoid[S3MetaData] {
    override def zero = S3MetaData(Nil)
    override def append(f1: S3MetaData, f2: => S3MetaData) = S3MetaData(f1.requestIds ++ f2.requestIds)
  }

  override def extractRequestIds =
    Some {
      headers =>
        {
          val hds = headers.headers
          for {
            reqId <- hds.get("x-amz-request-id")
            extReqId <- hds.get("x-amz-id-2")
          } yield S3MetaData(List(S3RequestId(reqId, extReqId)))
        }
    }

  override def extractRequestIdsFromException =
    Some {
      case as3e: AmazonS3Exception => Some(S3MetaData(List(S3RequestId(as3e.getRequestId, as3e.getExtendedRequestId))))
      case _                       => None
    }
}
