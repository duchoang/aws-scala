package io.atlassian.aws.s3

import io.atlassian.aws.spec.ArraySpecOps

import scalaz._
import com.amazonaws.services.s3.model.{S3Object, S3ObjectSummary, ObjectListing}
import com.amazonaws.services.s3.{AmazonS3Client => SDKS3Client}
import kadai.Invalid
import org.specs2.matcher.{MustMatchers, Expectable, Matcher}
import scala.collection.JavaConverters._

trait S3SpecOps extends MustMatchers with S3Arbitraries with ArraySpecOps {

  def createTestFolder(bucket: Bucket, testFolder: String)(implicit client: SDKS3Client) =
    runS3Action(S3.createFolder(bucket, testFolder))

  def deleteTestFolder(bucket: Bucket, testFolder: String)(implicit client: SDKS3Client) = {
    import Free._, Scalaz._
    S3.listKeys(bucket, s"$testFolder/").flatMap(l => {
      def bar(listing: Option[ObjectListing]): Trampoline[Option[ObjectListing]] = {
        listing match {
          case None => return_(Scalaz.none)
          case Some(l) =>
            val objects: List[S3ObjectSummary] = l.getObjectSummaries().asScala.toList
            suspend {
              // delete all objects
              val deleteAction = objects.traverseU(s => S3.delete(ContentLocation(Bucket(s.getBucketName), S3Key(s.getKey))))
              deleteAction |> runS3Action
              // get the next load
              if (l.isTruncated) {
                S3.nextBatchOfKeys(l) |> runS3Action match {
                  case -\/(e) => return_(Scalaz.none)
                  case \/-(nextL) => bar(Some(nextL))
                }
              } else {
                return_(Scalaz.none)
              }

            }
        }
      }

      S3Action.value(bar(Some(l)).run)
    }) |> runS3Action
  }

  def runS3Action[A](action: S3Action[A])(implicit client: SDKS3Client) =
    action.run(client).run

  def returnResult[A](check: A => Boolean)(implicit client: SDKS3Client) =
    new ServiceMatcher[A]({
      case -\/(f) => (false, s"Expected value, but was failure $f")
      case \/-(v) => (check(v), s"Expected value, but match failed")
    })

  def returnS3Object(o: ObjectToStore)(implicit client: SDKS3Client) =
    new S3ObjectMatcher[S3Object](o, identity)

  def returnS3Object[A](o: ObjectToStore, f: A => S3Object)(implicit client: SDKS3Client) =
    new S3ObjectMatcher[A](o, f)

  def fail[A](implicit client: SDKS3Client) =
    new ServiceMatcher[A]({
      case -\/(f) => (true, s"Expected failure")
      case \/-(v) => (false, s"Expected failure, but got value $v")
    })

  def matchData(expected: Array[Byte]) =
    { (o: S3Object) => toByteArray(o.getObjectContent) must matchByteContent(expected) and o.getObjectMetadata.getContentLength === expected.length}

  class ServiceMatcher[A](check: \/[Invalid, A] => (Boolean, String))(implicit client: SDKS3Client) extends Matcher[S3Action[A]] {
    def apply[S <: S3Action[A]](s: Expectable[S]) = {
      val execResult = runS3Action(s.value)
      val (comparisonResult, message) = check(execResult)
      result(comparisonResult, message, message, s)
    }
  }

  class S3ObjectMatcher[A](o: ObjectToStore, f: A => S3Object)(implicit client: SDKS3Client) extends Matcher[S3Action[A]] {
    def apply[S <: S3Action[A]](s: Expectable[S]) = {
      runS3Action(s.value) match {
        case -\/(f) => result(false, s"Expected value, but was failure $f", s"Expected value, but was failure $f", s)
        case \/-(v) => result(matchData(o.data)(f(v)), s)
      }
    }
  }
}
