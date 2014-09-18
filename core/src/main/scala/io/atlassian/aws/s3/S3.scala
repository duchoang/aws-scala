package io.atlassian.aws
package s3

import com.amazonaws.services.s3.model._
import java.io.{ ByteArrayInputStream, InputStream }
import io.atlassian.aws.AmazonExceptions.ServiceException
import kadai.Invalid

import scalaz.std.list._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import scalaz.syntax.std.boolean._
import scalaz.std.option._

object S3 {

  import S3Key._

  def get(location: ContentLocation): S3Action[S3Object] =
    S3Action.withClient(_.getObject(new GetObjectRequest(location.bucket, location.key)))

  def safeGet(location: ContentLocation): S3Action[Option[S3Object]] =
    get(location).map { some }.handle {
      case Invalid.Err(ServiceException(AmazonExceptions.ExceptionType.NotFound, _)) => Attempt.ok(None)
    }

  def putStream(location: ContentLocation, stream: InputStream, length: Option[Long] = None, metaData: ObjectMetadata = DefaultObjectMetadata, createFolders: Boolean = true): S3Action[PutObjectResult] = {
    length.foreach(metaData.setContentLength)

    for {
      _ <- createFolders.whenM(S3.createFoldersFor(location))
      putResult <- S3Action.withClient(_.putObject(location.bucket, location.key, stream, metaData))
    } yield putResult

  }

  def createFoldersFor(location: ContentLocation): S3Action[List[PutObjectResult]] =
    location.key.foldersWithLeadingPaths.traverse[S3Action, PutObjectResult] { folder => S3.createFolder(location.bucket, folder) }

  /**
   * Creates a folder in an S3 bucket. A folder is just an empty 'file' with a / on the end of the name. However, if you
   * want to create a folder in a bucket that enforces encryption, you need to create it using the appropriate
   * metadata, which this function can do.
   * @param bucket Bucket name
   * @param folder Folder name (without trailing slash)
   * @param metaData Folder metadata (default enforces encryption)
   * @return S3Action with no return result (Unit)
   */
  def createFolder(bucket: Bucket, folder: String, metaData: ObjectMetadata = DefaultObjectMetadata): S3Action[PutObjectResult] = {
    val dummyStream = new ByteArrayInputStream(Array[Byte]())
    putStream(ContentLocation(bucket, S3Key(s"$folder/")), dummyStream, Some(0), metaData, false)
  }

  /**
   * Copy contents at the oldBucket and oldKey to a newBucket and newKey.
   * @param oldLocation The source bucket and key
   * @param newLocation The destination bucket and key
   * @param newMetaData The function will copy the existing metadata of the source object unless you specify newMetaData which will be used instead.
   * @param createFolders Set to true if you want to create any folders referenced in the ContentLocation as part of the copy process.
   * @param overwrite Set to Overwrite if you want to overwrite whatever is in the destination location. Set to NoOverwrite to return without
   *                  overwriting the destination location.
   * @return S3Action with CopyResult (either Copied if it was copied, or NotCopied if the destination location already has content and
   *         NoOverwrite was specified).
   */
  def copy(oldLocation: ContentLocation,
           newLocation: ContentLocation,
           newMetaData: Option[ObjectMetadata] = None,
           createFolders: Boolean = true,
           overwrite: OverwriteMode = OverwriteMode.Overwrite): S3Action[Option[CopyObjectResult]] =
    for {
      doCopy <- overwrite match {
        case OverwriteMode.Overwrite   => S3Action.ok(true)
        case OverwriteMode.NoOverwrite => exists(newLocation).map { !_ }
      }
      result <- if (doCopy)
        forceCopy(oldLocation, newLocation, newMetaData, createFolders).map { some }
      else
        S3Action.ok(none[CopyObjectResult])
    } yield result

  private def forceCopy(oldLocation: ContentLocation, newLocation: ContentLocation, newMetaData: Option[ObjectMetadata], createFolders: Boolean): S3Action[CopyObjectResult] =
    for {
      _ <- createFolders.whenM { S3.createFoldersFor(newLocation) }
      metaData <- newMetaData.fold { metaData(oldLocation) } { S3Action.ok }
      result <- S3Action.withClient {
        _.copyObject(
          new CopyObjectRequest(oldLocation.bucket, oldLocation.key, newLocation.bucket, newLocation.key).withNewObjectMetadata(metaData)
        )
      }
    } yield result

  def safeMetaData(location: ContentLocation): S3Action[Option[ObjectMetadata]] =
    S3Action { client =>
      Attempt.safe {
        try {
          Some(client.getObjectMetadata(location.bucket, location.key))
        } catch {
          case e: AmazonS3Exception if e.getStatusCode == 404 => None
        }
      }
    }

  def metaData(location: ContentLocation): S3Action[ObjectMetadata] =
    safeMetaData(location).flatMap {
      case Some(o) => S3Action.ok(o)
      case None    => S3Action.fail(s"No object exists at $location")
    }

  def exists(location: ContentLocation): S3Action[Boolean] =
    safeMetaData(location).map { _.isDefined }

  def delete(location: ContentLocation): S3Action[Unit] =
    S3Action.withClient {
      _.deleteObject(new DeleteObjectRequest(location.bucket, location.key))
    }

  def listKeys(bucket: Bucket, prefix: String): S3Action[ObjectListing] =
    S3Action.withClient {
      _.listObjects(bucket, prefix)
    }

  def nextBatchOfKeys(lastListing: ObjectListing): S3Action[ObjectListing] =
    S3Action.withClient {
      _.listNextBatchOfObjects(lastListing)
    }

  def exists(bucket: Bucket): S3Action[Boolean] =
    S3Action.withClient {
      _.doesBucketExist(bucket)
    }

  def ServerSideEncryption: ObjectMetadata =
    DefaultObjectMetadata <| { _.setSSEAlgorithm(ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION) }

  def DefaultObjectMetadata: ObjectMetadata = new ObjectMetadata()

}
