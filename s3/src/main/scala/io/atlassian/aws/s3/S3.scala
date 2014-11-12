package io.atlassian.aws
package s3

import java.io.{ ByteArrayInputStream, InputStream }
import java.util.ArrayList

import com.amazonaws.regions.Region
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model.{ UploadPartResult, AbortMultipartUploadRequest, AmazonS3Exception, CopyObjectRequest, CopyObjectResult, DeleteObjectRequest, ObjectListing, PutObjectResult, ObjectMetadata, GetObjectRequest, S3Object, InitiateMultipartUploadRequest, UploadPartRequest, PartETag, CompleteMultipartUploadRequest, CompleteMultipartUploadResult }
import io.atlassian.aws.AmazonExceptions.ServiceException
import kadai.Invalid

import scala.collection.immutable.List
import scala.collection.JavaConverters._

import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.id._
import scalaz.syntax.traverse._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

object S3 {
  import S3Key._

  val MultipartChunkSize = 5 * 1024 * 1024

  def get(location: ContentLocation, range: Range = Range.All): S3Action[S3Object] =
    S3Action.withClient {
      _ getObject new GetObjectRequest(location.bucket.unwrap, location.key.unwrap) <| {
        req => range.get.foreach { case (from, to) => req.setRange(from, to) }
      }
    }

  def safeGet(location: ContentLocation, range: Range = Range.All): S3Action[Option[S3Object]] =
    get(location, range).map { some }.handle {
      case Invalid.Err(ServiceException(AmazonExceptions.ExceptionType.NotFound, _)) => Attempt.ok(None)
    }

  def putStream(location: ContentLocation, stream: InputStream, length: Option[Long] = None, metaData: ObjectMetadata = DefaultObjectMetadata, createFolders: Boolean = true): S3Action[PutObjectResult] =
    for {
      _ <- createFolders.whenM(S3.createFoldersFor(location))
      _ = length.foreach(metaData.setContentLength)
      putResult <- S3Action.withClient(_.putObject(location.bucket.unwrap, location.key.unwrap, stream, metaData))
    } yield putResult

  private def putChunks(location: ContentLocation, stream: InputStream, uploadId: String, parts: List[PartETag], length: Long, buffer: Array[Byte]): S3Action[(List[PartETag], Long)] =
    for {
      rn <- S3Action.safe { stream.read(buffer) }
      result <- rn match {
        case -1 => S3Action.value((parts, length))
        case _ => for {
          // TODO - Dirty hack to get the abort working. Need to add a combinator to AwsAction for this
          partResult <- AwsAction.apply[AmazonS3Client, UploadPartResult] { client =>
            try {
              Attempt.ok(client.uploadPart(new UploadPartRequest()
                .withBucketName(location.bucket.unwrap).withKey(location.key.unwrap)
                .withUploadId(uploadId).withPartNumber(parts.length + 1)
                .withInputStream(new ByteArrayInputStream(buffer, 0, rn))
                .withPartSize(rn.toLong)))
            } catch {
              case t: Throwable =>
                client.abortMultipartUpload(new AbortMultipartUploadRequest(location.bucket.unwrap, location.key.unwrap, uploadId))
                Attempt.exception(t)
            }
          }
          putNextChunk <- putChunks(location, stream, uploadId, parts :+ partResult.getPartETag, length + rn.toLong, buffer)
        } yield putNextChunk
      }
    } yield result

  /**
   * EXPERIMENTAL!!!
   * First cut at uploaded content of unknown length to S3 (thanks @sreis!)
   * TODO - abort multipart upload upon error
   */
  def putStream2(location: ContentLocation, stream: InputStream, length: Option[Long] = None, metaData: ObjectMetadata = DefaultObjectMetadata, createFolders: Boolean = true): S3Action[Long] =
    length match {
      case Some(contentLength) => for {
        _ <- putStream(location, stream, length, metaData, createFolders)
      } yield contentLength
      case None => for {
        _ <- createFolders.whenM(S3.createFoldersFor(location))
        initResult <- S3Action.withClient {
          _.initiateMultipartUpload(new InitiateMultipartUploadRequest(location.bucket.unwrap, location.key.unwrap, metaData))
        }
        putResult <- putChunks(location, stream, initResult.getUploadId, List(), 0, new Array[Byte](MultipartChunkSize))
        (parts, contentLength) = putResult
        compResult <- S3Action.withClient {
          // We need to convert `parts` to a mutable java.util.List, because the AWS SDK will sort the list internally.
          _.completeMultipartUpload(new CompleteMultipartUploadRequest(location.bucket.unwrap, location.key.unwrap, initResult.getUploadId, new ArrayList(parts.asJava)))
        }
      } yield contentLength
    }

  def createFoldersFor(location: ContentLocation): S3Action[List[PutObjectResult]] =
    location.key.foldersWithLeadingPaths.traverse[S3Action, PutObjectResult] {
      folder => S3.createFolder(location.bucket, folder)
    }

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
   * @param from The source bucket and key
   * @param to The destination bucket and key
   * @param meta The function will copy the existing metadata of the source object unless you specify newMetaData which will be used instead.
   * @param createFolders Set to true if you want to create any folders referenced in the ContentLocation as part of the copy process.
   * @param overwrite Set to Overwrite if you want to overwrite whatever is in the destination location. Set to NoOverwrite to return without
   *                  overwriting the destination location.
   * @return S3Action with CopyResult (either Copied if it was copied, or NotCopied if the destination location already has content and
   *         NoOverwrite was specified).
   */
  def copy(from: ContentLocation,
           to: ContentLocation,
           meta: Option[ObjectMetadata] = None,
           createFolders: Boolean = true,
           overwrite: OverwriteMode = OverwriteMode.Overwrite): S3Action[Option[CopyObjectResult]] =
    for {
      doCopy <- overwrite match {
        case OverwriteMode.Overwrite   => S3Action.ok(true)
        case OverwriteMode.NoOverwrite => exists(to).map { !_ }
      }
      result <- if (doCopy)
        forceCopy(from, to, meta, createFolders).map { some }
      else
        S3Action.ok(none[CopyObjectResult])
    } yield result

  private def forceCopy(from: ContentLocation, to: ContentLocation, newMetaData: Option[ObjectMetadata], createFolders: Boolean): S3Action[CopyObjectResult] =
    for {
      _ <- createFolders.whenM { S3.createFoldersFor(to) }
      metaData <- newMetaData.fold { metaData(from) } { S3Action.ok }
      result <- S3Action.withClient { _ copyObject new CopyObjectRequest(from.bucket.unwrap, from.key.unwrap, to.bucket.unwrap, to.key.unwrap).withNewObjectMetadata(metaData) }
    } yield result

  def safeMetaData(location: ContentLocation): S3Action[Option[ObjectMetadata]] =
    metaData(location).map { some }.handle {
      case Invalid.Err(ServiceException(AmazonExceptions.ExceptionType.NotFound, _)) => Attempt.ok(None)
    }

  def metaData(location: ContentLocation): S3Action[ObjectMetadata] =
    S3Action.withClient { _.getObjectMetadata(location.bucket.unwrap, location.key.unwrap) }

  def exists(location: ContentLocation): S3Action[Boolean] =
    safeMetaData(location).map { _.isDefined }

  def delete(location: ContentLocation): S3Action[Unit] =
    S3Action.withClient {
      _.deleteObject(new DeleteObjectRequest(location.bucket.unwrap, location.key.unwrap))
    }

  def listKeys(bucket: Bucket, prefix: String): S3Action[ObjectListing] =
    S3Action.withClient {
      _.listObjects(bucket.unwrap, prefix)
    }

  def nextBatchOfKeys(lastListing: ObjectListing): S3Action[ObjectListing] =
    S3Action.withClient {
      _.listNextBatchOfObjects(lastListing)
    }

  def exists(bucket: Bucket): S3Action[Boolean] =
    S3Action.withClient {
      _.doesBucketExist(bucket.unwrap)
    }

  def regionFor(bucket: Bucket): S3Action[Region] =
    S3Action.withClient { _.getBucketLocation(bucket.unwrap) }.flatMap { region =>
      region match {
        case AmazonRegion(r) => S3Action.ok(r)
        case _               => S3Action.fail(s"Could not parse region $region")
      }
    }

  def ServerSideEncryption: ObjectMetadata =
    DefaultObjectMetadata <| { _.setSSEAlgorithm(ObjectMetadata.AES_256_SERVER_SIDE_ENCRYPTION) }

  def DefaultObjectMetadata: ObjectMetadata = new ObjectMetadata()
}
