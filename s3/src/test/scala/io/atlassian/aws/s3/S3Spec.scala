package io.atlassian.aws
package s3

import com.amazonaws.regions.Region
import org.junit.runner.RunWith
import org.specs2.ScalaCheck
import org.specs2.SpecificationWithJUnit
import org.specs2.main.Arguments
import com.amazonaws.services.s3.{AmazonS3Client => SDKS3Client}
import org.specs2.specification.Step
import org.scalacheck.Prop
import java.io.ByteArrayInputStream
import scalaz.syntax.id._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class S3Spec(arguments: Arguments) extends SpecificationWithJUnit with ScalaCheck with S3Arbitraries with S3SpecOps {

  import S3Key._

  implicit val S3_CLIENT = new SDKS3Client()

  def is = skipAllUnless(arguments.commandLine.contains("aws-integration")) ^ s2"""

    This is a specification to test S3 actions.

    S3 library should                                       ${Step(createTestFolder(BUCKET, TEST_FOLDER))}
      have a working get and put i.e. I can get what I put  $getWhatWasPut
      have a working createFolders                          $createFoldersWorks
      have a put that creates folders as necessary          $putWithFoldersWorks
      have a copy that overwrites                           $copyWorks
      have a copy that does not overwrite                   $copyWithNoOverwriteDoesNotOverwrite
      have a copy that saves correctly in no overwrite mode $copyWithNoOverwriteSavesCorrectly
      have a copy that creates folders as necessary         $copyWithFoldersWorks
      have an exists that returns 'exists' correctly        $existsReturnsTrueForSomethingThatExists
      have an exists that returns 'not exists' correctly    $existsReturnsFalseForSomethingThatNotExists
      have a metaData function that returns correctly       $metaDataReturnsMetaData
      have a metaData function that errors if there is no object $metaDataErrorsIfNoObject
      have a safeGet that doesn't fail if the object doesn't exist $safeGetWorksIfNoObject
      have a safeGet that returns the object                $safeGetWorksIfThereIsObject
      have a function to get the region for a bucket        $regionForWorks
      have a function to get the region for a non-existent bucket work        $regionForWorksForNonExistentBucket
                                                            ${Step(deleteTestFolder(BUCKET, TEST_FOLDER))}
  """

  lazy val TEST_FOLDER = s"s3-test-${System.currentTimeMillis}"

  val BUCKET = Bucket(arguments.commandLine.value("bucket").getOrElse("sawa-syd-dev"))

  def getWhatWasPut = Prop.forAll {
    data: ObjectToStore => {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val location = ContentLocation(BUCKET, key)

      (for {
        _ <- S3.putStream(location, dataStream, Some(data.data.length.toLong))
        result <- S3.get(location)
      } yield result) must returnS3Object(data)

    }
  }.set(minTestsOk = 10)

  def createFoldersWorks = Prop.forAll {
    (folders: S3Folders, key: S3Key) => {
      import scala.collection.JavaConversions._
      val s3Key = S3Key(TEST_FOLDER :: folders.folders, key)
      val s3KeyWithoutLastElement = s3Key.prefix
      (for {
        _ <- S3.createFoldersFor(ContentLocation(BUCKET, s3Key))
        keys <- S3.listKeys(BUCKET, s3KeyWithoutLastElement)
      } yield keys) must returnResult { keys =>
        keys.getObjectSummaries.toList.map { _.getKey } must contain(s3KeyWithoutLastElement)
      }
    }
  }.set(minTestsOk = 10)

  def putWithFoldersWorks = Prop.forAll {
    (data: ObjectToStore, folders: S3Folders) => {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${folders.toPrefix}${data.key}")
      val location = ContentLocation(BUCKET, key)

      (for {
        _ <- S3.putStream(location, dataStream, Some(data.data.length.toLong))
        result <- S3.get(location)
      } yield result) must returnS3Object(data)
    }
  }.set(minTestsOk = 10)

  private def testCopy(overwrite: OverwriteMode) = Prop.forAll {
    (data: ObjectToStore, newKey: S3Key) => newKey != data.key ==> {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val newKeyNamespaced = S3Key(s"$TEST_FOLDER/${newKey}")
      val oldLocation = ContentLocation(BUCKET, key)
      val newLocation = ContentLocation(BUCKET, newKeyNamespaced)

      (for {
        _ <- S3.putStream(oldLocation, dataStream, Some(data.data.length.toLong))
        _ <- S3.copy(oldLocation, newLocation)
        oldNotDeleted <- S3.exists(oldLocation)
        result <- S3.get(newLocation)
      } yield (oldNotDeleted, result)) must returnResult {
        r => {
          val (oldNotDeleted, result) = r
          oldNotDeleted must beTrue and
            (result must matchData(data.data))
        }
      }
    }
  }.set(minTestsOk = 10)

  def copyWithNoOverwriteDoesNotOverwrite = Prop.forAll {
    (data: ObjectToStore, data2: ObjectToStore) => data2.key != data.key ==> {
      val dataStream1 = new ByteArrayInputStream(data.data)
      val dataStream2 = new ByteArrayInputStream(data2.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val newKeyNamespaced = S3Key(s"$TEST_FOLDER/${data2.key}")
      val oldLocation = ContentLocation(BUCKET, key)
      val newLocation = ContentLocation(BUCKET, newKeyNamespaced)

      (for {
        _ <- S3.putStream(oldLocation, dataStream1, Some(data.data.length.toLong))
        _ <- S3.putStream(newLocation, dataStream2, Some(data2.data.length.toLong))
        copyResult <- S3.copy(oldLocation, newLocation, overwrite = OverwriteMode.NoOverwrite)
        oldNotDeleted <- S3.exists(oldLocation)
        result <- S3.get(newLocation)
      } yield (oldNotDeleted, result, copyResult)) must returnResult {
        r => {
          val (oldNotDeleted, result, copyResult) = r
          val is = result.getObjectContent
          oldNotDeleted must beTrue and
            (copyResult must beNone) and
            (result must matchData(data2.data))
        }
      }
    }
  }.set(minTestsOk = 10)

  def copyWorks =
    testCopy(OverwriteMode.Overwrite)

  def copyWithNoOverwriteSavesCorrectly =
    testCopy(OverwriteMode.NoOverwrite)

  def copyWithFoldersWorks = Prop.forAll {
    (data: ObjectToStore, folders: S3Folders, newKey: S3Key) => newKey != data.key ==> {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val newKeyNamespaced = S3Key(s"$TEST_FOLDER/${folders.toPrefix}${newKey}")
      val oldLocation = ContentLocation(BUCKET, key)
      val newLocation = ContentLocation(BUCKET, newKeyNamespaced)

      (for {
        _ <- S3.putStream(oldLocation, dataStream, Some(data.data.length.toLong))
        _ <- S3.copy(oldLocation, newLocation)
        oldNotDeleted <- S3.exists(oldLocation)
        result <- S3.get(newLocation)
      } yield (oldNotDeleted, result)) must returnResult {
        r => {
          val (oldNotDeleted, result) = r
          val is = result.getObjectContent
          oldNotDeleted must beTrue and
            (result must matchData(data.data))
        }
      }
    }
  }.set(minTestsOk = 10)

  def existsReturnsTrueForSomethingThatExists = Prop.forAll {
    data: ObjectToStore => {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val location = ContentLocation(BUCKET, key)

      (for {
        _ <- S3.putStream(location, dataStream, Some(data.data.length.toLong))
        exists <- S3.exists(location)
      } yield exists) must returnResult { identity }
    }
  }.set(minTestsOk = 5)

  def existsReturnsFalseForSomethingThatNotExists = Prop.forAll {
    s3key: S3Key => {
      val key = S3Key(s"$TEST_FOLDER/$s3key")
      val location = ContentLocation(BUCKET, key)

      (for {
        exists <- S3.exists(location)
      } yield exists) must returnResult { exists => exists === false }
    }
  }.set(minTestsOk = 5)

  def metaDataReturnsMetaData = Prop.forAll {
    (data: ObjectToStore) => {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val location = ContentLocation(BUCKET, key)
      val metaData = S3.DefaultObjectMetadata <| { _.addUserMetadata("foo", data.key) }

      (for {
        _ <- S3.putStream(location, dataStream, Some(data.data.length.toLong), metaData)
        retrievedMetaData <- S3.metaData(location)
      } yield retrievedMetaData) must returnResult { r => r.getUserMetadata.get("foo") === data.key }
    }
  }.set(minTestsOk = 5)

  def metaDataErrorsIfNoObject = Prop.forAll {
    s3key: S3Key => {
      val key = S3Key(s"$TEST_FOLDER/$s3key")
      val location = ContentLocation(BUCKET, key)

      S3.metaData(location) must fail
    }
  }.set(minTestsOk = 5)

  def safeGetWorksIfNoObject = Prop.forAll {
    s3key: S3Key =>
      val key = S3Key(s"$TEST_FOLDER/$s3key")
      val location = ContentLocation(BUCKET, key)

      S3.safeGet(location) must returnResult { _.isEmpty }
  }.set(minTestsOk = 5)

  def safeGetWorksIfThereIsObject = Prop.forAll {
    data: ObjectToStore => {
      val dataStream = new ByteArrayInputStream(data.data)
      val key = S3Key(s"$TEST_FOLDER/${data.key}")
      val location = ContentLocation(BUCKET, key)

      (for {
        _ <- S3.putStream(location, dataStream, Some(data.data.length.toLong))
        result <- S3.safeGet(location)
      } yield result) must returnS3Object(data, _.get)

    }
  }.set(minTestsOk = 10)

  def regionForWorks =
    S3.regionFor(BUCKET) must returnResult { (r: Region) =>
      S3_CLIENT.getBucketLocation(BUCKET) === r.getName
    }

  def regionForWorksForNonExistentBucket =
    S3.regionFor(Bucket(java.util.UUID.randomUUID().toString)) must fail
}
