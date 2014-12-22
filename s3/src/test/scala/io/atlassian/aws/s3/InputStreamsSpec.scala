package io.atlassian.aws.s3

import java.io.ByteArrayInputStream

import org.scalacheck.Prop
import org.specs2.{ SpecificationWithJUnit, ScalaCheck }

class InputStreamsSpec extends SpecificationWithJUnit with ScalaCheck with S3Arbitraries {
  import InputStreams._
  def is = s2"""

    This is a specification to test InputStreams.

    InputStreams.readFully should
      work                              $readFullyWorks
      not stack overflow                $readFullyDoesntStackOverflow
  """

  def readFullyWorks = Prop.forAll {
    (data: ObjectToStore) =>
      val dataStream = new ByteArrayInputStream(data.data)
      val array = new Array[Byte](scala.util.Random.nextInt(2 * data.data.length) + 1)
      readFully(dataStream, array).run === ReadBytes.Chunk(Math.min(array.length, data.data.length))
  }.set(minTestsOk = 25)

  def readFullyDoesntStackOverflow = Prop.forAll {
    (data: ObjectToStore) =>
      val dataStream = new ByteArrayInputStream(data.data) {
        override def read(buffer: Array[Byte], offset: Int, length: Int): Int =
          super.read(buffer, offset, Math.min(10, buffer.length - offset))
      }
      val array = new Array[Byte](scala.util.Random.nextInt(2 * data.data.length) + 1)
      readFully(dataStream, array).run === ReadBytes.Chunk(Math.min(array.length, data.data.length))
  }.set(minTestsOk = 25)
}
