package io.atlassian.aws
package s3

import java.io.ByteArrayInputStream

import org.scalacheck.Prop
import org.specs2.{ SpecificationWithJUnit, ScalaCheck }

class InputStreamsSpec extends SpecificationWithJUnit with ScalaCheck with S3Arbitraries {
  import InputStreams._
  import spec.NumericTypes._

  def is = s2"""

    This is a specification to test InputStreams.

    InputStreams.readFully should
      work                              $readFullyWorks
      not stack overflow                $readFullyDoesntStackOverflow
  """

  def readFullyWorks = Prop.forAll {
    (len: Pos[Int]) =>
      val data = new Array[Byte](len.i)
      val dataStream = new ByteArrayInputStream(data)
      val array = new Array[Byte](scala.util.Random.nextInt(2 * len.i) + 1)
      readFully(dataStream, array).run === ReadBytes.Chunk(Math.min(array.length, len.i))
  }

  def readFullyDoesntStackOverflow = {
    val data = new Array[Byte](10000)
    val dataStream = new ByteArrayInputStream(data) {
      override def read(buffer: Array[Byte], offset: Int, length: Int): Int =
        super.read(buffer, offset, Math.min(10, buffer.length - offset))
    }
    val array = new Array[Byte](3)
    readFully(dataStream, array).run === ReadBytes.Chunk(Math.min(array.length, data.length))
  }
}
