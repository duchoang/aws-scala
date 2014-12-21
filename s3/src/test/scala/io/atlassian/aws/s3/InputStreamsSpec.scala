package io.atlassian.aws.s3

import java.io.ByteArrayInputStream

import org.scalacheck.Prop
import org.specs2.{ SpecificationWithJUnit, ScalaCheck }

class InputStreamsSpec extends SpecificationWithJUnit with ScalaCheck with S3Arbitraries {
  import InputStreams._
  def is = s2"""

    This is a specification to test InputStreams.

    InputStreams should
      have a working readFully                              $readFullyWorks
  """

  def readFullyWorks = Prop.forAll {
    (data: ObjectToStore) =>
      val dataStream = new ByteArrayInputStream(data.data)
      val array = new Array[Byte](scala.util.Random.nextInt(2 * data.data.length) + 1)
      readFully(dataStream, array).read === Math.min(array.length, data.data.length)
  }.set(minTestsOk = 25)

}
