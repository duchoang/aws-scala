package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec
import org.junit.runner.RunWith
import org.scalacheck.Prop

import scalaz.syntax.id._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ObjectMarshallUnmarshallSpec extends ScalaCheckSpec {

  def is = s2"""
  ObjectUmarshaller should
    have a working toFlattenedMap                   $flattenedMapWorks
  ObjectMarshaller and ObjectUnmarshaller should
    work together                                   $workTogether

  """

  import TestData._
  import Encoder._

  def flattenedMapWorks = Prop.forAll {
    thing: ThingValue =>
      val testData = thing.copy(deletedTimestamp = None)
      val mappedData = Marshaller[ThingValue].toFlattenedMap(testData)
      (mappedData.get("blobHash") === StringEncode(testData.blobHash)) and
        (mappedData.get("metaData") === StringEncode(testData.metaData)) and
        (mappedData.get("deletedTimestamp") must beNone)
  }
  def workTogether = Prop.forAll {
    thing: ThingValue =>
      (Marshaller[ThingValue].toFlattenedMap(thing) |> Unmarshaller[ThingValue].fromMap) must equal(Attempt.ok(thing))
  }
}
