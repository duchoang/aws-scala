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
    thing: Value =>
      val testData = thing.copy(deletedTimestamp = None)
      val mappedData = Value.column.marshaller.toFlattenedMap(testData)
      (mappedData.get("hash") === Encoder[String].encode(testData.hash)) and
        (mappedData.get("metaData") === Encoder[String].encode(testData.metaData)) and
        (mappedData.get("deletedTimestamp") must beNone)
  }
  def workTogether = Prop.forAll {
    thing: Value =>
      (Value.column.marshaller.toFlattenedMap(thing) |> Value.column.unmarshaller.fromMap) must equal(Attempt.ok(thing))
  }
}
