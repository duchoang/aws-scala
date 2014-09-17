package io.atlassian.aws
package sqs

import com.amazonaws.services.sqs.model.Message
import spec.ScalaCheckSpec
import org.junit.runner.RunWith
import org.scalacheck.Prop
import argonaut._, Argonaut._
import scala.collection.JavaConverters._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class RetriedMessageMarshallUnmarshallSpec extends ScalaCheckSpec {

  import Examples._
  import MessageAttributeEncoder._
  import Arbitraries._
  
  def is = s2"""
    
  RetriedMessage should
    be marshallable                                 $marshallOnRetriedMessage
    be unmarshallable                               $marshalledAndUnmarshall
    be unmarshallable without a retry count         $unmarshallWithoutRetryCount

  """


  def marshallOnRetriedMessage = Prop.forAll {
    thing: RetriedMessage[Replicate] =>
      val M = Marshaller[RetriedMessage[Replicate]]
      val header = M.headerFlattened(thing)
      val body = M.body(thing)
      
      body === thing.message.jencode.nospaces and
        (header.get("retry-count") === IntEncode(thing.retryCount))
  }
  
  def marshalledAndUnmarshall = Prop.forAll {
    thing: RetriedMessage[Replicate] =>
      val M = Marshaller[RetriedMessage[Replicate]]

      val message =
        new Message()
          .withBody(M.body(thing))
          .withMessageAttributes(M.headerFlattened(thing).asJava)

      Unmarshaller[RetriedMessage[Replicate]].unmarshall(message) === Attempt.ok(thing)
  }
  
  def unmarshallWithoutRetryCount = Prop.forAll {
    thing: Replicate =>
      val message =
        new Message()
          .withBody(Marshaller[Replicate].body(thing))
          .withMessageAttributes(Marshaller[Replicate].headerFlattened(thing).asJava)

      Unmarshaller[RetriedMessage[Replicate]].unmarshall(message) === Attempt.ok(RetriedMessage(0, thing))
  }
}
