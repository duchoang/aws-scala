package io.atlassian.aws.dynamodb

import java.nio.ByteBuffer

import kadai.Attempt
import org.scalacheck.{ Gen, Arbitrary }

import scalaz.Order
import scalaz.Ordering
import scalaz.syntax.id._
import scalaz.std.anyVal._

/**
 * In this example, we want two longs to be sorted by `a` first followed by `b`. We assume these are positive only
 * because Dynamo does sorting by treating each byte as unsigned number.
 */
case class TwoLongs(a: Long, b: Long)

object TwoLongs {
  implicit val TwoLongsOrder: Order[TwoLongs] =
    Order.order { (a, b) =>
      Order[Long].apply(a.a, b.a) match {
        case Ordering.EQ => Order[Long].apply(a.b, b.b)
        case o           => o
      }
    }

  implicit val TwoLongsArbitrary: Arbitrary[TwoLongs] =
    Arbitrary {
      for {
        a <- Gen.posNum[Long]
        b <- Gen.posNum[Long]
      } yield TwoLongs(a, b)
    }

  implicit val TwoLongsEncoder: Encoder[TwoLongs] =
    Encoder[ByteBuffer].contramap { longs =>
      // ByteBuffer by default is big-endian (MSB at lowest address) so we want the primary sorting long to
      // come first followed by the secondary
      ByteBuffer.allocateDirect(16).putLong(longs.a).putLong(longs.b) <| { _.rewind() }
    }

  implicit val TwoLongsDecoder: Decoder[TwoLongs] =
    Decoder[ByteBuffer].mapAttempt { bytes =>
      Attempt.safe {
        val buf = bytes.asLongBuffer()
        val a = buf.get()
        val b = buf.get()
        TwoLongs(a, b)
      }
    }
}