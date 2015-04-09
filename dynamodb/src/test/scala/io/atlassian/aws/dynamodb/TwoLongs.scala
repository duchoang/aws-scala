package io.atlassian.aws.dynamodb

import kadai.Attempt
import org.scalacheck.{ Gen, Arbitrary }
import scodec.bits.ByteVector

import scalaz.Order
import scalaz.Ordering
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
    Encoder[NonEmptyByteVector].contramap { longs =>
      ByteVector.fromLong(longs.a) ++ ByteVector.fromLong(longs.b) match {
        case NonEmptyByteVector(b) => b
      }
    }

  implicit val TwoLongsDecoder: Decoder[TwoLongs] =
    Decoder[NonEmptyByteVector].mapAttempt { bytes =>
      if (bytes.bytes.length != 16)
        Attempt.fail(s"Invalid length of byte vector ${bytes.bytes.length}")
      else
        Attempt.safe {
          bytes.bytes.splitAt(8) match {
            case (abytes, bbytes) => TwoLongs(abytes.toLong(), bbytes.toLong())
          }
        }
    }
}