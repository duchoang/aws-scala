package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.{ MoreEqualsInstances, Arbitraries }
import org.joda.time.DateTime
import org.scalacheck.{ Gen, Arbitrary }
import org.scalacheck.Arbitrary._
import scalaz.Equal, scalaz.std.AllInstances._

object TestData extends Arbitraries with MoreEqualsInstances {
  import Encoder._

  object Mapping {
    val key: Column[HashKey] = HashKey.column
    val sequence: Column[RangeKey] = RangeKey.column
    val hash: Column[String] = Column[String]("hash")
    val metaData: Column[String] = Column[String]("metaData")
    val length: Column[IndexRange] = IndexRange.column
    val deletedTimestamp: Column[Option[DateTime]] = Column[Option[DateTime]]("deletedTimestamp")
  }

  case class HashKey(a: String, b: String, c: String)
  object HashKey {
    val KeyRegex = "(.*)/(.*)/(.*)".r
    implicit val ThingHashKeyEncoder: Encoder[HashKey] =
      Encoder[String].contramap { k => s"${k.a}/${k.b}/${k.c}" }
    implicit val ThingHashKeyDecoder: Decoder[HashKey] =
      Decoder[String] collect decodeHashKey

    private lazy val decodeHashKey: PartialFunction[String, HashKey] = {
      case KeyRegex(a, b, c) => HashKey(a, b, c)
    }

    val column = Column[HashKey]("key")
  }
  case class RangeKey(seq: Long)
  object RangeKey {
    implicit val ThingRangeKeyEncoder: Encoder[RangeKey] =
      Encoder[Long].contramap { _.seq }
    implicit val ThingRangeKeyDecoder: Decoder[RangeKey] =
      Decoder[Long].map { RangeKey.apply }

    val column = Column[RangeKey]("seq")
  }

  case class Key(a: String, b: String, c: String, seq: Long)
  object Key {
    lazy val column =
      Column.compose2[Key](HashKey.column, RangeKey.column) {
        case Key(a, b, c, seq) => (HashKey(a, b, c), RangeKey(seq))
      } {
        case (HashKey(a, b, c), RangeKey(seq)) => Key(a, b, c, seq)
      }
  }

  case class IndexRange(length: Long)
  object IndexRange {
    implicit val ThingRangeKeyEncoder: Encoder[IndexRange] =
      Encoder[Long].contramap { _.length }
    implicit val ThingRangeKeyDecoder: Decoder[IndexRange] =
      Decoder[Long].map { IndexRange.apply }

    val column = Column[IndexRange]("length")
  }

  case class Value(hash: String, metaData: String, length: IndexRange, deletedTimestamp: Option[DateTime])
  object Value {
    import Mapping._
    val column =
      Column.compose4[Value](hash, metaData, length, deletedTimestamp) { case Value(h, md, len, ts) => (h, md, len, ts) } { Value.apply }
  }

  def simpleKeyTableNamed(tableName: String) =
    HashKeyTableDefinition.from[HashKey, Value](tableName, HashKey.column, Value.column)

  def complexKeyTableNamed(tableName: String) =
    HashRangeKeyTableDefinition.from[HashKey, RangeKey, Value](tableName, HashKey.column, RangeKey.column, Value.column)

  def tableWithLSI(tableName: String, indexName: String) =
    HashRangeKeyTableDefinition.withLocalSecondaryIndex(indexName, IndexRange.column, IndexProjection.allProjection[(HashKey, RangeKey), Value])

  implicit val HashKeyArbitrary: Arbitrary[HashKey] =
    Arbitrary {
      for {
        a <- Gen.uuid.map { _.toString }
        b <- Gen.uuid.map { _.toString }
        c <- Gen.uuid.map { _.toString }
      } yield HashKey(a, b, c)
    }

  implicit def KeyArbitrary: Arbitrary[Key] =
    Arbitrary {
      for {
        a <- Gen.uuid.map { _.toString }
        b <- Gen.uuid.map { _.toString }
        c <- Gen.uuid.map { _.toString }
        seq <- Gen.chooseNum(0, Long.MaxValue - 1000000) // We do some incrementing so give us some buffer around the wraparounds
      } yield Key(a, b, c, seq)
    }

  implicit def ValueArbitrary: Arbitrary[Value] =
    Arbitrary {
      for {
        hash <- Gen.identifier
        metaData <- Gen.identifier
        length <- arbitrary[Long]
        deletedTimestamp <- arbitrary[Option[DateTime]]
      } yield Value(hash, metaData, IndexRange(length), deletedTimestamp)
    }

  implicit def ValueEqual: Equal[Value] = Equal.equal((a, b) =>
    a.hash == b.hash && a.length == b.length &&
      implicitly[Equal[Option[DateTime]]].equal(a.deletedTimestamp, b.deletedTimestamp) &&
      a.metaData == b.metaData
  )
}
