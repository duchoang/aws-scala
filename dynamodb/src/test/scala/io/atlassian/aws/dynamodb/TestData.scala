package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.{ MoreEqualsInstances, Arbitraries }
import org.joda.time.DateTime
import org.scalacheck.{ Gen, Arbitrary }
import org.scalacheck.Arbitrary._
import scalaz.Equal, scalaz.std.AllInstances._

object TestData extends Arbitraries with MoreEqualsInstances {
  import Marshaller._, Encoder._, Unmarshaller.Operation._

  object Mapping {
    val key: Column[HashKey] = HashKey.column
    val sequence: Column[RangeKey] = RangeKey.column
    val hash: Column[String] = Column[String]("hash")
    val metaData: Column[String] = Column[String]("metaData")
    val length: Column[Long] = Column[Long]("length")
    val deletedTimestamp: Column[Option[DateTime]] = Column[Option[DateTime]]("deletedTimestamp")
  }

  case class HashKey(a: String, b: String, c: String)
  object HashKey {
    val KeyRegex = "(.*)/(.*)/(.*)".r
    implicit val ThingHashKeyEncoder: Encoder[HashKey] =
      Encoder[String].contramap { k => s"${k.a}/${k.b}/${k.c}" }
    implicit val ThingHashKeyDecoder: Decoder[HashKey] =
      Decoder[String].mapPartial { case KeyRegex(a, b, c) => HashKey(a, b, c) }

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

  case class Value(hash: String, metaData: String, length: Long, deletedTimestamp: Option[DateTime])
  object Value {
    import Mapping._
    val column =
      Column.compose4[Value](hash, metaData, length, deletedTimestamp) { case Value(h, md, len, ts) => (h, md, len, ts) } { Value.apply }

    implicit val storeValue: StoreValue[Value] =
      StoreValue.withUpdated[Value] {
        (o, a) =>
          (o.deletedTimestamp, a.deletedTimestamp) match {
            case (None, Some(DateTimeEncode(deletedTimestamp))) =>
              // If it is a delete just update the deleted timestamp.
              // We probably should create a different value class for deletions (it will be neater)
              StoreValue.updatedFromMap(Map("deletedTimestamp" -> StoreValue.put(deletedTimestamp)))
            case _ => StoreValue.newFromValues(a)(column)
          }
      }
  }

  def thingDynamoMappingForTableName(tableName: String) =
    TableDefinition.from[Key, Value](tableName, "logicalKey", Some(AttributeDefinition.number("seq")), 5, 5)

  implicit def KeyArbitrary: Arbitrary[Key] =
    Arbitrary {
      for {
        a <- Gen.uuid.map { _.toString }
        b <- Gen.uuid.map { _.toString }
        seq <- Gen.chooseNum(Long.MinValue + 1000000, Long.MaxValue - 1000000) // We do some incrementing so give us some buffer around the wraparounds
      } yield Key(a, b, java.util.UUID.randomUUID.toString, seq)
    }

  implicit def ValueArbitrary: Arbitrary[Value] =
    Arbitrary {
      for {
        hash <- Gen.identifier
        metaData <- Gen.identifier
        length <- arbitrary[Long]
        deletedTimestamp <- arbitrary[Option[DateTime]]
      } yield Value(hash, metaData, length, deletedTimestamp)
    }

  implicit def ValueEqual: Equal[Value] = Equal.equal((a, b) =>
    a.hash == b.hash && a.length == b.length &&
      implicitly[Equal[Option[DateTime]]].equal(a.deletedTimestamp, b.deletedTimestamp) &&
      a.metaData == b.metaData
  )
}
