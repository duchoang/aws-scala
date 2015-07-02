package io.atlassian.aws
package dynamodb

import io.atlassian.aws.dynamodb.Schema.Create.Throughput
import io.atlassian.aws.dynamodb.Schema.SimpleKeyTable
import io.atlassian.aws.spec.{ MoreEqualsInstances, Arbitraries }
import org.joda.time.DateTime
import org.scalacheck.{ Gen, Arbitrary }
import org.scalacheck.Arbitrary._
import scalaz.Equal, scalaz.std.AllInstances._
import scalaz.Isomorphism.<=>
import scalaz.syntax.equal._

object TestData extends Arbitraries with MoreEqualsInstances {

  import Encoder._

  val defaultThroughput = Throughput(5, 5)

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
      Encoder[Long].contramap {
        _.seq
      }
    implicit val ThingRangeKeyDecoder: Decoder[RangeKey] =
      Decoder[Long].map {
        RangeKey.apply
      }

    val column = Column[RangeKey]("seq")
  }

  case class Key(a: String, b: String, c: String, seq: Long)

  object Key {
    lazy val column =
      Column.compose2[Key](HashKey.column, RangeKey.column)(Iso.to)(Function.untupled(Iso.from))

    implicit val Iso: Key <=> (HashKey, RangeKey) = new (Key <=> (HashKey, RangeKey)) {
      def from = { case (HashKey(a, b, c), RangeKey(d)) => Key(a, b, c, d) }
      def to = { case Key(a, b, c, d) => (HashKey(a, b, c), RangeKey(d)) }
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
      Column.compose4[Value](hash, metaData, length, deletedTimestamp) { case Value(h, md, len, ts) => (h, md, len, ts) } {
        Value.apply
      }
  }

  case class KeyValue(key: HashKey, value: Value)

  object KeyValue {
    lazy val column =
      Column.compose2[KeyValue](HashKey.column, Value.column)((KeyValue.unapply _) andThen (_.get))(KeyValue.apply _)
  }

  def simpleKeyTableNamed(tableName: String): SimpleKeyTable[HashKey, KeyValue] =
    Schema.SimpleKeyTable(tableName, HashKey.column, KeyValue.column)

  def defineSchema(name: String, t: Table.ComplexKey)(kc: Column[t.K], vc: Column[t.V], hc: NamedColumn[t.H], rc: NamedColumn[t.R]): Schema.Standard[t.K, t.V, t.H, t.R] =
    Schema.ComplexKeyTable(name, Schema.Named(hc, rc), vc)(t.keyIso)

  implicit val HashKeyArbitrary: Arbitrary[HashKey] =
    Arbitrary {
      for {
        a <- Gen.uuid
        b <- Gen.uuid
        c <- Gen.uuid
      } yield HashKey(a.toString, b.toString, c.toString)
    }

  implicit def KeyArbitrary: Arbitrary[Key] =
    Arbitrary {
      for {
        a <- Gen.uuid
        b <- Gen.uuid
        c <- Gen.uuid
        seq <- Gen.chooseNum(0, Long.MaxValue - 1000000) // We do some incrementing so give us some buffer around the wraparounds
      } yield Key(a.toString, b.toString, c.toString, seq)
    }

  implicit def ValueArbitrary: Arbitrary[Value] =
    Arbitrary {
      for {
        hash <- Gen.identifier
        metaData <- Gen.identifier
        length <- Gen.chooseNum(0, Long.MaxValue - 1000000) // We do some incrementing so give us some buffer around the wraparounds
        deletedTimestamp <- arbitrary[Option[DateTime]]
      } yield Value(hash, metaData, IndexRange(length), deletedTimestamp)
    }

  implicit def ValueEqual: Equal[Value] = Equal.equal((a, b) =>
    a.hash == b.hash && a.length == b.length &&
      implicitly[Equal[Option[DateTime]]].equal(a.deletedTimestamp, b.deletedTimestamp) &&
      a.metaData == b.metaData)

  implicit def KeyValueArbitrary: Arbitrary[KeyValue] = Arbitrary {
    for {
      key <- arbitrary[HashKey]
      value <- arbitrary[Value]
    } yield KeyValue(key, value)
  }

  implicit def HashKeyEqual: Equal[HashKey] = Equal.equal { (s, t) =>
    s.a === t.a && s.b === t.b && s.c === t.c
  }

  implicit def KeyValueEqual: Equal[KeyValue] = Equal.equal { (s, t) =>
    s.key === t.key && s.value === t.value
  }
}
