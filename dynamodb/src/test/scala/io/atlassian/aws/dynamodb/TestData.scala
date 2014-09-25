package io.atlassian.aws
package dynamodb

import io.atlassian.aws.spec.{MoreEqualsInstances, Arbitraries}
import org.joda.time.DateTime
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary._
import scalaz.Equal, scalaz.std.AllInstances._

object TestData extends Arbitraries with MoreEqualsInstances {
  import Marshaller._, Encoder._, Unmarshaller.Operation._

  case class ThingKey(tenant: String, app: String, blobId: String, seq: Long)
  case class ThingRangeKey(seq: Long)
  case class ThingHashKey(tenant: String, app: String, blobId: String)
  case class ThingValue(blobHash: String, metaData: String, length: Long, deletedTimestamp: Option[DateTime])

  object Mappings {
    val logicalKey = Column[ThingHashKey]("logicalKey")
    val sequence = Column[ThingRangeKey]("seq")
    val blobHash = Column[String]("blobHash")
    val metaData = Column[String]("metaData")
    val length = Column[Long]("length")
    val deletedTimestamp = Column[Option[DateTime]]("deletedTimestamp")
  }

  import Mappings._

  implicit val ThingHashKeyColumn = logicalKey
  implicit val ThingRangeKeyColumn = sequence

  implicit val ThingHashKeyEncoder: Encoder[ThingHashKey] =
    Encoder.StringEncode.contramap[ThingHashKey] { key =>
      s"${key.tenant}/${key.app}/${key.blobId}"
    }

  implicit val ThingHashKeyDynamoMarshaller =
    Marshaller.fromColumn(logicalKey)
  
  implicit val ThingRangeKeyEncoder: Encoder[ThingRangeKey] =
    Encoder[Long].contramap { _.seq }

  implicit val ThingRangeKeyDynamoMarshaller =
    Marshaller.fromColumn(sequence)
  
  implicit val ThingKeyDynamoMarshaller =
    Marshaller.fromColumn2[ThingHashKey, ThingRangeKey, ThingKey](logicalKey, sequence){ key =>
      (ThingHashKey(key.tenant, key.app, key.blobId), ThingRangeKey(key.seq))
    }

  implicit val ThingKeyDynamoUnmarshaller =
    Unmarshaller.from[ThingKey] {
      for {
        hashKey <- get[String]("logicalKey")
        splitter = """(.+)/(.+)/(.+)""".r
        seq <- get[Long]("seq")
      } yield {
        val splitter(tenant, app, blob) = hashKey
        ThingKey(tenant, app, blob, seq)
      }
    }

  implicit val ThingValueDynamoMarshaller =
    Marshaller.from[ThingValue] { a =>
      Map(
        blobHash(a.blobHash),
        metaData(a.metaData),
        length(a.length),
        deletedTimestamp(a.deletedTimestamp)
      )
    }

  implicit val ThingValueDynamoUnmarshaller = Unmarshaller.from[ThingValue](
    for {
      hash <- blobHash.get
      meta <- metaData.get
      len <- length.get
      del <- deletedTimestamp.get
    } yield ThingValue(hash, meta, len, del)
  )

  implicit def ThingValueDynamoStoreValue: StoreValue[ThingValue] = StoreValue.withUpdated[ThingValue] {
    (o, a) =>
      (o.deletedTimestamp, a.deletedTimestamp) match {
        case (None, Some(DateTimeEncode(deletedTimestamp))) =>
          // If it is a delete just update the deleted timestamp.
          // We probably should create a different value class for deletions (it will be neater)
          StoreValue.updatedFromMap(
            Map("deletedTimestamp" -> StoreValue.put(deletedTimestamp))
          )
        case _ => StoreValue.newFromValues(a)
      }
  }

  def thingDynamoMappingForTableName(tableName: String) = 
    TableDefinition.from[ThingKey, ThingValue](tableName, "logicalKey", Some(AttributeDefinition.number("seq")), 5, 5)

  implicit def ThingKeyArbitrary: Arbitrary[ThingKey] =
    Arbitrary {
      for {
        tenant <- Gen.uuid.map { _.toString }
        app <- Gen.uuid.map { _.toString }
        seq <- Gen.chooseNum(Long.MinValue + 1000000, Long.MaxValue - 1000000)  // We do some incrementing so give us some buffer around the wraparounds
      } yield ThingKey(tenant, app, java.util.UUID.randomUUID.toString, seq)
    }

  implicit def ThingValueArbitrary: Arbitrary[ThingValue] =
    Arbitrary {
      for {
        blobHash <- Gen.identifier
        metaData <- Gen.identifier
        length <- arbitrary[Long]
        deletedTimestamp <- arbitrary[Option[DateTime]]
      } yield ThingValue(blobHash, metaData, length, deletedTimestamp)
    }

  implicit def ThingValueEqual: Equal[ThingValue] = Equal.equal((a, b) =>
    a.blobHash == b.blobHash && a.length == b.length &&
    implicitly[Equal[Option[DateTime]]].equal(a.deletedTimestamp, b.deletedTimestamp) &&
    a.metaData == b.metaData
  )
}
