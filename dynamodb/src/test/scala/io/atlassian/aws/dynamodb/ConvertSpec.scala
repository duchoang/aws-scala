package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.{AttributeDefinition => AttDef, _}
import io.atlassian.aws.dynamodb.Schema.Create.{LocalIndexDef, IndexProjection, Throughput}
import io.atlassian.aws.dynamodb.Schema._
import io.atlassian.aws.spec.ScalaCheckSpec
import org.joda.time.{ DateTime, Instant }
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.runner.JUnitRunner
import scala.collection.JavaConverters._
import scalaz.Isomorphism.<=>

@RunWith(classOf[JUnitRunner])
class ConvertSpec extends ScalaCheckSpec {

  def is = """
    Convert should correctly conver the following to their AWS counterparts:
    Throughput                                  => ProvisionedThroughput                          $throughput

    Named with no range column                  => a single element List of AttributeDefinition
                                                     [Int]                                        ${attributeDefinitionNoRange[Int]}
                                                     [Long]                                       ${attributeDefinitionNoRange[Long]}
                                                     [String]                                     ${attributeDefinitionNoRange[String]}
                                                     [Instant]                                    ${attributeDefinitionNoRange[Instant]}
                                                     [DateTime]                                   ${attributeDefinitionNoRange[DateTime]}

    Named with defined range column             => a two-element list of AttributeDefinition
                                                     [Int, Long]                                  ${attributeDefinitionWithRange[Int, Long]}
                                                     [String, Instant]                            ${attributeDefinitionWithRange[String, Instant]}
                                                     [String, DateTime]                           ${attributeDefinitionWithRange[String, DateTime]}

    Named with no range column                  => a single element List of KeySchemaElement
                                                     [Int]                                        ${keySchemaElementNoRange[Int]}
                                                     [Long]                                       ${keySchemaElementNoRange[Long]}
                                                     [String]                                     ${keySchemaElementNoRange[String]}
                                                     [Instant]                                    ${keySchemaElementNoRange[Instant]}
                                                     [DateTime]                                   ${keySchemaElementNoRange[DateTime]}

    Named with defined range column             => a two-element List of KeySchemaElement
                                                     [Int, Long]                                  ${keySchemaElementWithRange[Int, Long]}
                                                     [String, Instant]                            ${keySchemaElementWithRange[String, Instant]}
                                                     [String, DateTime]                           ${keySchemaElementWithRange[String, DateTime]}

    IndexProjection                             => Projection                                     $projection
    LocalIndexDef                               => LocalSecondaryIndex                            localSecondaryIndex
    GlobalIndexDef                              => GlobalSecondaryIndex                           globalSecondaryIndex
    TableCreate with no range column            => CreateTableRequest                             createTableRequestNoRange
    TableCreate with defined range column       => CreateTableRequest                             createTableRequestWithRange
    TableCreate with secondary indexes          => CreateTableRequest                             createTableRequestWithSI

    ???                                         => UpdateItemRequest                              updateItemRequest
    """

  import Convert._

  implicit val arbThroughput =
    Arbitrary {
      for {
        read <- Gen.posNum[Long]
        write <- Gen.posNum[Long]
      } yield Throughput(read, write)
    }

  def throughput =
    forAll { (throughput: Throughput) =>
      throughput.convertTo[ProvisionedThroughput] must_=== new ProvisionedThroughput(throughput.read, throughput.write)
    }

  implicit def arbNamedColumn[T: Encoder: Decoder] =
    Arbitrary { arbitrary[String] map Column[T] }

  def attributeDefinitionNoRange[H: Arbitrary: Encoder: Decoder] =
    forAll { (hash: NamedColumn[H]) =>
      Named[H, Nothing](hash, Column.NoColumn).convertTo[List[AttDef]] must_=== List(hash.dynamoType)
    }

  def attributeDefinitionWithRange[H: Arbitrary: Encoder: Decoder, R: Arbitrary: Encoder: Decoder] =
    forAll { (hash: NamedColumn[H], range: NamedColumn[R]) =>
      Named(hash, range).convertTo[List[AttDef]] must_=== List(hash.dynamoType, range.dynamoType)
    }

  def keySchemaElementNoRange[H: Arbitrary: Encoder: Decoder] =
    forAll { (hash: NamedColumn[H]) =>
      Named[H, Nothing](hash, Column.NoColumn).convertTo[List[KeySchemaElement]] must_===
        List(new KeySchemaElement(hash.name, KeyType.HASH))
    }

  def keySchemaElementWithRange[H: Arbitrary: Encoder: Decoder, R: Arbitrary: Encoder: Decoder] =
    forAll { (hash: NamedColumn[H], range: NamedColumn[R]) =>
      Named(hash, range).convertTo[List[KeySchemaElement]] must_===
        List(new KeySchemaElement(hash.name, KeyType.HASH), new KeySchemaElement(range.name, KeyType.RANGE))
    }

  val genPartialIndexProjection =
    arbitrary[List[String]] map IndexProjection.Partial

  implicit val arbIndexProjection: Arbitrary[IndexProjection] =
    Arbitrary { Gen.oneOf(Gen.oneOf(IndexProjection.All, IndexProjection.KeyOnly), genPartialIndexProjection) }

  def projection =
    forAll { (indexProjection: IndexProjection) =>
      indexProjection.convertTo[Projection] must_=== {
        indexProjection match {
          case IndexProjection.All => new Projection().withProjectionType(ProjectionType.ALL)
          case IndexProjection.KeyOnly => new Projection().withProjectionType(ProjectionType.KEYS_ONLY)
          case IndexProjection.Partial(Nil) => new Projection().withProjectionType(ProjectionType.INCLUDE)
          case IndexProjection.Partial(list) =>
            new Projection().withProjectionType(ProjectionType.INCLUDE).withNonKeyAttributes(list.asJavaCollection)
        }
      }
    }

  import Encoder._

  trait ColumnMaker {
    type T
    val column: Column[T]
  }

  trait NamedColumnBlueprint {
    type T
    implicit val encoder: Encoder[T] = implicitly[Encoder[T]]
    implicit val decoder: Decoder[T] = implicitly[Decoder[T]]
    def column(name: String): NamedColumn[T] = Column(name)
  }

  object NamedColumnBlueprint {
    def apply[A: Encoder: Decoder]: NamedColumnBlueprint = new NamedColumnBlueprint {
      type T = A
    }
  }

  val NamedColumnMakers: List[NamedColumnBlueprint] = List(
    NamedColumnBlueprint[Int],
    NamedColumnBlueprint[Long],
    NamedColumnBlueprint[String],
    NamedColumnBlueprint[Instant],
    NamedColumnBlueprint[DateTime]
  )



  trait SimpleKeyTableMaker {
    val keyColumn: NamedColumnMaker
    val dataColumn: ColumnMaker
    val extraColumn: ColumnMaker
    val globalIndexHashColumn: NamedColumnMaker
    val globalIndexRangeColumn: NamedColumnMaker
    case class Value(key: keyColumn.T, data: dataColumn.T, extra: extraColumn.T, globalIndexHash: globalIndexHashColumn.T, globalIndexRange: globalIndexRangeColumn.T)
    val table: SimpleKeyTable[keyColumn.T, Value]

    def globalIndexKeyProjection(name: String): SecondaryIndex[K, GV, GH, GR] =
      table.deriveGlobalIndex(name, Named(globalIndexHashColumn.column(s"${name}_GI_hash"), globalIndexRangeColumn.column(s"${name}_GI_range")), )
  }

  val genSimpleTableMaker: Gen[SimpleKeyTableMaker]

  case class IntValue(value: Int)
  case class LongValue(value: Long)
  case class StringValue(value: String)
  case class InstantValue(value: Instant)
  case class DateTimeValue(value: DateTime)


  case class Hash(hash: Long)
  case class PrimaryRange(range: Long)
  case class SecondaryRange(range: String)
  case class OtherHash(hash: String)
  case class Value(hash: Hash, range1: PrimaryRange, range2: SecondaryRange, otherHash: OtherHash, data: String)
  
  val ColumnHashKey: NamedColumn[Hash] = Column[Hash]("hash")
  val ColumnPrimaryRange: NamedColumn[PrimaryRange] = Column[PrimaryRange]("range1")
  val ColumnSecondaryRange: NamedColumn[SecondaryRange] = Column[SecondaryRange]("range2")
  val ColumnOtherHash: NamedColumn[OtherHash] = Column[OtherHash]("other_hash")
  val ColumnData: NamedColumn[String] = Column[String]("data")
  val ColumnValue: Column[Value] = Column.case5[Value](ColumnHashKey.column, ColumnPrimaryRange.column, ColumnSecondaryRange.column, ColumnOtherHash.column, ColumnData.column)(Value.apply, Value.unapply)

  val simpleKeyTable: SimpleKeyTable[Hash, Value] = SimpleKeyTable("simple", ColumnHashKey, ColumnValue)

  case class ComplexKey(hash: Hash, range: PrimaryRange)
  implicit val ComplexKeyIso: ComplexKey <=> (Hash, PrimaryRange) = new (ComplexKey <=> (Hash, PrimaryRange)) {
    override def from: ((Hash, PrimaryRange)) => ComplexKey = (tuple) => ComplexKey(tuple._1, tuple._2)
    override def to: (ComplexKey) => (Hash, PrimaryRange) = c => (c.hash, c.range)
  }
  val complexKeyTable: ComplexKeyTable[ComplexKey, Value, Hash, PrimaryRange] = ComplexKeyTable("complex", Named(ColumnHashKey, ColumnPrimaryRange), ColumnValue)
  
  val simpleGlobalIndex = simpleKeyTable.deriveGlobalIndex("simpleGI", Named(ColumnOtherHash, ColumnSecondaryRange), ColumnHashKey.column)
  val complexGlobalIndex = complexKeyTable.deriveGlobalIndex("complexGI", Named(ColumnOtherHash, ColumnSecondaryRange), ColumnValue)
  val complexLocalIndex = complexKeyTable.deriveLocalIndex("simpleGI", ColumnSecondaryRange, ColumnValue)

//  def localSecondaryIndex = {
//    Create.CreateTable()
//  }
}
