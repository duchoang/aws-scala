package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model.{AttributeDefinition => AttDef, _}
import io.atlassian.aws.dynamodb.Schema.Create.{IndexProjection, Throughput}
import io.atlassian.aws.dynamodb.Schema._
import io.atlassian.aws.spec.ScalaCheckSpec
import org.joda.time.{ DateTime, Instant }
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.specs2.runner.JUnitRunner
import scala.collection.JavaConverters._
import scalaz.NonEmptyList
import scalaz.syntax.id._

@RunWith(classOf[JUnitRunner])
class ConvertSpec extends ScalaCheckSpec {

  def is = s2"""
    Convert should correctly conver the following to their AWS counterparts:
    Throughput                                  => ProvisionedThroughput                                                          $throughput

    Named with no range column                  => a single element List of AttributeDefinition
                                                     [Int]                                                                        ${attributeDefinitionNoRange[Int]}
                                                     [Long]                                                                       ${attributeDefinitionNoRange[Long]}
                                                     [String]                                                                     ${attributeDefinitionNoRange[String]}
                                                     [Instant]                                                                    ${attributeDefinitionNoRange[Instant]}
                                                     [DateTime]                                                                   ${attributeDefinitionNoRange[DateTime]}

    Named with defined range column             => a two-element list of AttributeDefinition
                                                     [Int, Long]                                                                  ${attributeDefinitionWithRange[Int, Long]}
                                                     [String, Instant]                                                            ${attributeDefinitionWithRange[String, Instant]}
                                                     [String, DateTime]                                                           ${attributeDefinitionWithRange[String, DateTime]}

    Named with no range column                  => a single element List of KeySchemaElement
                                                     [Int]                                                                        ${keySchemaElementNoRange[Int]}
                                                     [Long]                                                                       ${keySchemaElementNoRange[Long]}
                                                     [String]                                                                     ${keySchemaElementNoRange[String]}
                                                     [Instant]                                                                    ${keySchemaElementNoRange[Instant]}
                                                     [DateTime]                                                                   ${keySchemaElementNoRange[DateTime]}

    Named with defined range column             => a two-element List of KeySchemaElement
                                                     [Int, Long]                                                                  ${keySchemaElementWithRange[Int, Long]}
                                                     [String, Instant]                                                            ${keySchemaElementWithRange[String, Instant]}
                                                     [String, DateTime]                                                           ${keySchemaElementWithRange[String, DateTime]}

    SimpleTable with no index                   => CreateTableRequest
                                                     [Int]                                                                        ${createSimpleTableRequestNoIndex[Int, Extra]}
                                                     [Long]                                                                       ${createSimpleTableRequestNoIndex[Long, Extra]}
                                                     [String]                                                                     ${createSimpleTableRequestNoIndex[String, Extra]}
                                                     [Instant]                                                                    ${createSimpleTableRequestNoIndex[Instant, Extra]}
                                                     [DateTime]                                                                   ${createSimpleTableRequestNoIndex[DateTime, Extra]}

    ComplexTable with no index                  => CreateTableRequest
                                                     [Int, Long]                                                                  ${createComplexTableRequestNoIndex[Int, Long, Extra]}
                                                     [String, Instant]                                                            ${createComplexTableRequestNoIndex[String, Instant, Extra]}
                                                     [DateTime, Int]                                                              ${createComplexTableRequestNoIndex[DateTime, Int, Extra]}

    IndexProjection                             => Projection                                                                     $projection

    LocalIndexDef                               => LocalSecondaryIndex
                                                     ComplexTable [Int, Long, String]                                             ${localSecondaryIndex[Int, Long, String, Extra]}
                                                     ComplexTable [Instant, DateTime, Int]                                        ${localSecondaryIndex[Instant, DateTime, Int, Extra]}

    GlobalIndexDef                              => GlobalSecondaryIndex
                                                     SimpleTable [Int, Long, String]                                              ${globalSecondaryIndexForSimpleTable[Int, Long, String, Extra]}
                                                     SimpleTable [Instant, DateTime, Long]                                        ${globalSecondaryIndexForSimpleTable[Instant, DateTime, Long, Extra]}
                                                     ComplexTable [Int, String, Long, Instant]                                    ${globalSecondaryIndexForComplexTable[Int, String, Long, Instant, Extra]}
                                                     ComplexTable [DateTime, Long, Int, String]                                   ${globalSecondaryIndexForComplexTable[DateTime, Long, Int, String, Extra]}

    CreateTable with secondary index            => CreateTableRequest
                                                     SimpleTable with Global SI [Int, Long, String]                               ${simpleTableWithGlobalSecondaryIndex[Int, Long, String, Extra]}
                                                     SimpleTable with Global SI [Instant, DateTime, Int]                          ${simpleTableWithGlobalSecondaryIndex[Instant, DateTime, Int, Extra]}
                                                     ComplexTable with Global SI [Int, String, Long, Instant]                     ${complexTableWithGlobalSecondaryIndex[Int, String, Long, Instant, Extra]}
                                                     ComplexTable with Global SI [DateTime, Long, Int, String]                    ${complexTableWithGlobalSecondaryIndex[DateTime, Long, Int, String, Extra]}
                                                     ComplexTable with Local SI [Int, String, Instant]                            ${complexTableWithLocalSecondaryIndex[Int, String, Instant, Extra]}
                                                     ComplexTable with Local SI [Long, Int, DateTime]                             ${complexTableWithLocalSecondaryIndex[Long, Int, DateTime, Extra]}
                                                     ComplexTable with both Global & Local SI [Int, String, Long, Instant]        ${complexTableWithBothTypesOfSecondaryIndices[Int, String, Long, Instant, DateTime, Extra]}
                                                     ComplexTable with both Global & Local SI [DateTime, Long, Int, String]       ${complexTableWithBothTypesOfSecondaryIndices[DateTime, Long, Int, String, Long, Extra]}

    """

  case class Extra(i: Int, l: Long, s: String, t: Instant, d:DateTime)

  case class ColumnAndAttributeNames[A](column: Column[A], attrs: NonEmptyList[String])

  implicit val arbExtraColumnAndAttributes: Arbitrary[ColumnAndAttributeNames[Extra]] = Arbitrary {
    for {
      i <- Gen.uuid map (_.toString)
      l <- Gen.uuid map (_.toString)
      s <- Gen.uuid map (_.toString)
      t <- Gen.uuid map (_.toString)
      d <- Gen.uuid map (_.toString)
    } yield {
      val ic = Column[Int](i)
      val lc = Column[Long](l)
      val sc = Column[String](s)
      val tc = Column[Instant](t)
      val dc = Column[DateTime](d)
      ColumnAndAttributeNames(
        Column.case5(ic.column, lc.column, sc.column, tc.column, dc.column)(Extra.apply, Extra.unapply),
        NonEmptyList(ic.name, lc.name, sc.name, tc.name, dc.name)
      )
    }
  }

  implicit val arbExtraColumn: Arbitrary[Column[Extra]] = Arbitrary {
    arbExtraColumnAndAttributes.arbitrary map (_.column)
  }

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
    Arbitrary { Gen.uuid map (uuid => Column[T](uuid.toString)) }

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

  def createSimpleTableRequestNoIndex[K: Arbitrary: Encoder: Decoder, XX](implicit ev: Arbitrary[Column[XX]]) =
    forAll { (tableName: String, key: NamedColumn[K], extra: Column[XX], throughput: Throughput) =>
      case class V(k: K, xx: XX)
      val value = Column.case2(key.column, extra)(V.apply, V.unapply)
      val table = SimpleKeyTable(tableName, key, value)

      val createTable = Create.simple(table, throughput)
      createTable.convertTo[CreateTableRequest] must_===
        new CreateTableRequest(tableName, List(new KeySchemaElement(key.name, KeyType.HASH)).asJava).
          withAttributeDefinitions(List(key.dynamoType).asJava).
          withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput])
    }

  def createComplexTableRequestNoIndex[
    H: Arbitrary: Encoder: Decoder,
    R: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[Column[XX]]) = {
    forAll { (tableName: String, hash: NamedColumn[H], range: NamedColumn[R], extra: Column[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, xx: XX)
      val key = Column.compose2[K](hash.column, range.column)(identity)(Tuple2.apply)
      val value = Column.case2(key, extra)(V.apply, V.unapply)
      val table = ComplexKeyTable(tableName, Named(hash, range), value)

      val createTable = Create.complexKey(table, throughput)
      createTable.convertTo[CreateTableRequest] must_===
        new CreateTableRequest(tableName, List(new KeySchemaElement(hash.name, KeyType.HASH), new KeySchemaElement(range.name, KeyType.RANGE)).asJava).
          withAttributeDefinitions(List(hash.dynamoType, range.dynamoType).asJava).
          withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput])
    }
  }

  implicit def genNonEmptyList[A](genA: Gen[A]): Gen[NonEmptyList[A]] =
    for {
      head <- genA
      tail <- Gen.listOf(genA)
    } yield NonEmptyList.nel(head, tail)

  implicit def arbNonEmptyList[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary { genNonEmptyList(arbitrary[A]) }

  val genPartialIndexProjection: Gen[IndexProjection.Partial] =
    arbitrary[NonEmptyList[String]] map IndexProjection.Partial

  implicit val arbIndexProjection: Arbitrary[IndexProjection] =
    Arbitrary { Gen.oneOf(Gen.oneOf(IndexProjection.all, IndexProjection.keyOnly), genPartialIndexProjection) }

  def projection =
    forAll { (indexProjection: IndexProjection) =>
      indexProjection.convertTo[Projection] must_=== {
        indexProjection match {
          case IndexProjection.All => new Projection().withProjectionType(ProjectionType.ALL)
          case IndexProjection.KeyOnly => new Projection().withProjectionType(ProjectionType.KEYS_ONLY)
          case IndexProjection.Partial(nel) =>
            new Projection().withProjectionType(ProjectionType.INCLUDE).withNonKeyAttributes(nel.list.asJavaCollection)
        }
      }
    }

  sealed trait ProjectionAndAttributeNames {
    type View
    val projection: IndexProjection
    val column: Column[View]
    val attributeNames: Option[NonEmptyList[String]]
  }

  private def genProjectionAndAttributeNames[K, V, XX](kv: Schema.KeyValue[K, V], extra: ColumnAndAttributeNames[XX]): Gen[ProjectionAndAttributeNames] =
    Gen.oneOf(
      new ProjectionAndAttributeNames {
        type View = V
        val projection = IndexProjection.All
        val column = kv.value
        val attributeNames = None
      },
      new ProjectionAndAttributeNames {
        type View = K
        val projection = IndexProjection.KeyOnly
        val column = kv.key
        val attributeNames = None
      },
      new ProjectionAndAttributeNames {
        type View = XX
        val projection = IndexProjection.Partial(extra.attrs)
        val column = extra.column
        val attributeNames = Some(extra.attrs)
      }
    )

  def localSecondaryIndex[
    H: Arbitrary: Encoder: Decoder,
    R: Arbitrary: Encoder: Decoder,
    RR: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (tableName: String, indexName: String, hash: NamedColumn[H], range: NamedColumn[R], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, rr: RR, xx: XX)
      val key = Column.compose2[K](hash.column, range.column)(identity)(Tuple2.apply)
      val value = Column.case3(key, indexRange.column, extra.column)(V.apply, V.unapply)
      val table = ComplexKeyTable(tableName, Named(hash, range), value)
      val indexHashRange = Named(hash, indexRange)

      forAll(genProjectionAndAttributeNames(table.kv, extra)) { projection =>
        val index = table.deriveLocalIndex(indexName, indexRange, projection.column)
        Create.complexKey(table, throughput).addLocalIndex(
          index,
          projection.projection
        ).localIndexes.headOption.map(_.convertTo[LocalSecondaryIndex]) must beSome(
          new LocalSecondaryIndex()
            .withIndexName(indexName)
            .withKeySchema(indexHashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
            .withProjection(projection.projection.convertTo[Projection])
        )
      }
    }
  }

  def globalSecondaryIndexForSimpleTable[
    K: Arbitrary: Encoder: Decoder,
    HH: Arbitrary: Encoder: Decoder,
    RR: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (tableName: String, indexName: String, key: NamedColumn[K], indexHash: NamedColumn[HH], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      case class V(k: K, hh: HH, rr: RR, xx: XX)
      val value = Column.case4(key.column, indexHash.column, indexRange.column, extra.column)(V.apply, V.unapply)
      val table = SimpleKeyTable(tableName, key, value)
      val indexHashRange = Named(indexHash, indexRange)

      globalSecondaryIndex(Create.simple(table, throughput), indexName, indexHashRange, extra)
    }
  }

  def globalSecondaryIndexForComplexTable[
    H: Arbitrary: Encoder: Decoder,
    R: Arbitrary: Encoder: Decoder,
    HH: Arbitrary: Encoder: Decoder,
    RR: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (tableName: String, indexName: String, hash: NamedColumn[H], range: NamedColumn[R], indexHash: NamedColumn[HH], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, hh: HH, rr: RR, xx: XX)
      val key = Column.compose2[K](hash.column, range.column)(identity)(Tuple2.apply)
      val value = Column.case4(key, indexHash.column, indexRange.column, extra.column)(V.apply, V.unapply)
      val table = ComplexKeyTable(tableName, Named(hash, range), value)
      val indexHashRange = Named(indexHash, indexRange)

      globalSecondaryIndex(Create.complexKey(table, throughput), indexName, indexHashRange, extra)
    }
  }

  private def makeGlobalSecondaryIndex[HH, RR](indexName: String, indexHashRange: Named[HH, RR], indexProjection: IndexProjection, indexThroughput: Throughput): GlobalSecondaryIndex =
    new GlobalSecondaryIndex()
      .withIndexName(indexName)
      .withKeySchema(indexHashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
      .withProjection(indexProjection.convertTo[Projection])
      .withProvisionedThroughput(indexThroughput.convertTo[ProvisionedThroughput])

  private def makeLocalSecondaryIndex[HH, RR](indexName: String, indexHashRange: Named[HH, RR], indexProjection: IndexProjection): LocalSecondaryIndex =
    new LocalSecondaryIndex()
      .withIndexName(indexName)
      .withKeySchema(indexHashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
      .withProjection(indexProjection.convertTo[Projection])

  private def globalSecondaryIndex[K, V, H, R, VV, HH, RR, XX](table: Create.CreateTable[K, V, H, R], indexName: String, indexHashRange: Named[HH, RR], extra: ColumnAndAttributeNames[XX]) =
    forAll(genProjectionAndAttributeNames(table.tableSchema.kv, extra), arbitrary[Throughput]) { (projection, indexThroughput) =>
      val index = table.tableSchema.deriveGlobalIndex(indexName, indexHashRange, projection.column)
      table.addGlobalIndex(
        index,
        projection.projection,
        indexThroughput
      ).globalIndexes.headOption.map(_.convertTo[GlobalSecondaryIndex]) must beSome(
        makeGlobalSecondaryIndex(index.name, indexHashRange, projection.projection, indexThroughput)
      )
    }

  private def makeCreateTableRequest[H, R](tableName: String, attDefs: List[AttDef], hashRange: Named[H, R], gsis: List[GlobalSecondaryIndex], lsis: List[LocalSecondaryIndex], throughput: Throughput): CreateTableRequest =
    new CreateTableRequest().withTableName(tableName) <| { req =>
      req.withAttributeDefinitions(attDefs.asJavaCollection)
        .withKeySchema(hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
        .withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput])
      if (gsis.nonEmpty)
        req.withGlobalSecondaryIndexes(gsis.asJavaCollection)
      if (lsis.nonEmpty)
        req.withLocalSecondaryIndexes(lsis.asJavaCollection)
    }

  def simpleTableWithGlobalSecondaryIndex[
      K: Arbitrary: Encoder: Decoder,
      HH: Arbitrary: Encoder: Decoder,
      RR: Arbitrary: Encoder: Decoder,
      XX
    ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
      forAll { (tableName: String, indexName: String, key: NamedColumn[K], indexHash: NamedColumn[HH], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
        case class V(k: K, hh: HH, rr: RR, xx: XX)
        val value = Column.case4(key.column, indexHash.column, indexRange.column, extra.column)(V.apply, V.unapply)
        val table = SimpleKeyTable(tableName, key, value)
        val indexHashRange = Named(indexHash, indexRange)

        val createTable = Create.simple(table, throughput)
        forAll(genProjectionAndAttributeNames(createTable.tableSchema.kv, extra), arbitrary[Throughput]) { (projection, indexThroughput) =>
          val index = createTable.tableSchema.deriveGlobalIndex(indexName, indexHashRange, projection.column)
          val expectedAttDefs = List(key.dynamoType, indexHash.dynamoType, indexRange.dynamoType).distinct
          val expectedGlobalSecondaryIndex = makeGlobalSecondaryIndex(indexName, indexHashRange, projection.projection, indexThroughput)
          createTable.addGlobalIndex(
            index,
            projection.projection,
            indexThroughput
          ).convertTo[CreateTableRequest] must_===
          makeCreateTableRequest(tableName, expectedAttDefs, table.hashRange, List(expectedGlobalSecondaryIndex), List.empty, throughput)
        }
      }
    }

  def complexTableWithGlobalSecondaryIndex[
      H: Arbitrary: Encoder: Decoder,
      R: Arbitrary: Encoder: Decoder,
      HH: Arbitrary: Encoder: Decoder,
      RR: Arbitrary: Encoder: Decoder,
      XX
    ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (tableName: String, indexName: String, hash: NamedColumn[H], range: NamedColumn[R], indexHash: NamedColumn[HH], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, hh: HH, rr: RR, xx: XX)
      val key = Column.compose2[K](hash.column, range.column)(identity)(Tuple2.apply)
      val value = Column.case4(key, indexHash.column, indexRange.column, extra.column)(V.apply, V.unapply)
      val table = ComplexKeyTable(tableName, Named(hash, range), value)
      val indexHashRange = Named(indexHash, indexRange)

//      globalSecondaryIndex(Create.complexKey(table, throughput), indexName, indexHashRange, extra)
      val createTable = Create.complexKey(table, throughput)
      forAll(genProjectionAndAttributeNames(createTable.tableSchema.kv, extra), arbitrary[Throughput]) { (projection, indexThroughput) =>
        val index = createTable.tableSchema.deriveGlobalIndex(indexName, indexHashRange, projection.column)
        val expectedAttDefs = List(hash.dynamoType, range.dynamoType, indexHash.dynamoType, indexRange.dynamoType).distinct
        val expectedGlobalSecondaryIndex = makeGlobalSecondaryIndex(indexName, indexHashRange, projection.projection, indexThroughput)
        createTable.addGlobalIndex(
          index,
          projection.projection,
          indexThroughput
        ).convertTo[CreateTableRequest] must_===
          makeCreateTableRequest(tableName, expectedAttDefs, table.hashRange, List(expectedGlobalSecondaryIndex), List.empty, throughput)
      }
    }
  }

  def complexTableWithLocalSecondaryIndex[
    H: Arbitrary: Encoder: Decoder,
    R: Arbitrary: Encoder: Decoder,
    RR: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (tableName: String, indexName: String, hash: NamedColumn[H], range: NamedColumn[R], indexRange: NamedColumn[RR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, rr: RR, xx: XX)
      val key = Column.compose2[K](hash.column, range.column)(identity)(Tuple2.apply)
      val value = Column.case3(key, indexRange.column, extra.column)(V.apply, V.unapply)
      val table = ComplexKeyTable(tableName, Named(hash, range), value)
      val indexHashRange = Named(hash, indexRange)

      val createTable = Create.complexKey(table, throughput)
      forAll(genProjectionAndAttributeNames(createTable.tableSchema.kv, extra)) { projection =>
        val index = createTable.tableSchema.deriveLocalIndex(indexName, indexRange, projection.column)
        val expectedAttDefs = List(hash.dynamoType, range.dynamoType, indexRange.dynamoType).distinct
        val expectedLocalSecondaryIndex = makeLocalSecondaryIndex(indexName, indexHashRange, projection.projection)
        createTable.addLocalIndex(
          index,
          projection.projection
        ).convertTo[CreateTableRequest] must_===
          makeCreateTableRequest(tableName, expectedAttDefs, table.hashRange, List.empty, List(expectedLocalSecondaryIndex), throughput)
      }
    }
  }

  case class ComplexTableFixture[H: Encoder: Decoder, R: Encoder: Decoder](name: String, hash: NamedColumn[H], range: NamedColumn[R])
  case class GlobalIndexFixture[H: Encoder: Decoder, R: Encoder: Decoder](name: String, hash: NamedColumn[H], range: NamedColumn[R])
  case class LocalIndexFixture[R: Encoder: Decoder](name: String, range: NamedColumn[R])

  implicit def arbComplexTableFixture[H: Encoder: Decoder, R: Encoder: Decoder] = Arbitrary {
    for {
      name <- Gen.uuid map (_.toString)
      hash <- arbitrary[NamedColumn[H]]
      range <- arbitrary[NamedColumn[R]]
    } yield ComplexTableFixture[H, R](name, hash, range)
  }

  implicit def arbGlobalIndexFixture[H: Encoder: Decoder, R: Encoder: Decoder] = Arbitrary {
    for {
      name <- Gen.uuid map (_.toString)
      hash <- arbitrary[NamedColumn[H]]
      range <- arbitrary[NamedColumn[R]]
    } yield GlobalIndexFixture[H, R](name, hash, range)
  }

  implicit def arbLocalIndexFixture[R: Encoder: Decoder] = Arbitrary {
    for {
      name <- Gen.uuid map (_.toString)
      range <- arbitrary[NamedColumn[R]]
    } yield LocalIndexFixture[R](name, range)
  }

  def complexTableWithBothTypesOfSecondaryIndices[
    H: Arbitrary: Encoder: Decoder,
    R: Arbitrary: Encoder: Decoder,
    GH: Arbitrary: Encoder: Decoder,
    GR: Arbitrary: Encoder: Decoder,
    LR: Arbitrary: Encoder: Decoder,
    XX
  ](implicit ev: Arbitrary[ColumnAndAttributeNames[XX]]) = {
    forAll { (complexTableFixture: ComplexTableFixture[H, R], globalIndexFixture: GlobalIndexFixture[GH, GR], localIndexFixture: LocalIndexFixture[LR], extra: ColumnAndAttributeNames[XX], throughput: Throughput) =>
      type K = (H, R)
      case class V(k: K, gh: GH, gr: GR, lr: LR, xx: XX)
      val key = Column.compose2[K](complexTableFixture.hash.column, complexTableFixture.range.column)(identity)(Tuple2.apply)
      val value = Column.case5(key, globalIndexFixture.hash.column, globalIndexFixture.range.column, localIndexFixture.range.column, extra.column)(V.apply, V.unapply)
      val table = ComplexKeyTable(complexTableFixture.name, Named(complexTableFixture.hash, complexTableFixture.range), value)
      val globalIndexHashRange = Named(globalIndexFixture.hash, globalIndexFixture.range)
      val localIndexHashRange = Named(complexTableFixture.hash, localIndexFixture.range)

      val createTable = Create.complexKey(table, throughput)
      forAll(genProjectionAndAttributeNames(createTable.tableSchema.kv, extra), genProjectionAndAttributeNames(createTable.tableSchema.kv, extra), arbitrary[Throughput]) { (localProjection, globalProjection, indexThroughput) =>
        val localIndex = createTable.tableSchema.deriveLocalIndex(localIndexFixture.name, localIndexFixture.range, localProjection.column)
        val globalIndex = createTable.tableSchema.deriveGlobalIndex(globalIndexFixture.name, globalIndexHashRange, globalProjection.column)
        val expectedAttDefs = List(
          complexTableFixture.hash.dynamoType, complexTableFixture.range.dynamoType,
          localIndexFixture.range.dynamoType,
            globalIndexFixture.hash.dynamoType, globalIndexFixture.range.dynamoType
        ).distinct
        val expectedLocalSecondaryIndex = makeLocalSecondaryIndex(localIndexFixture.name, localIndexHashRange, localProjection.projection)
        val expectedGlobalSecondaryIndex = makeGlobalSecondaryIndex(globalIndexFixture.name, globalIndexHashRange, globalProjection.projection, indexThroughput)
        createTable.addGlobalIndex(
          globalIndex,
          globalProjection.projection,
          indexThroughput
        ).addLocalIndex(
          localIndex,
          localProjection.projection
        ).convertTo[CreateTableRequest] must_===
          makeCreateTableRequest(complexTableFixture.name, expectedAttDefs, table.hashRange, List(expectedGlobalSecondaryIndex), List(expectedLocalSecondaryIndex), throughput)
      }
    }
  }
}
