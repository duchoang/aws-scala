package io.atlassian.aws
package dynamodb

import scalaz.Isomorphism.<=>

// TODO: renamed the file to Schema.scala
object Schema {
  case class KeyValue[K, V](name: String, key: Column[K], value: Column[V])

  case class Named[A, B](a: NamedColumn[A], b: NamedColumn[B]) {
    val tupled: Column[(A, B)] = Column.compose2[(A, B)](a, b)(identity)(Tuple2.apply)
  }

  /**
   * An index contains the optional index name (none for primary index), the key-value table that is indexed and
   * the hash-range used for indexing.
   */
  sealed trait Index[K, V, H, R] {
    val indexName: Option[String]
    val kv: KeyValue[K, V]
    val hashRange: Named[H, R]
  }

  /**
   * A secondary index.
   *
   * @param name the mandatory name of the secondary index.
   * @param kv the key-value table view that is indexed. Note that the value may be just a part of the original table's value.
   * @param hashRange the hash-range used for indexing
   * @tparam K the key type of the key-value table
   * @tparam V the value type of the key-value table
   * @tparam H the hash type of the hash-range key
   * @tparam R the range type of the hash-range key
   */
  case class SecondaryIndex[K, V, H, R](name: String, kv: KeyValue[K, V], hashRange: Named[H, R]) extends Index[K, V, H, R] {
    val indexName: Option[String] = Some(name)
  }

  /**
   * A complex-keyed table consists of a key-value table and a primary index on the key-value table.
   */
  case class Standard[K, V, H, R] private[Schema] (
    kv: KeyValue[K, V],
    hashRange: Named[H, R]) extends Index[K, V, H, R] {
    val tableName = kv.name
    val indexName: Option[String] = None

    private def projected[VV](projection: Column[VV]): KeyValue[K, VV] =
      KeyValue[K, VV](kv.name, kv.key, projection)

    /**
     * Derives a global secondary index view indexed by the given hash-range key and the projection
     *
     * @param indexName name of the global secondary index
     * @param indexHashRange the hash-range key used by the index
     * @param projection the projection of the index
     * @tparam VV the value type of the projection
     * @tparam HH the hash type of the index hash-range key
     * @tparam RR the range type of the index hash-range key
     * @return
     */
    def deriveGlobalIndex[VV, HH, RR](indexName: String, indexHashRange: Named[HH, RR], projection: Column[VV]): SecondaryIndex[K, VV, HH, RR] =
      SecondaryIndex[K, VV, HH, RR](
        indexName,
        projected(projection),
        indexHashRange
      )

    /**
     * Derives a local secondary index view indexed by the table's hash and the given range key and the given projection
     *
     * @param indexName name of the local secondary index
     * @param indexRange the range key used by the index together with the table's hash key
     * @param projection the projection of the index
     * @tparam VV the value type of the projection
     * @tparam RR the range type of the index range key
     * @return
     */
    def deriveLocalIndex[VV, RR](indexName: String, indexRange: NamedColumn[RR], projection: Column[VV]): SecondaryIndex[K, VV, H, RR] =
      SecondaryIndex[K, VV, H, RR](
        indexName,
        projected(projection),
        Named[H, RR](hashRange.a, indexRange)
      )
  }

  type ComplexKeyTable[K, V, H, R] = Standard[K, V, H, R]

  /**
   * Defines a complex key table by providing the hash-range key named columns and the value column.
   *
   * The key column is derived from the hash-range key columns. The primary key consists of the hash key and the range key.
   */
  object ComplexKeyTable {
    def apply[K, V, H, R](name: String, hashRange: Named[H, R], value: Column[V])(keyIso: K <=> (H, R)) =
      Standard[K, V, H, R](KeyValue(name, hashRange.tupled.xmap(keyIso.from, keyIso.to), value), hashRange)
  }

  type SimpleKeyTable[K, V] = Standard[K, V, K, Nothing]

  /**
   * Defines a simple key table. The key column must be a named hash key.
   */
  object SimpleKeyTable {
    def apply[K, V](name: String, key: NamedColumn[K], value: Column[V]) =
      Standard[K, V, K, Nothing](KeyValue[K, V](name, key, value), Named[K, Nothing](key, Column.NoColumn))
  }

  /**
   * Defines the data used for creating table and indexes programmatically.
   */
  object Create {

    case class Throughput(write: Long, read: Long)

    def complexKey[K, V, H, R](tableSchema: Standard[K, V, H, R], throughput: Throughput) = CreateTable(tableSchema, throughput)
    def simple[K, V](tableSchema: SimpleKeyTable[K, V], throughput: Throughput) = CreateTable[K, V, K, Nothing](tableSchema, throughput)

    sealed trait IndexProjection
    object IndexProjection {
      case object KeyOnly extends IndexProjection
      case object All extends IndexProjection
      /**
       * A partial projection requires the names of all columns not used in the keys.
       * @param nonKeyAttributes a list of the column names in the projection that is not used in the key.
       */
      case class Partial(nonKeyAttributes: List[String]) extends IndexProjection
    }

    sealed trait IndexDef {
      type KI
      type VI
      type HI
      type RI

      def index: SecondaryIndex[KI, VI, HI, RI]
      def projection: IndexProjection
    }

    sealed trait GlobalIndexDef extends IndexDef {
      /**
       * The throughput for the global secondary index, must be provided when creating the table with the global secondary index.
       * @return
       */
      def throughput: Throughput
    }

    sealed trait LocalIndexDef extends IndexDef

    /**
     * The request for creating a DynamoDb table.
     * 
     * @param tableSchema the schema of the DynamoDb table
     * @param throughput the throughput of the table
     * @param localIndexes a list of up to 5 local secondary indexes
     * @param globalIndexes a list of up to 5 global secondary indexes
     * @tparam K the key type of the table
     * @tparam V the value type of the table
     * @tparam H the hash type of the hash-range key of the table
     * @tparam R the range type of the hash-range key of the table
     */
    case class CreateTable[K, V, H, R](tableSchema: Standard[K, V, H, R], throughput: Throughput, localIndexes: Vector[LocalIndexDef] = Vector(), globalIndexes: Vector[GlobalIndexDef] = Vector()) {

      type KT = K
      type VT = V
      type HT = H
      type RT = R

      /**
       * Adds a global secondary index with the given index schema, projection and throughput.
       * TODO: to enforce the nonKeyAttributes of Partial projection has all the column names of VV - columns of KK
       * 
       * @param indexSchema the schema of the index
       * @param indexProjection the projection of the index
       * @param indexThroughput the throughput of the index
       * @tparam VV the projected value type of the index
       * @tparam HH the hash type of the hash-range key of the index
       * @tparam RR the range type of the hash-range key of the index
       * @return
       */
      def addGlobalIndex[VV, HH, RR](indexSchema: SecondaryIndex[K, VV, HH, RR], indexProjection: IndexProjection, indexThroughput: Throughput) =
        copy(globalIndexes = globalIndexes :+ new GlobalIndexDef {
          type KI = KT
          type VI = VV
          type HI = HH
          type RI = RR

          val index: SecondaryIndex[KI, VI, HI, RI] = indexSchema
          val projection: IndexProjection = indexProjection
          val throughput: Throughput = indexThroughput
        })

      /**
       * Adds a global secondary index with the given index schema, projection and throughput.
       * TODO: to enforce the nonKeyAttributes of Partial projection has all the column names of VV - columns of KK
       *
       * @param indexSchema the schema of the index
       * @param indexProjection the projection of the index
       * @tparam VV the projected value type of the index
       * @tparam RR the range type of the hash-range key of the index
       * @return
       */
      def addLocalIndex[VV, RR](indexSchema: SecondaryIndex[K, VV, H, RR], indexProjection: IndexProjection) =
        copy(localIndexes = localIndexes :+ new LocalIndexDef {
          type KI = KT
          type VI = VV
          type HI = HT
          type RI = RR

          val index: SecondaryIndex[KI, VI, HI, RI] = indexSchema
          val projection: IndexProjection = indexProjection
        })
    }

  }
}

