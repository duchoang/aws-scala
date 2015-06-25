package io.atlassian.aws
package dynamodb

import scalaz.Show

import com.amazonaws.services.dynamodbv2.model.ProjectionType

object Schema {
  case class KeyValue[K, V](name: String, key: Column[K], value: Column[V])
  case class Named[A, B](a: NamedColumn[A], b: NamedColumn[B])

  case class Standard[K, V, H, R](
    kv: KeyValue[K, V],
    hashRange: Named[H, R]) {
    def name = kv.name
  }

  object Standard {
    def apply[K, V](kv: KeyValue[K, V]) =
      Standard[K, V, Nothing, Nothing](kv, Named[Nothing, Nothing](Column.NoColumn, Column.NoColumn))
  }

  case class Throughput(write: Long, read: Long)

  case class Index[K, V, H, R](name: Option[String], table: Standard[K, V, H, R])

  trait IndexDef {
    type K
    type V
    type H
    type R

    def index: Index[K, V, H, R]
  }

  case class Create[K, V, H, R](std: Standard[K, V, H, R], indexes: Vector[IndexDef] = Vector()) {
    def addIndex[KK, VV, HH, RR](i: Index[KK, VV, HH, RR]) =
      copy(indexes = indexes :+ new IndexDef {
        type K = KK
        type V = VV
        type H = HH
        type R = RR

        def index: Index[K, V, H, R] = i
      })
  }

  case class IndexProjection[V](val projectionType: ProjectionType, value: Column[V], nonKeyAttributes: List[String] = Nil)

  object IndexProjection {
    def keyOnlyProjection[K, V]: KeyValue[K, V] => IndexProjection[K] =
      kv => IndexProjection(ProjectionType.KEYS_ONLY, kv.key)

    def allProjection[K, V]: KeyValue[K, V] => IndexProjection[V] =
      kv => IndexProjection(ProjectionType.ALL, kv.value)

    def includeProjection[K, V, VV](value: Column[VV], nonKeyAttributeNames: List[String]): KeyValue[K, V] => IndexProjection[VV] =
      _ => IndexProjection(ProjectionType.INCLUDE, value, nonKeyAttributeNames)
  }
}