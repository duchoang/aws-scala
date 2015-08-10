package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._
import scalaz.{ NonEmptyList, Functor }
import scalaz.std.option._
import scalaz.syntax.foldable._
import scalaz.syntax.id._

//private[dynamodb] 
sealed trait Convert[A, B] {
  def convert: A => B
}

//private[dynamodb]
object Convert {
  import Schema._
  import Create._
  def apply[A, B](a: A)(implicit ev: Convert[A, B]): B =
    implicitly[Convert[A, B]].convert(a)

  implicit def ConvertFunctor[A, B, F[_]: Functor](implicit ev: Convert[A, B]): Convert[F[A], F[B]] =
    new Convert[F[A], F[B]] {
      def convert = Functor[F].map(_)(ev.convert)
    }

  implicit class ConversionSyntax[A](val a: A) extends AnyVal {
    def convertTo[B](implicit ev: Convert[A, B]): B =
      ev convert a
  }

  //
  // instances
  //

  implicit object ThroughputToProvisioned extends Convert[Throughput, ProvisionedThroughput] {
    def convert = { t => new ProvisionedThroughput(t.read, t.write) }
  }

  implicit def NamedToAttributeDefinition[H, R] =
    new Convert[Named[H, R], List[AttributeDefinition]] {
      def convert = {
        case Named(hash, Column.NoColumn) => List(hash.dynamoType)
        case Named(hash, range) => List(hash.dynamoType, range.dynamoType)
      }
    }

  implicit def NamedToSchema[H, R] =
    new Convert[Named[H, R], List[KeySchemaElement]] {
      def convert = {
        case Named(hash, Column.NoColumn) => List(new KeySchemaElement(hash.name, KeyType.HASH))
        case Named(hash, range) => List(new KeySchemaElement(hash.name, KeyType.HASH), new KeySchemaElement(range.name, KeyType.RANGE))
      }
    }

  implicit def IndexToProjection: Convert[IndexProjection, Projection] =
    new Convert[IndexProjection, Projection] {
      def convert = p => (p match {
        case IndexProjection.KeyOnly => (ProjectionType.KEYS_ONLY, List())
        case IndexProjection.All => (ProjectionType.ALL, List())
        case IndexProjection.Partial(nonKeyAttributes) => (ProjectionType.INCLUDE, nonKeyAttributes)
      }) |> {
        case (projectionType, nonKeyAttributes) =>
          new Projection().withProjectionType(projectionType)
            .withNonKeyAttributes(if (nonKeyAttributes.isEmpty) null else nonKeyAttributes.asJavaCollection)
      }
    }

  implicit class ToOnelOps[A](val l: Seq[A]) extends AnyVal {
    def toOnel: Option[NonEmptyList[A]] = l.toList match {
      case Nil    => None
      case h :: t => Some(NonEmptyList.nel(h, t))
    }
  }

  implicit def LocalIndexDefToLocalSecondaryIndex: Convert[LocalIndexDef, LocalSecondaryIndex] =
    new Convert[LocalIndexDef, LocalSecondaryIndex] {
      def convert = indexDef =>
        new LocalSecondaryIndex()
          .withIndexName(indexDef.index.name)
          .withKeySchema(indexDef.index.hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
          .withProjection(indexDef.projection.convertTo[Projection])
    }

  implicit def GlobalIndexDefToGlobalSecondaryIndex: Convert[GlobalIndexDef, GlobalSecondaryIndex] =
      new Convert[GlobalIndexDef, GlobalSecondaryIndex] {
        def convert = indexDef =>
          new GlobalSecondaryIndex()
            .withIndexName(indexDef.index.name)
            .withKeySchema(indexDef.index.hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
            .withProjection(indexDef.projection.convertTo[Projection])
            .withProvisionedThroughput(Convert[Throughput, ProvisionedThroughput](indexDef.throughput))
      }

  implicit def TableCreateToCreateTableRequest[K, V, H, R]: Convert[CreateTable[K, V, H, R], CreateTableRequest] =
    new Convert[CreateTable[K, V, H, R], CreateTableRequest] {
      def convert = {
        case CreateTable(Standard(table, hashRange), throughput, localIndexes, globalIndexes) =>
          new CreateTableRequest().withTableName(table.name) <| { req =>

            val attributeDefinitions = (hashRange :: localIndexes.toList.map(_.index.hashRange) ::: globalIndexes.toList.map(_.index.hashRange))
              .flatMap(_.convertTo[List[AttributeDefinition]])

            req.withAttributeDefinitions(attributeDefinitions.distinct.asJavaCollection)
            .withKeySchema(hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
            .withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput])
          } |> { req =>
            globalIndexes.toOnel.foldl(req) { req => globalIndexNel =>
              req.withGlobalSecondaryIndexes(globalIndexNel.list.map(_.convertTo[GlobalSecondaryIndex]).asJavaCollection)
            }
          } |> { req =>
            localIndexes.toOnel.foldl(req){ req => localIndexNel =>
              req.withLocalSecondaryIndexes(localIndexNel.list.map(_.convertTo[LocalSecondaryIndex]).asJavaCollection)
            }
          }
      }
    }
}
