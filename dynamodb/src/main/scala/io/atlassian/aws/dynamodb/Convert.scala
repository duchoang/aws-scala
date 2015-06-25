package io.atlassian.aws.dynamodb

import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConverters._
import scalaz.{ Functor, Show, State }
import scalaz.Isomorphism.<=>
import scalaz.syntax.id._
import scalaz.syntax.show._
import scalaz.std.list._

//private[dynamodb] 
sealed trait Convert[A, B] {
  def convert: A => B
}

//private[dynamodb]
object Convert {
  import Schema._
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
    def convert = { case Throughput(r, w) => new ProvisionedThroughput(r, w) }
  }

  implicit def NamedToAttributeDefinition[H, R] =
    new Convert[Named[H, R], List[AttributeDefinition]] {
      def convert = {
        case Named(hash, range) => List(hash.decoder.dynamoType(hash.name), range.decoder.dynamoType(range.name))
      }
    }

  implicit def NamedToSchema[H, R] =
    new Convert[Named[H, R], List[KeySchemaElement]] {
      def convert = {
        case Named(hash, range) => List(new KeySchemaElement(hash.name, KeyType.HASH), new KeySchemaElement(range.name, KeyType.RANGE))
      }
    }

  implicit def IndexToProjection[V] =
    new Convert[IndexProjection[V], Projection] {
      def convert = p =>
        new Projection().withProjectionType(p.projectionType)
          .withNonKeyAttributes(if (p.nonKeyAttributes.isEmpty) null else p.nonKeyAttributes.asJavaCollection)
    }

  implicit def TableCreateToCreateTableRequest[K, V, H, R] =
    new Convert[Create[K, V, H, R], CreateTableRequest] {
      def convert = {
        case Create(Standard(table, hashRange), indexes) =>
          new CreateTableRequest().withTableName(table.name) <| { req =>
            req.withAttributeDefinitions(hashRange.convertTo[List[AttributeDefinition]].asJavaCollection)
            req.withKeySchema(hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
          }
        //.withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput]) 
        //TODO add global secondary index details
        //<| { req =>
        //table.globalSecondaryIndexes.toOnel.foreach { is => req.withGlobalSecondaryIndexes { is.map(toAwsGSI).list.asJavaCollection } }
        //}

      }
    }

  //  implicit def HashRangeKeyToCreateTable[H, R, V] =
  //    new Convert[HashRangeKey[H, R, V], CreateTableRequest] {
  //      def convert = {
  //        case HashRangeKey(table, hashRange, throughput) =>
  //          new CreateTableRequest().withTableName(table.name)
  //            .withAttributeDefinitions(hashRange.convertTo[List[AttributeDefinition]].asJavaCollection)
  //            .withKeySchema(hashRange.convertTo[List[KeySchemaElement]].asJavaCollection)
  //            .withProvisionedThroughput(throughput.convertTo[ProvisionedThroughput])
  //        //TODO add global secondary index details
  //        //<| { req =>
  //        //table.globalSecondaryIndexes.toOnel.foreach { is => req.withGlobalSecondaryIndexes { is.map(toAwsGSI).list.asJavaCollection } }
  //        //}
  //        //          req =>
  //        //            table.localSecondaryIndexes.toOnel.foreach { is => req.withLocalSecondaryIndexes { is.map(toAwsLSI).list.asJavaCollection } }
  //        //            table.globalSecondaryIndexes.toOnel.foreach { is => req.withGlobalSecondaryIndexes { is.map(toAwsGSI).list.asJavaCollection } }
  //        //          }
  //
  //      }
  //    }

  //  def ConvertGlobalSecondary[V] =
  //    new Convert[GlobalSecondary[V], GlobalSecondaryIndex] {
  //      def convert(idx: GlobalSecondary[V]) =
  //        new GlobalSecondaryIndex()
  //          .withIndexName(idx.name)
  //          .withKeySchema(idx.schemaElements.convertTo[List[KeySchemaElement]].asJavaCollection)
  //          .withProjection(idx.projection.convertTo[Projection])
  //          .withProvisionedThroughput(Convert[Throughput, ProvisionedThroughput](idx.throughput))
  //    }
  //  def ConvertLocalSecondary[V] =
  //    new Convert[LocalSecondary[V], LocalSecondaryIndex] {
  //      def convert = {
  //        case LocalSecondary(name, schema, prj) =>
  //          new LocalSecondaryIndex()
  //            .withIndexName(name)
  //            .withKeySchema(schema.convertTo[List[KeySchemaElement]].asJavaCollection)
  //            .withProjection(prj.convertTo[Projection])
  //      }
  //    }

}
