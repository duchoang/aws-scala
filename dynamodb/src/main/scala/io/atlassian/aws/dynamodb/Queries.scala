package io.atlassian.aws.dynamodb

import DynamoDB.ReadConsistency

/**
 * Queries is defined in terms of Hash and Range types.
 *
 * The Hash type identifies records and the Range allows us to do ordering.
 */
trait Queries {
  /** the Hash type */
  type H

  /** the Range type */
  type R

  sealed trait Query {
    def config(c: Query.Config): Query
  }

  object Query {
    def hash(hash: H, indexName: Option[String] = None, config: Config = Config()): Query =
      Hashed(hash, indexName, config)

    def range(hash: H, range: R, cmp: Comparison, indexName: Option[String] = None, config: Config = Config()): Query =
      Ranged(hash, range, cmp, indexName, config)

    case class Hashed(hash: H, indexName: Option[String], config: Config) extends Query {
      def config(c: Query.Config) = copy(config = c)
    }
    case class Ranged(hash: H, range: R, cmp: Comparison, indexName: Option[String], config: Config) extends Query {
      def config(c: Query.Config) = copy(config = c)
    }

    case class Config(direction: ScanDirection = ScanDirection.Ascending, limit: Option[Int] = None, consistency: ReadConsistency = ReadConsistency.Eventual)
  }
}