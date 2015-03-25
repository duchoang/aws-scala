package io.atlassian.aws.dynamodb

import scalaz.Order

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

  sealed trait Query

  object Query {
    def hash(hash: H, config: Config = Config()): Query =
      Hashed(hash, config)

    def range(hash: H, range: R, cmp: Comparison, config: Config = Config())(implicit order: Order[R]): Query =
      Ranged(hash, range, cmp, config)

    case class Hashed(hash: H, config: Config) extends Query
    case class Ranged(hash: H, range: R, cmp: Comparison, config: Config) extends Query

    case class Config(limit: Option[Int] = None)
  }
}