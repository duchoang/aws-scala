package io.atlassian.aws.dynamodb

import scalaz.Order

trait KeyValueQuery {
  /** key type */
  type K

  /** ordering, or sequence type, may not be avaiable in which case use Nothing */
  type O

  /** value type*/
  type V

  sealed trait Query

  object Query {
    def key(k: K, config: Config = Config())(implicit order: Order[O]): Query =
      Hashed(k, config)

    def range(k: K, ord: O, cmp: Comparison, config: Config = Config())(implicit order: Order[O]): Query =
      Ranged(k, ord, cmp, config)

    case class Hashed(key: K, config: Config) extends Query
    case class Ranged(key: K, ord: O, cmp: Comparison, config: Config) extends Query

    case class Config(limit: Option[Int] = None)
  }
}