package io.atlassian.aws.dynamodb

/**
 * Witness that the type is
 */
sealed trait Keyed[A]
object Keyed {
  object StringKeyed extends Keyed[String]
  object IntKeyed extends Keyed[Int]
  object LongKeyed extends Keyed[Long]
}

sealed trait Key[A]
object Key {
  case class Hash[A: Keyed](col: NamedColumn[A]) extends Key[A]
  case class Composite[A, B: Keyed, C: Keyed](hash: NamedColumn[B], range: NamedColumn[C]) extends Key[A]
  case class Mapped[A, B](f: A => B, key: Key[B]) extends Key[A]
}
