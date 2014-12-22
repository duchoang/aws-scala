package io.atlassian.aws.spec

import org.scalacheck.{ Gen, Arbitrary }

object NumericTypes {
  case class Pos[A](i: A)
  implicit def posArbitrary[A: Numeric: Gen.Choose]: Arbitrary[Pos[A]] =
    Arbitrary { Gen.posNum[A].map { Pos.apply } }
}
