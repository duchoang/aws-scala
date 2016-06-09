package io.atlassian.aws.spec

import org.scalacheck.Arbitrary

import scalaz._

/**
 * Instances of {@link scalacheck.Arbitrary} for types of Scalaz that we need.
 * Taken from scalaz-scalacheck-binding which currently is behind on scalacheck version.
 */
object ScalazArbitrary {
  import Arbitrary._
  import ScalaCheckBinding._

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def DisjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Functor[Arbitrary].map(arb[Either[A, B]]) {
      case Left(a)  => -\/(a)
      case Right(b) => \/-(b)
    }
}