package io.atlassian.aws.spec

import org.joda.time.{ DateTime, DateTimeZone, Instant }
import org.scalacheck.{ Arbitrary, Gen }
import org.specs2.matcher.{ Expectable, MatchResult, Matcher }
import org.specs2.{ ScalaCheck, SpecificationWithJUnit }

import scalaz.Equal
import scalaz.std.AllInstances._

trait ScalaCheckSpec extends SpecificationWithJUnit with ScalaCheck with Arbitraries with ScalazEqualMatcher with MoreEqualsInstances

trait Arbitraries {
  implicit def DateTimeArbitrary: Arbitrary[DateTime] =
    Arbitrary { Arbitrary.arbitrary[Int].map { i => new DateTime(i.toLong) } }

  implicit def InstantArbitrary: Arbitrary[Instant] =
    Arbitrary { Arbitrary.arbitrary[Int].map { i => new Instant(i.toLong) } }
}

trait ScalazEqualMatcher {
  def equal[T: Equal](expected: T): Matcher[T] = new Matcher[T] {
    def apply[S <: T](actual: Expectable[S]): MatchResult[S] = {
      val actualT = actual.value.asInstanceOf[T]
      def test = Equal[T].equal(expected, actualT)
      def koMessage = s"$actualT !== $expected"
      def okMessage = s"$actualT === $expected"
      Matcher.result(test, okMessage, koMessage, actual)
    }
  }
}

trait MoreEqualsInstances {
	implicit def JodaInstantEqual: Equal[Instant] = Equal.equalA
  implicit def JodaDateTimeEqual: Equal[DateTime] = Equal[Instant].contramap { _.toInstant }
}