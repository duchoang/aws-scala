package io.atlassian.aws.spec

import org.joda.time.{DateTime, DateTimeZone}
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.matcher.{Expectable, MatchResult, Matcher}
import org.specs2.{ScalaCheck, SpecificationWithJUnit}

import scalaz.Equal
import scalaz.std.AllInstances._

trait ScalaCheckSpec extends SpecificationWithJUnit with ScalaCheck with Arbitraries with ScalazEqualMatcher with MoreEqualsInstances

trait Arbitraries {
  implicit def DateTimeArbitrary: Arbitrary[DateTime] =
    Arbitrary {
      for {
        diff <- Gen.chooseNum(-1000000, 1000000)
      } yield DateTime.now().plus(diff.toLong)
    }
}

trait ScalazEqualMatcher {
  def equal[T : Equal](expected: T): Matcher[T] = new Matcher[T] {
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
  implicit def JodaDateTimeEqual: Equal[DateTime] = Equal.equalBy { _.withZone(DateTimeZone.UTC).toInstant.getMillis }
}