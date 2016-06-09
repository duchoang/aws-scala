package io.atlassian.aws.spec

import scalaz.Equal
import scalaz.std.AllInstances._

import org.specs2._
import org.specs2.matcher._
import org.specs2.specification._
import org.specs2.specification.core._
import org.specs2.scalacheck._
import org.specs2.main.{ ArgumentsShortcuts, ArgumentsArgs }

import org.scalacheck.Properties
import org.scalacheck.util.{ FreqMap, Pretty }

import org.joda.time.{ DateTime, Instant }
import org.specs2.matcher.{ Expectable, MatchResult, Matcher }

trait MutableScalaCheckSpec extends org.specs2.mutable.Spec with ScalaCheck with ScalazEqualMatcher with MoreEqualsInstances
    with MatchersImplicits with StandardMatchResults
    with ArgumentsShortcuts with ArgumentsArgs {
  val ff = fragmentFactory; import ff._
  setArguments(fullStackTrace)

  def checkAll(name: String, props: Properties)(implicit p: Parameters, f: FreqMap[Set[Any]] => Pretty) {
    addFragment(text(s"$name  ${props.name} must satisfy"))
    addFragments(Fragments.foreach(props.properties) { case (name, prop) => Fragments(name in check(prop, p, f)) })
    ()
  }
}

trait ScalaCheckSpec extends Specification with ScalaCheck with ScalazEqualMatcher with MoreEqualsInstances

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