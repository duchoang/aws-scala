package io.atlassian.aws.dynamodb

import io.atlassian.aws.spec.ScalaCheckSpec
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Prop }
import Unmarshaller._

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ColumnSpec extends ScalaCheckSpec {

  def is = s2"""
    Columns should 
      round trip:
        Int         ${round[Int]}
        Long        ${round[Long]}
        String      ${round[String]}
        DateTime    ${round[DateTime]}
      
      compose:
        2 columns    $compose2
        3 columns    $compose3
        4 columns    $compose4
        5 columns    $compose5
        6 columns    $compose6
    """

  def check[X: Equal](c: Column[X])(x: X) =
    c.unmarshall(c.marshall.toFlattenedMap(x)).toOption.exists(Equal[X].equal(_, x))

  def round[A: Arbitrary: Encoder: Decoder: Equal] =
    Prop.forAll { (name: String, a: A) =>
      check(Column[A](name))(a) &&
        check(Column[A](name).liftOption)(Some(a)) &&
        check(Column[A](name).liftOption)(Option.empty[A])
    }

  def compose2 =
    Prop.forAll { (s: String, d: DateTime) =>
      check {
        Column.compose2[(String, DateTime)](c1, c2) { case (ss, dd) => (ss, dd) } { case (ss, dd) => (ss, dd) }
      }((s, d))
    }

  def compose3 =
    Prop.forAll { (s: String, d: DateTime, l: Long) =>
      check {
        Column.compose3[(String, DateTime, Long)](c1, c2, c3) { case (ss, dd, ll) => (ss, dd, ll) } { case (ss, dd, ll) => (ss, dd, ll) }
      }((s, d, l))
    }

  def compose4 =
    Prop.forAll { (s: String, d: DateTime, l: Long, i: Int) =>
      check {
        Column.compose4[(String, DateTime, Long, Int)](c1, c2, c3, c4) { case (ss, dd, ll, ii) => (ss, dd, ll, ii) } { case (ss, dd, ll, ii) => (ss, dd, ll, ii) }
      }((s, d, l, i))
    }

  def compose5 =
    Prop.forAll { (s: String, d: DateTime, l1: Long, i: Int, l2: Long) =>
      check {
        Column.compose5[(String, DateTime, Long, Int, Long)](c1, c2, c3, c4, c5) { case (ss, dd, l1, ii, l2) => (ss, dd, l1, ii, l2) } { case (ss, dd, l1, ii, l2) => (ss, dd, l1, ii, l2) }
      }((s, d, l1, i, l2))
    }

  def compose6 =
    Prop.forAll { (s1: String, d: DateTime, l1: Long, i: Int, l2: Long, s2: String) =>
      check {
        Column.compose6[(String, DateTime, Long, Int, Long, String)](c1, c2, c3, c4, c5, c6) { case (s1, dd, l1, ii, l2, s2) => (s1, dd, l1, ii, l2, s2) } { case (s1, dd, l1, ii, l2, s2) => (s1, dd, l1, ii, l2, s2) }
      }((s1, d, l1, i, l2, s2))
    }

  val c1 = Column[String]("one")
  val c2 = Column[DateTime]("two")
  val c3 = Column[Long]("three")
  val c4 = Column[Int]("four")
  val c5 = Column[Long]("five")
  val c6 = Column[String]("six")
}