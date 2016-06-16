package io.atlassian.aws.dynamodb

import io.atlassian.aws.spec.{ Arbitraries, ScalaCheckSpec }
import org.joda.time.{ DateTime, Instant }
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Prop }
import Unmarshaller._

import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.std.tuple._

@RunWith(classOf[org.specs2.runner.JUnitRunner])
object ColumnSpec extends ScalaCheckSpec {
  import Arbitraries._
  def is = s2"""
    Columns should 
      round trip:
        Int         ${round[Int]}
        Long        ${round[Long]}
        String      ${round[String]}
        Instant     ${round[Instant]}
        DateTime    ${round[DateTime]}
      
      compose:
        2 columns    $compose2
        3 columns    $compose3
        4 columns    $compose4
        5 columns    $compose5
        6 columns    $compose6

      compose case classes:
        2 columns    $case2
        3 columns    $case3
        4 columns    $case4
        5 columns    $case5
        6 columns    $case6
    """

  def check[X: Equal](c: Column[X])(x: X) =
    c.unmarshall(c.marshall.toFlattenedMap(x)).toOption.exists(Equal[X].equal(_, x))

  def round[A: Arbitrary: Encoder: Decoder: Equal] =
    Prop.forAll { (name: String, a: A) =>
      check(Column[A](name).column)(a) &&
        check(Column[A](name).column.liftOption)(Some(a)) &&
        check(Column[A](name).column.liftOption)(Option.empty[A])
    }

  def compose2 =
    Prop.forAll { (s: String, d: Instant) =>
      check {
        Column.compose2[(String, Instant)](c1, c2) { case (ss, dd) => (ss, dd) } { case (ss, dd) => (ss, dd) }
      }((s, d))
    }

  def case2 =
    Prop.forAll { (s: String, d: Instant) =>
      case class Check(s: String, d: Instant)
      implicit val CheckEq = Equal.equalA[Check]

      check {
        Column.case2[Check](c1, c2)(Check.apply, Check.unapply)
      }(Check(s, d))
    }

  def compose3 =
    Prop.forAll { (s: String, d: Instant, l: Long) =>
      check {
        Column.compose3[(String, Instant, Long)](c1, c2, c3) { case (ss, dd, ll) => (ss, dd, ll) } { case (ss, dd, ll) => (ss, dd, ll) }
      }((s, d, l))
    }

  def case3 =
    Prop.forAll { (s: String, d: Instant, l: Long) =>
      case class Check(s: String, d: Instant, l: Long)
      implicit val CheckEq = Equal.equalA[Check]
      check {
        Column.case3[Check](c1, c2, c3)(Check.apply, Check.unapply)
      }(Check(s, d, l))
    }

  def compose4 =
    Prop.forAll { (s: String, d: Instant, l: Long, i: Int) =>
      check {
        Column.compose4[(String, Instant, Long, Int)](c1, c2, c3, c4) { case (ss, dd, ll, ii) => (ss, dd, ll, ii) } { case (ss, dd, ll, ii) => (ss, dd, ll, ii) }
      }((s, d, l, i))
    }

  def case4 =
    Prop.forAll { (s: String, d: Instant, l: Long, i: Int) =>
      case class Check(s: String, d: Instant, l: Long, i: Int)
      implicit val CheckEq = Equal.equalA[Check]
      check {
        Column.case4[Check](c1, c2, c3, c4)(Check.apply, Check.unapply)
      }(Check(s, d, l, i))
    }

  def compose5 =
    Prop.forAll { (s: String, d: Instant, l1: Long, i: Int, l2: Long) =>
      check {
        Column.compose5[(String, Instant, Long, Int, Long)](c1, c2, c3, c4, c5) { case (ss, dd, l1, ii, l2) => (ss, dd, l1, ii, l2) } { case (ss, dd, l1, ii, l2) => (ss, dd, l1, ii, l2) }
      }((s, d, l1, i, l2))
    }

  def case5 =
    Prop.forAll { (s: String, d: Instant, l1: Long, i: Int, l2: Long) =>
      case class Check(s: String, d: Instant, l1: Long, i: Int, l2: Long)
      implicit val CheckEq = Equal.equalA[Check]
      check {
        Column.case5[Check](c1, c2, c3, c4, c5)(Check.apply, Check.unapply)
      }(Check(s, d, l1, i, l2))
    }

  def compose6 =
    Prop.forAll { (s1: String, d: Instant, l1: Long, i: Int, l2: Long, s2: String) =>
      check {
        Column.compose6[(String, Instant, Long, Int, Long, String)](c1, c2, c3, c4, c5, c6) { case (s1, dd, l1, ii, l2, s2) => (s1, dd, l1, ii, l2, s2) } { case (s1, dd, l1, ii, l2, s2) => (s1, dd, l1, ii, l2, s2) }
      }((s1, d, l1, i, l2, s2))
    }

  def case6 =
    Prop.forAll { (s1: String, d: Instant, l1: Long, i: Int, l2: Long, s2: String) =>
      case class Check(s1: String, d: Instant, l1: Long, i: Int, l2: Long, s2: String)
      implicit val CheckEq = Equal.equalA[Check]
      check {
        Column.case6[Check](c1, c2, c3, c4, c5, c6)(Check.apply, Check.unapply)
      }(Check(s1, d, l1, i, l2, s2))
    }

  val c1 = Column[String]("one").column
  val c2 = Column[Instant]("two").column
  val c3 = Column[Long]("three").column
  val c4 = Column[Int]("four").column
  val c5 = Column[Long]("five").column
  val c6 = Column[String]("six").column
}
