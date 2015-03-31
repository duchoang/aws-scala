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

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class ColumnSpec extends ScalaCheckSpec {

  def is = s2"""
    Columns should
      round trip Int    ${round[Int]}
      round trip Long    ${round[Long]}
      round trip String    ${round[String]}
      round trip DateTime    ${round[DateTime]}
    """

  def round[A: Arbitrary: Encoder: Decoder: Equal] = Prop.forAll {
    (name: String, a: A) =>
      def check[X: Equal](c: Column[X])(x: X) =
        c.unmarshaller.run(c.marshaller.toFlattenedMap(x)).toOption.exists(Equal[X].equal(_, x))

      check(Column[A](name))(a)
      check(Column[A](name).liftOption)(Some(a))
      check(Column[A](name).liftOption)(Option.empty[A])
  }
}