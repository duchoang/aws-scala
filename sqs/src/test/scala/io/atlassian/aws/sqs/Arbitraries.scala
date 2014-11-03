package io.atlassian.aws.sqs

import io.atlassian.aws.sqs.Examples.{ Person, Replicate }
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import scalaz.syntax.apply._

object Arbitraries {
  implicit val ReplicateArbitrary: Arbitrary[Replicate] =
    Arbitrary {
      for {
        s <- arbitrary[String]
        d <- arbitrary[String]
        b <- arbitrary[Boolean]
      } yield Replicate(s, d, b)
    }

  implicit def RetriedMessageArbitrary[A: Arbitrary]: Arbitrary[RetriedMessage[A]] =
    Arbitrary {
      for {
        c <- arbitrary[Int]
        a <- arbitrary[A]
      } yield RetriedMessage(c, a)
    }

  implicit val PersonArbitrary: Arbitrary[Person] =
    Arbitrary {
      for {
        s <- arbitrary[String]
        i <- arbitrary[Int]
      } yield Person(s, i)
    }
}
