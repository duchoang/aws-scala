package io.atlassian.aws.sqs

import io.atlassian.aws.sqs.Examples.Replicate
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import scalaz.syntax.apply._
import scalaz.scalacheck.ScalaCheckBinding._

object Arbitraries {
  implicit val ReplicateArbitrary: Arbitrary[Replicate] =
    Arbitrary {
      (arbitrary[String] |@| arbitrary[String] |@| arbitrary[Boolean])(Replicate.apply)
    }

  implicit def RetriedMessageArbitrary[A: Arbitrary]: Arbitrary[RetriedMessage[A]] =
    Arbitrary {
      (arbitrary[Int] |@| arbitrary[A])(RetriedMessage.apply)
    }

}
