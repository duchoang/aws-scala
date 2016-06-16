package io.atlassian.aws.spec

import kadai.Invalid
import kadai.Invalid._
import org.joda.time.{ DateTime, Instant }
import org.scalacheck.{ Arbitrary, Cogen }

object Arbitraries {
  implicit val DateTimeArbitrary: Arbitrary[DateTime] =
    Arbitrary { Arbitrary.arbitrary[Int].map { i => new DateTime(i.toLong) } }

  implicit val InstantArbitrary: Arbitrary[Instant] =
    Arbitrary { Arbitrary.arbitrary[Int].map { i => new Instant(i.toLong) } }

  implicit val cogenInvalid: Cogen[Invalid] =
    Cogen(i => i match {
      case Message(_)   => 0L
      case Err(_)       => 1L
      case Composite(_) => 2L
      case Zero         => 3L
    })
}
