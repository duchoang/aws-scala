package io.atlassian.aws

import io.atlassian.aws.spec.{ Arbitraries, MutableScalaCheckSpec, ScalazProperties }
import kadai.Invalid
import org.junit.runner.RunWith
import org.scalacheck.{ Arbitrary, Gen }

import scalaz.concurrent.Future
import scalaz.{ EitherT, Equal, ReaderT, WriterT }

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class AwsActionMonadSpec extends MutableScalaCheckSpec {

  import Arbitraries._
  import ScalazProperties._
  import scalaz.std.anyVal.intInstance
  import org.scalacheck.Arbitrary._
  import Invalid.syntax._

  type R = Unit
  type W = Int

  type Action[A] = AwsAction[R, W, A]


  implicit def AwsActionArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Action[A]] = Arbitrary {
    A.arbitrary map { AwsAction.ok[R, W, A] }
  }

  implicit def ActionEqual[A](implicit E: Equal[A]): Equal[Action[A]] =
    new Equal[Action[A]] {
      override def equal(a1: Action[A], a2: Action[A]): Boolean =
        a1.unsafePerformWithMetaData(()) == a2.unsafePerformWithMetaData(())
    }

  implicit def ArbitraryInvalid: Arbitrary[Invalid] =
    Arbitrary {
      Gen.oneOf(
        Arbitrary.arbitrary[Throwable].map { _.invalid },
        Arbitrary.arbitrary[String].map { _.invalid }
      )
    }

  checkAll("AwsActionMonad Monad laws", monad.laws[Action])
  checkAll("AwsActionMonad MonadPlus laws", monadPlus.laws[Action])
  checkAll("AwsActionMonad MonadError laws", monadError.laws[Action, Invalid])
}
