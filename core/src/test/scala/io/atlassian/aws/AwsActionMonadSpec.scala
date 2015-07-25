package io.atlassian.aws

import kadai.Invalid
import org.junit.runner.RunWith
import org.specs2.scalaz.Spec
import org.specs2.ScalaCheck
import org.scalacheck.{ Gen, Arbitrary }
import scalaz.concurrent.Future
import scalaz.{ EitherT, ReaderT, Equal, WriterT }
import scalaz.scalacheck.ScalazProperties

@RunWith(classOf[org.specs2.runner.JUnitRunner])
class AwsActionMonadSpec extends Spec with ScalaCheck {

  import ScalazProperties._
  import Invalid.InvalidMonoid
  import scalaz.std.anyVal.intInstance
  import org.scalacheck.Arbitrary._
  import Invalid.syntax._

  type R = Unit
  type W = Int

  type Action[A] = AwsAction[R, W, A]
  implicit val ActionMonad = new AwsActionMonad[R, W]()(intInstance)

  type WriterW[A] = WriterT[Future, W, A]
  type ActionWithLeftSide[L, A] = ReaderT[EitherT[WriterW, L, ?], R, A]

  implicit def AwsActionArbitrary[A](implicit A: Arbitrary[A]): Arbitrary[Action[A]] = Arbitrary {
    A.arbitrary map { AwsAction.ok[R, W, A] }
  }

  implicit def ActionEqual[A](implicit E: Equal[A]): Equal[Action[A]] =
    new Equal[Action[A]] {
      implicit class ActionOps(action: Action[A]) extends AwsActionOps[R, W, A](action)
      override def equal(a1: Action[A], a2: Action[A]): Boolean =
        a1.runActionWithMetaData(()) == a2.runActionWithMetaData(())
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
  checkAll("AwsActionMonad MonadError laws", monadError.laws[ActionWithLeftSide, Invalid])
}
