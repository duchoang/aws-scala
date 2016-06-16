package io.atlassian.aws.spec

import org.scalacheck.Prop.forAll
import org.scalacheck.{ Arbitrary, Cogen, Properties }

import scalaz._

/**
 * Scalacheck properties that should hold for instances of type classes defined in Scalaz Core.
 * Taken from scalaz-scalacheck-binding which currently is behind on scalacheck version.
 */
object ScalazProperties {
  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }

  object equal {
    def commutativity[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.commutative _)

    def reflexive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.reflexive _)

    def transitive[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.transitive _)

    def naturality[A](implicit A: Equal[A], arb: Arbitrary[A]) = forAll(A.equalLaw.naturality _)

    def laws[A](implicit A: Equal[A], arb: Arbitrary[A]): Properties =
      newProperties("equal") { p =>
        p.property("commutativity") = commutativity[A]
        p.property("reflexive") = reflexive[A]
        p.property("transitive") = transitive[A]
        p.property("naturality") = naturality[A]
        ()
      }
  }

  object semigroup {
    def associative[A](implicit A: Semigroup[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.semigroupLaw.associative _)

    def laws[A](implicit A: Semigroup[A], eqa: Equal[A], arb: Arbitrary[A]): Properties =
      newProperties("semigroup") { p =>
        p.property("associative") = associative[A]
        ()
      }
  }

  object monoid {
    def leftIdentity[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.leftIdentity _)

    def rightIdentity[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]) = forAll(A.monoidLaw.rightIdentity _)

    def laws[A](implicit A: Monoid[A], eqa: Equal[A], arb: Arbitrary[A]): Properties =
      newProperties("monoid") { p =>
        p.include(semigroup.laws[A])
        p.property("left identity") = leftIdentity[A]
        p.property("right identity") = rightIdentity[A]
        ()
      }
  }

  object plus {
    def associative[F[_], X](implicit f: Plus[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusLaw.associative[X] _)

    def laws[F[_]](implicit F: Plus[F], afx: Arbitrary[F[Int]], ef: Equal[F[Int]]): Properties =
      newProperties("plus") { p =>
        p.include(semigroup.laws[F[Int]](F.semigroup[Int], implicitly, implicitly))
        p.property("associative") = associative[F, Int]
        ()
      }
  }

  object plusEmpty {
    def leftPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusEmptyLaw.leftPlusIdentity[X] _)

    def rightPlusIdentity[F[_], X](implicit f: PlusEmpty[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.plusEmptyLaw.rightPlusIdentity[X] _)

    def laws[F[_]](implicit F: PlusEmpty[F], afx: Arbitrary[F[Int]], af: Arbitrary[Int => Int], ef: Equal[F[Int]]): Properties =
      newProperties("plusEmpty") { p =>
        p.include(plus.laws[F])
        p.include(monoid.laws[F[Int]](F.monoid[Int], implicitly, implicitly))
        p.property("left plus identity") = leftPlusIdentity[F, Int]
        p.property("right plus identity") = rightPlusIdentity[F, Int]
        ()
      }
  }

  object invariantFunctor {
    def identity[F[_], X](implicit F: InvariantFunctor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.invariantFunctorLaw.invariantIdentity[X] _)

    def composite[F[_], X, Y, Z](implicit F: InvariantFunctor[F], af: Arbitrary[F[X]], axy: Arbitrary[(X => Y)],
                                 ayz: Arbitrary[(Y => Z)], ayx: Arbitrary[(Y => X)], azy: Arbitrary[(Z => Y)], ef: Equal[F[Z]]) =
      forAll(F.invariantFunctorLaw.invariantComposite[X, Y, Z] _)

    def laws[F[_]](implicit F: InvariantFunctor[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]): Properties =
      newProperties("invariantFunctor") { p =>
        p.property("identity") = identity[F, Int]
        p.property("composite") = composite[F, Int, Int, Int]
        ()
      }
  }

  object functor {
    def identity[F[_], X](implicit F: Functor[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.functorLaw.identity[X] _)

    def composite[F[_], X, Y, Z](implicit F: Functor[F], af: Arbitrary[F[X]], axy: Arbitrary[(X => Y)],
                                 ayz: Arbitrary[(Y => Z)], ef: Equal[F[Z]]) =
      forAll(F.functorLaw.composite[X, Y, Z] _)

    def laws[F[_]](implicit F: Functor[F], af: Arbitrary[F[Int]], axy: Arbitrary[(Int => Int)],
                   ef: Equal[F[Int]]): Properties =
      newProperties("functor") { p =>
        p.include(invariantFunctor.laws[F])
        p.property("identity") = identity[F, Int]
        p.property("composite") = composite[F, Int, Int, Int]
        ()
      }
  }

  object apply { self =>
    def composition[F[_], X, Y, Z](implicit ap: Apply[F], afx: Arbitrary[F[X]], au: Arbitrary[F[Y => Z]],
                                   av: Arbitrary[F[X => Y]], e: Equal[F[Z]]) = forAll(ap.applyLaw.composition[X, Y, Z] _)

    def laws[F[_]](implicit F: Apply[F], af: Arbitrary[F[Int]],
                   aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]]): Properties =
      newProperties("apply") { p =>
        p.include(functor.laws[F])
        p.property("composition") = self.composition[F, Int, Int, Int]
        ()
      }
  }

  object applicative {
    def identity[F[_], X](implicit f: Applicative[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(f.applicativeLaw.identityAp[X] _)

    def homomorphism[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], af: Arbitrary[X => Y], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.homomorphism[X, Y] _)

    def interchange[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[X], afx: Arbitrary[F[X => Y]], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.interchange[X, Y] _)

    def mapApConsistency[F[_], X, Y](implicit ap: Applicative[F], ax: Arbitrary[F[X]], afx: Arbitrary[X => Y], e: Equal[F[Y]]) =
      forAll(ap.applicativeLaw.mapLikeDerived[X, Y] _)

    def laws[F[_]](implicit F: Applicative[F], af: Arbitrary[F[Int]],
                   aff: Arbitrary[F[Int => Int]], e: Equal[F[Int]]): Properties =
      newProperties("applicative") { p =>
        p.include(ScalazProperties.apply.laws[F])
        p.property("identity") = applicative.identity[F, Int]
        p.property("homomorphism") = applicative.homomorphism[F, Int, Int]
        p.property("interchange") = applicative.interchange[F, Int, Int]
        p.property("map consistent with ap") = applicative.mapApConsistency[F, Int, Int]
        ()
      }
  }

  object bind {
    def associativity[M[_], X, Y, Z](implicit M: Bind[M], amx: Arbitrary[M[X]], af: Arbitrary[(X => M[Y])],
                                     ag: Arbitrary[(Y => M[Z])], emz: Equal[M[Z]]) =
      forAll(M.bindLaw.associativeBind[X, Y, Z] _)

    def bindApConsistency[M[_], X, Y](implicit M: Bind[M], amx: Arbitrary[M[X]],
                                      af: Arbitrary[M[X => Y]], emy: Equal[M[Y]]) =
      forAll(M.bindLaw.apLikeDerived[X, Y] _)

    def laws[M[_]](implicit a: Bind[M], am: Arbitrary[M[Int]],
                   af: Arbitrary[Int => M[Int]], ag: Arbitrary[M[Int => Int]], e: Equal[M[Int]]): Properties =
      newProperties("bind") { p =>
        p.include(ScalazProperties.apply.laws[M])

        p.property("associativity") = bind.associativity[M, Int, Int, Int]
        p.property("ap consistent with bind") = bind.bindApConsistency[M, Int, Int]
        ()
      }
  }

  object monad {
    def rightIdentity[M[_], X](implicit M: Monad[M], e: Equal[M[X]], a: Arbitrary[M[X]]) =
      forAll(M.monadLaw.rightIdentity[X] _)

    def leftIdentity[M[_], X, Y](implicit am: Monad[M], emy: Equal[M[Y]], ax: Arbitrary[X], af: Arbitrary[(X => M[Y])]) =
      forAll(am.monadLaw.leftIdentity[X, Y] _)

    def laws[M[_]](implicit a: Monad[M], am: Arbitrary[M[Int]],
                   af: Arbitrary[Int => M[Int]], ag: Arbitrary[M[Int => Int]], e: Equal[M[Int]]): Properties =
      newProperties("monad") { p =>
        p.include(applicative.laws[M])
        p.include(bind.laws[M])
        p.property("right identity") = monad.rightIdentity[M, Int]
        p.property("left identity") = monad.leftIdentity[M, Int, Int]
        ()
      }
  }

  object monadPlus {
    def emptyMap[F[_], X](implicit f: MonadPlus[F], afx: Arbitrary[X => X], ef: Equal[F[X]]) =
      forAll(f.monadPlusLaw.emptyMap[X] _)

    def leftZero[F[_], X](implicit F: MonadPlus[F], afx: Arbitrary[X => F[X]], ef: Equal[F[X]]) =
      forAll(F.monadPlusLaw.leftZero[X] _)

    def rightZero[F[_], X](implicit F: MonadPlus[F], afx: Arbitrary[F[X]], ef: Equal[F[X]]) =
      forAll(F.strongMonadPlusLaw.rightZero[X] _)

    def laws[F[_]](implicit F: MonadPlus[F], afx: Arbitrary[F[Int]], afy: Arbitrary[F[Int => Int]], ef: Equal[F[Int]]): Properties =
      newProperties("monad plus") { p =>
        p.include(monad.laws[F])
        p.include(plusEmpty.laws[F])
        p.property("empty map") = emptyMap[F, Int]
        p.property("left zero") = leftZero[F, Int]
        ()
      }
    def strongLaws[F[_]](implicit F: MonadPlus[F], afx: Arbitrary[F[Int]], afy: Arbitrary[F[Int => Int]], ef: Equal[F[Int]]) =
      newProperties("monad plus") { p =>
        p.include(laws[F])
        p.property("right zero") = rightZero[F, Int]
        ()
      }
  }

  object monadError {
    def raisedErrorsHandled[F[_], E, A](implicit me: MonadError[F, E], eq: Equal[F[A]], ae: Arbitrary[E], afea: Arbitrary[E => F[A]]) =
      forAll(me.monadErrorLaw.raisedErrorsHandled[A] _)
    def errorsRaised[F[_], E, A](implicit me: MonadError[F, E], eq: Equal[F[A]], ae: Arbitrary[E], aa: Arbitrary[A]) =
      forAll(me.monadErrorLaw.errorsRaised[A] _)
    def errorsStopComputation[F[_], E, A](implicit me: MonadError[F, E], eq: Equal[F[A]], ae: Arbitrary[E], aa: Arbitrary[A]) =
      forAll(me.monadErrorLaw.errorsStopComputation[A] _)

    def laws[F[_], E](implicit me: MonadError[F, E], am: Arbitrary[F[Int]], afap: Arbitrary[F[Int => Int]], aeq: Equal[F[Int]], ae: Arbitrary[E], ce: Cogen[E]): Properties =
      newProperties("monad error") { p =>
        p.include(monad.laws[F])
        p.property("raisedErrorsHandled") = raisedErrorsHandled[F, E, Int]
        p.property("errorsRaised") = errorsRaised[F, E, Int]
        p.property("errorsStopComputation") = errorsStopComputation[F, E, Int]
        ()
      }
  }
}