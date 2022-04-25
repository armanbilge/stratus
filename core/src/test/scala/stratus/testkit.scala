package stratus

import algebra.ring.CommutativeSemifield
import algebra.ring.Semifield
import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import spire.laws.arb.rational
import spire.math.Rational

import Arbitrary.arbitrary

opaque type PosRational = Rational

object PosRational:
  given Arbitrary[PosRational] = Arbitrary(rational.arbitrary.map(_.abs))
  given Cogen[PosRational] =
    Cogen[(BigInt, BigInt)].contramap(r => (r.numerator.toBigInt, r.denominator.toBigInt))
  given Eq[PosRational] = Rational.RationalAlgebra
  given CommutativeSemifield[PosRational] = Rational.RationalAlgebra

given [W: Semifield](using Arbitrary[List[W]]): Arbitrary[Eagle[W]] =
  Arbitrary(arbitrary[List[W]].map(_.foldLeft(Eagle.eaglet)(_.observe(_))))

given [W](using cogen: Cogen[(Long, W, W)]): Cogen[Eagle[W]] =
  cogen.contramap(e => (e.observationCount, e.meanWeight, e.meanSquaredWeight))
