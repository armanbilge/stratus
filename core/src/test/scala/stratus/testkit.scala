/*
 * Copyright 2022 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package stratus

import algebra.ring.CommutativeSemifield
import algebra.ring.Semifield
import cats.kernel.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import spire.laws.arb.rational
import spire.math.Rational

import Arbitrary.arbitrary

opaque type NonNegRational = Rational

object NonNegRational:
  def apply(n: Int): NonNegRational =
    require(n >= 0)
    Rational(n, 1)

  given Arbitrary[NonNegRational] = Arbitrary(
    for
      n <- arbitrary[Byte]
      d <- arbitrary[Byte].map(n => if n == 0 then 1 else n)
    yield Rational(n.abs, d)
  )
  given Cogen[NonNegRational] =
    Cogen[(BigInt, BigInt)].contramap(r => (r.numerator.toBigInt, r.denominator.toBigInt))
  given Eq[NonNegRational] = Rational.RationalAlgebra
  given CommutativeSemifield[NonNegRational] = Rational.RationalAlgebra

given [W: Semifield](using Arbitrary[List[W]]): Arbitrary[Eagle[W]] =
  Arbitrary(arbitrary[List[W]].map(_.foldLeft(Eagle.eaglet)(_.observe(_))))

given [W](using cogen: Cogen[(Long, W, W)]): Cogen[Eagle[W]] =
  cogen.contramap(e => (e.observationCount, e.meanWeight, e.meanSquaredWeight))
