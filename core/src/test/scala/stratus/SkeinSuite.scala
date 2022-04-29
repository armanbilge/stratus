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

import algebra.ring.CommutativeRig
import cats.Id
import cats.Monad
import cats.data.NonEmptyVector
import cats.data.StateT
import cats.kernel.Eq
import cats.kernel.Order
import cats.laws.discipline.arbitrary.given
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import schrodinger.Dist
import schrodinger.math.syntax.*
import schrodinger.montecarlo.Weighted
import schrodinger.random.all.given
import schrodinger.stats.all.given

class ResamplerTests[F[_], W, A](resampler: Resampler[Dist[W, _], W, A]) extends Laws:
  def resampler(
      using CommutativeRig[W],
      Eq[W],
      Arbitrary[Vector[Weighted[W, A]]],
      Arbitrary[Eagle[W]]): RuleSet =
    DefaultRuleSet(
      "resampler",
      None,
      "conserves expected total weight" -> forAll {
        (samples: Vector[Weighted[W, A]], eagle: Eagle[W]) =>
          val received = resampler
            .resample(eagle)
            .map(_.toVector)
            .whileM[Vector](StateT.inspect(_.nonEmpty))
            .map(_.flatten.map(_.weight))
            .map(CommutativeRig[W].sum(_))
            .runA(samples)

          received.mean === CommutativeRig[W].sum(samples.map(_.weight))
      }
    )

class SkeinSuite extends DisciplineSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMaxSize(6)

  checkAll(
    "Resampler.identity",
    ResamplerTests[Dist[NonNegRational, _], NonNegRational, Long](Resampler.identity).resampler
  )

  property("identity resampler preserves samples") {
    forAll { (samples: Vector[Weighted[NonNegRational, Long]], eagle: Eagle[NonNegRational]) =>
      val resampled = Resampler
        .identity[Dist[NonNegRational, _], NonNegRational, Long]
        .resample(eagle)
        .map(_.toVector)
        .whileM[Vector](StateT.inspect(_.nonEmpty))
        .map(_.flatten)
        .map(_.sorted(using Order[Weighted[NonNegRational, Long]].toOrdering))
        .runA(samples)

      val sortedSamples = samples.sorted(using Order[Weighted[NonNegRational, Long]].toOrdering)

      assertEquals(resampled.support, Map(sortedSamples -> NonNegRational(1)))
    }
  }

  property("random-access vector pop") {
    forAll(
      for
        nev <- arbitrary[NonEmptyVector[Long]]
        v = nev.toVector
        i <- Gen.chooseNum(0, v.length - 1)
      yield (v, i)) { (v, i) =>
      val (vp, vi) = Resampler.pop[Id, Long](i).run(v)
      assertEquals((vp :+ vi).sorted, v.sorted)
    }
  }
