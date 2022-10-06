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
import algebra.ring.CommutativeSemifield
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
import schrodinger.kernel.testkit.Dist
import schrodinger.math.syntax.*
import schrodinger.montecarlo.Weighted

class ResamplerTests[F[_], W, A](resampler: Resampler[Dist[W, _], W]) extends Laws:
  def resampler[A](using
      CommutativeSemifield[W],
      Eq[W],
      Arbitrary[NonEmptyVector[Weighted[W, A]]],
      Arbitrary[Eagle[W]],
  ): RuleSet =
    DefaultRuleSet(
      "resampler",
      None,
      "conserves expected total weight" -> forAll {
        (samples: NonEmptyVector[Weighted[W, A]], eagle: Eagle[W]) =>
          val received = resampler
            .resample[A](eagle |+| Eagle(samples.map(_.weight)))
            .whileM[Vector](StateT.inspect(_.nonEmpty))
            .map(_.flatten.map(_.weight))
            .map(CommutativeRig[W].sum(_))
            .runA(samples.toVector)

          received.mean === CommutativeRig[W].sum(samples.map(_.weight).toVector)
      },
    )

class ResamplerSuite extends DisciplineSuite:

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMaxSize(
        if System.getProperty("java.vm.name") == "Scala Native" then 3 else 4,
      )

  checkAll(
    "Resampler.identity",
    ResamplerTests[Dist[NonNegRational, _], NonNegRational, Long](Resampler.identity).resampler,
  )

  checkAll(
    "Resampler.targetMeanWeight",
    ResamplerTests[Dist[NonNegRational, _], NonNegRational, Long](
      Resampler.targetMeanWeight,
    ).resampler,
  )

  property("identity resampler preserves samples") {
    forAll { (samples: Vector[Weighted[NonNegRational, Long]], eagle: Eagle[NonNegRational]) =>
      val resampled = Resampler
        .identity[Dist[NonNegRational, _], NonNegRational]
        .resample[Long](eagle)
        .map(_.toVector)
        .whileM[Vector](StateT.inspect(_.nonEmpty))
        .map(_.flatten)
        .map(_.sorted(using Order[Weighted[NonNegRational, Long]].toOrdering))
        .runA(samples)

      val sortedSamples = samples.sorted(using Order[Weighted[NonNegRational, Long]].toOrdering)

      assertEquals(resampled.support, Map(sortedSamples -> NonNegRational(1)))
    }
  }

  property("targetMeanWeight resampler targets mean weight") {
    forAll {
      (
          samples: NonEmptyVector[Weighted[NonNegRational, Long]],
          eagle0: Eagle[NonNegRational],
      ) =>
        val eagle = eagle0 |+| Eagle(samples.map(_.weight))

        val resampled = Resampler
          .targetMeanWeight[Dist[NonNegRational, _], NonNegRational]
          .resample[Long](eagle)
          .whileM[Vector](StateT.inspect(_.nonEmpty))
          .map(_.flatten)
          .runA(samples.toVector)

        for
          resample <- resampled.support.keySet
          sample <- resample
        do assertEquals(sample.weight, eagle.meanWeight)
    }
  }

  property("splitting weighted conserves weight") {
    forAll { (wa: Weighted[NonNegRational, Long], b: Byte) =>
      val wp = NonNegRational(1, b.toInt.abs + 1) * wa.weight
      val (wa1, wa2) = Resampler.split(wa, wp)
      assertEquals(wa1.weight + wa2.weight, wa.weight)
    }
  }

  property("random-access vector pop") {
    forAll { (nev: NonEmptyVector[Long]) =>
      val v = nev.toVector
      forAll(Gen.chooseNum(0, v.length - 1)) { i =>
        val (vp, vi) = Resampler.pop[Id, Long](i).run(v)
        assertEquals((vp :+ vi).sorted, v.sorted)
      }
    }
  }
