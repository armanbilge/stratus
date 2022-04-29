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

import cats.Id
import cats.data.NonEmptyVector
import cats.laws.discipline.arbitrary.given
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import schrodinger.Dist
import schrodinger.montecarlo.Weighted
import schrodinger.random.all.given
import schrodinger.stats.all.given
import cats.data.StateT
import cats.kernel.Order

class SkeinSuite extends DisciplineSuite:

  property("identity resampler preserves samples") {
    forAll { (samples: Vector[Weighted[NonNegRational, Long]], eagle: Eagle[NonNegRational]) =>
      val received = Resampler
        .identity[Dist[NonNegRational, _], NonNegRational, Long]
        .resample(eagle)
        .map(_.toVector)
        .whileM[Vector](StateT.inspect(_.nonEmpty))
        .map(_.flatten)
        // .map(_.sorted(using Order[Weighted[NonNegRational, Long]].toOrdering))
        .runA(samples)

      ???
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
