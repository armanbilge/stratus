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

import algebra.ring.Semifield
import cats.data.NonEmptyList
import cats.kernel.laws.discipline.CommutativeMonoidTests
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.arbitrary.given
import cats.syntax.all.*
import munit.DisciplineSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import schrodinger.math.syntax.*
import spire.math.Rational

class EagleSuite extends DisciplineSuite:

  checkAll("Eagle", EqTests[Eagle[NonNegRational]].eqv)
  checkAll("Eagle", CommutativeMonoidTests[Eagle[NonNegRational]].commutativeMonoid)

  property("track means") {
    forAll { (weights: NonEmptyList[NonNegRational]) =>
      val expected = Eagle(weights)
      val obtained = weights.foldLeft(Eagle.eaglet[NonNegRational])(_.observe(_))
      assertEquals(obtained, expected)
    }
  }

  property("observe consistent with monoid") {
    forAll { (eagle: Eagle[NonNegRational], observation: NonNegRational) =>
      assertEquals(
        eagle.observe(observation),
        eagle |+| Eagle(1, observation, observation * observation),
      )
    }
  }
