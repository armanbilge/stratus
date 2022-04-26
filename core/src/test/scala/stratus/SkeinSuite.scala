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
import cats.laws.discipline.arbitrary.given
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import cats.data.NonEmptyVector

class SkeinSuite extends DisciplineSuite:

  property("random-access vector pop") {
    forAll(
      for
        nev <- arbitrary[NonEmptyVector[Long]]
        v = nev.toVector
        i <- Gen.chooseNum(0, v.length - 1)
      yield (v, i)) { (v, i) =>
      val (vp, vi) = Resampler.pop[Id, Long](i).run(v)
      assertEquals(vi, v(i))
      assertEquals(vp.sorted, v.indices.filterNot(_ == i).map(v).toVector.sorted)
    }
  }
