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

import munit.DisciplineSuite
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import algebra.ring.Semifield
import cats.kernel.laws.discipline.EqTests
import cats.kernel.laws.discipline.CommutativeMonoidTests

class EagleSuite extends DisciplineSuite:

  checkAll("Eagle", EqTests[Eagle[PosRational]].eqv)
  checkAll("Eagle", CommutativeMonoidTests[Eagle[PosRational]].commutativeMonoid)
