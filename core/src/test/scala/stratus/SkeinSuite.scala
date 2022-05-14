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

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.Stream
import munit.CatsEffectSuite
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllF
import schrodinger.RVIO
import schrodinger.kernel.DiscreteUniform
import schrodinger.montecarlo.Weighted
import schrodinger.unsafe.rng.SplitMix

class SkeinSuite extends CatsEffectSuite, ScalaCheckSuite:
  
  test("sample size maintained above threshold") {
    forAllF(Gen.size) { x =>
      
      RVIO.algebra[SplitMix].flatMap {
        case given RVIO.Algebra[SplitMix] =>
          Stream.eval(Ref.of(Eagle.eaglet[NonNegRational])).flatMap { eagle =>
            Stream.repeatEval {
              DiscreteUniform(0 to 128)
                .map(w => Weighted(NonNegRational(w), NonNegRational(0), ()))
                .flatTap(wu => eagle.update(_.observe(wu.weight)))
            }
          }
          ???
      }

      IO.unit
    }
  }
