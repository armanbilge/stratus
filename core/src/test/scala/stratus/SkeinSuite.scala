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

import cats.data.StateT
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import fs2.Stream
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.effect.PropF.forAllF
import schrodinger.RVIO
import schrodinger.kernel.DiscreteUniform
import schrodinger.montecarlo.Weighted
import schrodinger.testkit.arbitrary.given
import schrodinger.unsafe.rng.SplitMix

import scala.concurrent.duration.*

class SkeinSuite extends CatsEffectSuite, ScalaCheckEffectSuite:

  override def munitIOTimeout = 1.minute

  test("sample size maintained above threshold") {
    forAllF(Gen.size, arbitrary[SplitMix]) { (skeinSize, splitMix) =>
      RVIO.algebra[SplitMix].flatMap { case given RVIO.Algebra[SplitMix] =>
        val rv = Ref.of(Eagle.eaglet[NonNegRational]).flatMap { eagle =>
          Stream
            .repeatEval {
              DiscreteUniform(0 to 128)
                .map(w => Weighted(NonNegRational(w), NonNegRational(1), ()))
                .flatTap(wu => eagle.update(_.observe(wu.weight)))
            }
            .through(
              Skein(
                skeinSize,
                eagle.get,
                new:
                  def resample[A](eagle: Eagle[NonNegRational]) =
                    StateT
                      .inspectF[RVIO[SplitMix, _], Vector[Weighted[NonNegRational, A]], Unit](
                        samples => RVIO.eval(IO(assert(samples.nonEmpty))),
                      )
                      >> Resampler.identity.resample(eagle),
              ).pipe,
            )
            .take(100 * skeinSize)
            .compile
            .drain
        }

        rv.simulate(splitMix)
      }
    }
  }

  test("terminates") {
    forAllF(
      arbitrary[List[Weighted[NonNegRational, Unit]]],
      Gen.size,
      arbitrary[SplitMix],
    ) { (samples, skeinSize, splitMix) =>
      RVIO.algebra[SplitMix].flatMap { case given RVIO.Algebra[SplitMix] =>
        val rv = Ref.of(Eagle.eaglet[NonNegRational]).flatMap { eagle =>
          Stream
            .emits(samples)
            .through(
              Skein(
                skeinSize,
                eagle.get,
                Resampler.identity,
              ).pipe,
            )
            .compile
            .drain
        }

        rv.simulate(splitMix)
      }

    }
  }
