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

import algebra.ring.AdditiveMonoid
import algebra.ring.CommutativeSemifield
import algebra.ring.Rig
import algebra.ring.Semifield
import cats.Foldable
import cats.Show
import cats.derived.*
import cats.kernel.CommutativeMonoid
import cats.kernel.Eq
import cats.kernel.Monoid
import cats.syntax.all.*
import schrodinger.math.syntax.*

import scala.annotation.tailrec
import scala.util.NotGiven

import Eagle.*

final case class Eagle[W](
    observationCount: Long,
    meanWeight: W,
    meanSquaredWeight: W
) derives Eq,
      Show:

  def effectiveSampleSize(using W: Semifield[W], eq: Eq[W]): W =
    relativeEffectiveSampleSize * W.fromBigInt(observationCount)

  def relativeEffectiveSampleSize(using W: Semifield[W], eq: Eq[W]): W =
    if W.isZero(meanSquaredWeight) then W.zero
    else (meanWeight * meanWeight) / meanSquaredWeight

  def observe(weight: W)(using W: Semifield[W]): Eagle[W] =
    val observationCount = W.fromBigInt(this.observationCount)
    val newObservationCount = W.fromBigInt(this.observationCount + 1)
    val correction = observationCount / newObservationCount
    Eagle(
      this.observationCount + 1,
      meanWeight * correction + weight / newObservationCount,
      meanSquaredWeight * correction + (weight * weight) / newObservationCount
    )

object Eagle:
  def apply[F[_]: Foldable, W](weights: F[W])(using W: Semifield[W]): Eagle[W] =
    val count = weights.size
    val n = W.fromBigInt(count)
    val meanWeight = W.sum(weights.toIterable) / n
    val meanSquaredWeight = W.sum(weights.toIterable.map(x => x * x)) / n
    Eagle(count, meanWeight, meanSquaredWeight)

  def eaglet[W](using W: AdditiveMonoid[W]): Eagle[W] =
    Eagle(0, W.zero, W.zero)

  given [W: Semifield](using NotGiven[CommutativeSemifield[W]]): Monoid[Eagle[W]] =
    new EagleMonoid

  given [W: CommutativeSemifield]: CommutativeMonoid[Eagle[W]] =
    new EagleMonoid[W] with CommutativeMonoid[Eagle[W]]

  private class EagleMonoid[W](using W: Semifield[W]) extends Monoid[Eagle[W]]:
    def empty = eaglet[W]

    def combine(x: Eagle[W], y: Eagle[W]) =
      if x.observationCount == 0 then y
      else if y.observationCount == 0 then x
      else
        val observationCount = W.fromBigInt(x.observationCount + y.observationCount)
        val xCorrection = W.fromBigInt(x.observationCount) / observationCount
        val yCorrection = W.fromBigInt(y.observationCount) / observationCount

        Eagle(
          x.observationCount + y.observationCount,
          x.meanWeight * xCorrection + y.meanWeight * yCorrection,
          x.meanSquaredWeight * xCorrection + y.meanSquaredWeight * yCorrection
        )
