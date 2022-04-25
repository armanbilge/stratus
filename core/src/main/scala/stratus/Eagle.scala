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
import algebra.ring.Rig
import algebra.ring.Semifield
import cats.Show
import cats.derived.*
import cats.kernel.Eq
import cats.kernel.Monoid
import schrodinger.math.syntax.*

import scala.annotation.tailrec

import Eagle.*
import cats.kernel.CommutativeMonoid
import algebra.ring.CommutativeSemifield
import scala.util.NotGiven

final case class Eagle[W](
    observationCount: Long,
    meanWeight: W,
    meanSquaredWeight: W
) derives Eq,
      Show:
  def effectiveSampleSize(using W: Semifield[W], eq: Eq[W]): W =
    relativeEffectiveSampleSize * fromLong(observationCount)

  def relativeEffectiveSampleSize(using W: Semifield[W], eq: Eq[W]): W =
    if W.isZero(meanSquaredWeight) then W.zero
    else W.pow(meanWeight, 2) / meanSquaredWeight

  def observe(weight: W)(using W: Semifield[W]): Eagle[W] =
    val observationCount = fromLong(this.observationCount)
    val newObservationCount = fromLong(this.observationCount + 1)
    val correction = observationCount / newObservationCount
    Eagle(
      this.observationCount + 1,
      meanWeight * correction + weight / newObservationCount,
      meanSquaredWeight * correction + W.pow(weight, 2) / newObservationCount
    )

object Eagle:
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
        val observationCount = fromLong(x.observationCount + y.observationCount)
        val xCorrection = fromLong(x.observationCount) / observationCount
        val yCorrection = fromLong(y.observationCount) / observationCount

        Eagle(
          x.observationCount + y.observationCount,
          x.meanWeight * xCorrection + y.meanWeight * yCorrection,
          x.meanSquaredWeight * xCorrection + y.meanSquaredWeight * yCorrection
        )

  private def fromLong[W](n: Long)(using W: Rig[W]): W =
    def fromInt(n: Int) = W.sumN(W.one, n.toInt)

    if n.isValidInt then fromInt(n.toInt)
    else
      val d = fromInt(1 << 30)
      val mask = (1L << 30) - 1
      @tailrec def loop(k: W, x: BigInt, acc: W): W =
        if x.isValidInt then k * fromInt(x.toInt) + acc
        else
          val y = x >> 30
          val r = fromInt((x & mask).toInt)
          loop(d * k, y, k * r + acc)

      loop(W.one, n.abs, W.zero)
