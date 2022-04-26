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

import cats.Monad
import cats.data.StateT
import cats.instances.stream
import schrodinger.montecarlo.Weighted

trait Resampler[F[_], W, A]:
  def resample(eagle: Eagle[W]): StateT[F, Vector[Weighted[W, A]], Option[Weighted[W, A]]]

object Resampler:
  private[stratus] def pop[F[_]: Monad, A](i: Int): StateT[F, Vector[A], A] =
    for
      v <- StateT.get
      _ <- StateT.set {
        if i < v.length - 1 then v.init.updated(i, v.last)
        else v.init
      }
    yield v(i)
