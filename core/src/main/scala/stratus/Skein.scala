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

import fs2.Pipe
import fs2.Pull
import fs2.Stream
import schrodinger.montecarlo.Weighted

trait Skein[F[_], W]:
  def pipe[A]: Pipe[F, Weighted[W, A], Weighted[W, A]]

object Skein:
  def apply[F[_], W](size: Int, eagle: F[Eagle[W]], resampler: Resampler[F, W]): Skein[F, W] =
    new:
      def pipe[A] = in =>

        case class State(
            flock: Vector[Weighted[W, A]] = Vector.empty,
            finalFlight: Boolean = false
        )

        def go(in: Stream[F, Weighted[W, A]]): Pull[F, Weighted[W, A], State] =
          in.pull.uncons
          ???

        ???
