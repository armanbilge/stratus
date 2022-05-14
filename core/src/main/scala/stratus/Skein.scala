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

import cats.FlatMap
import cats.syntax.all.*
import fs2.Pipe
import fs2.Pull
import fs2.Stream
import schrodinger.montecarlo.Weighted

trait Skein[F[_], W]:
  def pipe[A]: Pipe[F, Weighted[W, A], Weighted[W, A]]

object Skein:
  def apply[F[_]: FlatMap, W](
      size: Int,
      eagle: F[Eagle[W]],
      resampler: Resampler[F, W]): Skein[F, W] =
    new:
      def pipe[A] = in =>

        def go(
            flock: Vector[Weighted[W, A]],
            in: Stream[F, Weighted[W, A]]
        ): Pull[F, Weighted[W, A], Unit] =
          in.pull.uncons.flatMap {
            case Some((chunk, tail)) =>
              emitWhile(flock ++ chunk.toVector, size).flatMap(go(_, tail))
            case None =>
              emitWhile(flock, 0) >> Pull.done
          }

        def emitWhile(
            flock: Vector[Weighted[W, A]],
            threshold: Int
        ): Pull[F, Weighted[W, A], Vector[Weighted[W, A]]] =
          if flock.length >= threshold && flock.nonEmpty then
            Pull.eval(eagle.flatMap(resampler.resample(_).run(flock))).flatMap {
              (flock, sample) => Pull.outputOption1(sample) >> emitWhile(flock, threshold)
            }
          else Pull.pure(flock)

        go(Vector.empty, in).stream
