/*
 * Copyright 2026 canardlapin
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

package eyes4s.fs2

import eyes4s.kernel.Machine

import _root_.fs2.{Chunk, Pipe, Pull, Stream}

/** The streaming half of "one definition, two runtimes".
  *
  * A [[Machine]] is a pure state machine with no opinion about where its input
  * comes from. `runAll` drives it over a collection in a pure module; [[toPipe]]
  * drives the identical value over a live stream. The detector is not
  * reimplemented, adapted, or wrapped -- it is the same value, and the property
  * test in this module checks that the two agree.
  *
  * This is what lets a gaze-contingent experiment and an offline analysis share
  * code rather than merely share a specification.
  */
extension [I, O](m: Machine[I, O])

  /** Drive this machine over a stream.
    *
    * ==Flush semantics, stated rather than discovered==
    *
    * `flush` runs when the input ends, so a **finite** stream produces exactly
    * what `runAll` produces. An unbounded live stream never ends, so the event
    * in progress is never emitted. That is correct -- it has not finished
    * happening -- but it means the agreement between the two runtimes holds for
    * finite input only, which is what PRD D-5 claims and no more.
    */
  def toPipe[F[_]]: Pipe[F, I, O] =
    in =>
      def go(state: m.S, rest: Stream[F, I]): Pull[F, O, Unit] =
        rest.pull.uncons.flatMap {
          case Some((chunk, tail)) =>
            var cur = state
            val out = Vector.newBuilder[O]
            chunk.foreach { i =>
              val (next, produced) = m.detector.step(cur, i)
              cur = next
              out ++= produced
            }
            Pull.output(Chunk.from(out.result())) >> go(cur, tail)
          case None =>
            Pull.output(Chunk.from(m.detector.flush(state)))
        }

      go(m.detector.init, in).stream
