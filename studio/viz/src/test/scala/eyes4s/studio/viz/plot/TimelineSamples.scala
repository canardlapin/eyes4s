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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.plot.{PlotSource, Timeline, TimelineColumns, TimelineFixation}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.selection.FixationIndex
import org.scalacheck.Gen

/** Timelines for the timeline plot's tests (ticket S4.5e). */
object TimelineSamples:

  val trial: TrialKey = TrialKey("P17", Phase.Encoding, "enc_03", 1)

  val columns: TimelineColumns =
    TimelineColumns.standard.fold(e => throw new AssertionError(e.message), identity)

  private def index(i: Int) =
    FixationIndex.of(i).fold(e => throw new AssertionError(e), identity)

  /** A timeline of `(onset, duration)` intervals, numbered from 1. */
  def timeline(intervals: Vector[(Long, Long)]): Timeline =
    Timeline
      .of(
        trial,
        intervals.zipWithIndex.map { case ((on, du), k) =>
          TimelineFixation(index(k + 1), on, du)
        }
      )
      .fold(e => throw new AssertionError(e.message), identity)

  def source(t: Timeline): PlotSource =
    Timeline.source(t, columns).fold(e => throw new AssertionError(e.message), identity)

  /** The Explore board's 13 fixations of P17 enc_03 (onset, duration ms):
    * test values in the board's rhythm, not results.
    */
  val board: Timeline = timeline(
    Vector(
      (0L, 180L),
      (336L, 320L),
      (812L, 260L),
      (1228L, 380L),
      (1764L, 240L),
      (2160L, 412L),
      (2652L, 200L),
      (2932L, 350L),
      (3362L, 220L),
      (3662L, 300L),
      (4042L, 260L),
      (4382L, 110L),
      (4572L, 360L)
    )
  )

  /** Up to twenty fixations with gaps and durations of 1 ms to a second. */
  val genTimeline: Gen[Timeline] =
    for
      n         <- Gen.choose(0, 20)
      intervals <- Gen.listOfN(n, Gen.zip(Gen.choose(0L, 400L), Gen.choose(1L, 1000L)))
    yield timeline(
      intervals.toVector
        .scanLeft((0L, 0L)) { case ((on, du), (gap, next)) => (on + du + gap, next) }
        .drop(1)
    )
