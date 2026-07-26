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

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import _root_.fs2.{Pure, Stream}

class StreamingSuite extends munit.FunSuite:

  val clock = ClockId("tracker")

  private def at(ms: Long, x: Double) =
    Sample(Instant.millis(ms), Gaze.Tracked(Pt[Deg](x, 0.0), None))

  private def hold(from: Long, n: Int, x: Double) =
    (0 until n).map(i => at(from + i, x)).toVector

  private def sweep(from: Long, n: Int, x0: Double, x1: Double) =
    (0 until n).map(i => at(from + i, x0 + (x1 - x0) * i / n)).toVector

  /** A realistic pipeline: clean the signal, then detect. */
  private val pipeline: Machine[Sample[Deg], Event[Deg]] =
    Filter
      .median[Deg](2)
      .andThen(Detectors.ivt(Velocity.degPerSecond(30.0).toOption.get, Span.millis(20), clock))

  private val input =
    hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0) ++
      sweep(150, 20, 30.0, 5.0) ++ hold(170, 80, 5.0)

  private def streamed(xs: Vector[Sample[Deg]], chunkSize: Int): Vector[Event[Deg]] =
    Stream
      .emits(xs)
      .chunkLimit(chunkSize)
      .flatMap(Stream.chunk)
      .through(pipeline.toPipe[Pure])
      .toVector

  // -------------------------------------------------------------------------
  // The claim, tested
  // -------------------------------------------------------------------------

  test("runAll and toPipe agree on finite input") {
    assertEquals(streamed(input, 16), pipeline.runAll(input))
  }

  test("the agreement holds for the identical machine value, not a copy") {
    // Constructed once, driven twice. If the two runtimes needed different
    // definitions the claim would be about a specification, not about code.
    val m: Machine[Sample[Deg], Event[Deg]] = pipeline
    assertEquals(
      Stream.emits(input).through(m.toPipe[Pure]).toVector,
      m.runAll(input)
    )
  }

  test("chunking does not change the result") {
    // Where a streaming driver usually breaks: state carried across a chunk
    // boundary, or a flush run per chunk rather than once at the end.
    val reference = pipeline.runAll(input)
    Seq(1, 2, 3, 7, 64, 1000).foreach { n =>
      assertEquals(streamed(input, n), reference, clue(n))
    }
  }

  test("an empty stream still flushes, and yields nothing") {
    assertEquals(streamed(Vector.empty, 8), Vector.empty[Event[Deg]])
    assertEquals(pipeline.runAll(Vector.empty), Vector.empty[Event[Deg]])
  }

  // -------------------------------------------------------------------------
  // The caveat, also tested
  // -------------------------------------------------------------------------

  test("without termination the in-progress event is not emitted") {
    // The final fixation runs to the end of the input, so it exists only
    // because the stream ended and flush ran. Truncating the stream before the
    // end -- the live case -- must not produce it.
    val full = streamed(input, 8)

    val truncated = Stream
      .emits(input)
      .through(pipeline.toPipe[Pure])
      .take(full.length.toLong - 1)
      .toVector

    assertEquals(truncated.length, full.length - 1)
    assert(!truncated.contains(full.last))
  }

  test("events appear as their input arrives, not only at the end") {
    // The point of streaming: a gaze-contingent loop needs the first fixation
    // before the trial is over.
    val prefix = Stream.emits(input).through(pipeline.toPipe[Pure]).take(1).toVector
    assertEquals(prefix.length, 1)
    assert(prefix.head.isInstanceOf[Event.Fixation[Deg]])
  }

end StreamingSuite
