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

package eyes4s.kernel

import eyes4s.kernel.Unit2D.Norm

class AgreementSuite extends munit.FunSuite:

  val a = Frame.of[Norm](FrameId("a"), Bounds.sized[Norm](1, 1).toOption.get, YAxis.Down)
  val b = Frame.of[Norm](FrameId("b"), Bounds.sized[Norm](1, 1).toOption.get, YAxis.Down)

  test("clocks agree only with themselves") {
    assertEquals(Agreement.clocks(ClockId("t"), ClockId("t")), Right(ClockId("t")))
    assertEquals(
      Agreement.clocks(ClockId("t"), ClockId("s")),
      Left(TimeError.ClockMismatch(ClockId("t"), ClockId("s")))
    )
  }

  test("frames agree by identity, not by geometry") {
    // Identical bounds and axis; only the name differs.
    assertEquals(a.bounds, b.bounds)
    assertEquals(Agreement.frames(a, a), Right(a))
    assertEquals(Agreement.frames(a, b), Left(GeometryError.FrameMismatch(a.id, b.id)))
  }

  test("the n-ary form reports the mismatch against the first element") {
    // Folding pairwise would report `b` against whichever neighbour it landed
    // next to. Anchoring on the head gives a stable, explainable message.
    assertEquals(
      Agreement.allFrames(Seq(a, a, b)),
      Left(GeometryError.FrameMismatch(a.id, b.id))
    )
    assertEquals(Agreement.allFrames(Seq(a, a, a)), Right(Some(a)))
    assertEquals(Agreement.allFrames(Seq.empty[Frame[Norm]]), Right(None))
  }

  test("n-ary clocks behave the same way") {
    val t = ClockId("t")
    val s = ClockId("s")
    assertEquals(Agreement.allClocks(Seq(t, t, s)), Left(TimeError.ClockMismatch(t, s)))
    assertEquals(Agreement.allClocks(Seq(t, t)), Right(Some(t)))
    assertEquals(Agreement.allClocks(Seq.empty), Right(None))
  }

  test("every kernel operation that crosses coordinate systems reports the same way") {
    // The convention, asserted rather than described: the three call sites that
    // combine two carriers all produce the same error value for the same
    // mismatch, because they now share one implementation.
    val onT      = Interval.of(ClockId("t"), Instant.millis(0), Instant.millis(10)).toOption.get
    val onS      = Interval.of(ClockId("s"), Instant.millis(0), Instant.millis(10)).toOption.get
    val expected = Left(TimeError.ClockMismatch(ClockId("t"), ClockId("s")))

    assertEquals(onT.overlaps(onS), expected)
    assertEquals(onT.encloses(onS), expected)
    assertEquals(Overlap.OnsetInside.selects(onS, onT), expected)
  }

end AgreementSuite
