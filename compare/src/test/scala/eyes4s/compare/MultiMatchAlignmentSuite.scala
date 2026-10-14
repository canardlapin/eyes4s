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

package eyes4s.compare

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class MultiMatchAlignmentSuite extends munit.FunSuite:
  private val ComponentOracleTolerance = 1e-12
  private val frame                    = Frame
    .screen("multimatch-tie-fixture", 1024, 768)
    .fold(error => fail(error.message), identity)
  private val clock                             = ClockId("multimatch-tie-fixture")
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def scanpath(
      points: Vector[(Double, Double)],
      durations: Vector[Long] = Vector.empty,
      sourceClock: ClockId = clock,
      shiftMicros: Long = 0L,
      spacingMicros: Long = 150000L,
      inFrame: Frame[Px] = frame
  ): Scanpath[Px] =
    get(
      Scanpath.of(
        inFrame,
        sourceClock,
        IArray.from(points.zipWithIndex.map { (point, index) =>
          val duration = durations.lift(index).getOrElse(100000L)
          val onset    = shiftMicros + index * spacingMicros
          get(
            Event.Fixation.of(
              get(
                Interval
                  .of(sourceClock, Instant.micros(onset), Instant.micros(onset + duration))
              ),
              Pt[Px](point._1, point._2),
              0.0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        })
      )
    )

  private def values(score: MultiMatchScore): Vector[Double] =
    Vector(score.shape, score.direction, score.length, score.position, score.duration)

  MultiMatchAlignmentReference.cases.foreach { fixture =>
    test(s"independent exhaustive-path oracle and all components: ${fixture.name}") {
      val left  = scanpath(fixture.left)
      val right = scanpath(fixture.right)
      val path  = get(MultiMatch.alignment(left, right))
      assertEquals(path.matches, fixture.selected)
      assertEquals(
        get(MultiMatch.alignment(right, left)).matches,
        fixture.selected.map((i, j) => (j, i))
      )
      assertEquals(get(MultiMatch.alignment(right, left)).cost, path.cost)
      val score = get(MultiMatch[Px].compare(left, right))
      assertEquals(get(MultiMatch[Px].compare(right, left)), score)
      values(score).zip(fixture.expected).foreach { (actual, expected) =>
        assertEqualsDouble(actual, expected, ComponentOracleTolerance, clue((actual, expected)))
      }
    }
  }

  test("near costs smaller than a score tolerance still select different correspondences") {
    val positive = MultiMatchAlignmentReference.cases(1)
    val negative = MultiMatchAlignmentReference.cases(2)
    val a = get(MultiMatch[Px].compare(scanpath(positive.left), scanpath(positive.right)))
    val b = get(MultiMatch[Px].compare(scanpath(negative.left), scanpath(negative.right)))
    assert(math.abs(a.direction - b.direction) > 0.1)
    assert(positive.selected != negative.selected)
  }

  private def scalarPath(
      start: Double,
      vectors: Vector[Int],
      durations: Vector[Long]
  ): Scanpath[Px] =
    val x = vectors.scanLeft(start)((value, movement) => value + movement)
    scanpath(x.map(value => (value, 100.0)), durations, spacingMicros = 1000000L)

  test("vertical versus horizontal tie is symmetric in canonical content order") {
    val a = scalarPath(100.0, Vector(0, 1, 0), Vector(100000L, 200000L, 300000L, 400000L))
    val b = scalarPath(200.0, Vector(1, 0, 1), Vector(150000L, 250000L, 350000L, 450000L))
    val expected = Vector((0, 0), (0, 1), (1, 2), (2, 2))
    assertEquals(get(MultiMatch.alignment(a, b)).matches, expected)
    assertEquals(get(MultiMatch.alignment(b, a)).matches, expected.map((i, j) => (j, i)))
    assertEquals(get(MultiMatch[Px].compare(a, b)), get(MultiMatch[Px].compare(b, a)))
  }

  test("all 729 paired ternary movement sequences preserve exact reversal") {
    val sequences = (for a <- 0 to 2; b <- 0 to 2; c <- 0 to 2 yield Vector(a, b, c)).toVector
    sequences.foreach { left =>
      sequences.foreach { right =>
        val a       = scalarPath(100.0, left, Vector(100000L, 200000L, 300000L, 400000L))
        val b       = scalarPath(200.0, right, Vector(150000L, 250000L, 350000L, 450000L))
        val path    = get(MultiMatch.alignment(a, b))
        val reverse = get(MultiMatch.alignment(b, a))
        assertEquals(reverse.matches, path.matches.map((i, j) => (j, i)), clue((left, right)))
        assertEquals(reverse.cost, path.cost)
        assertEquals(get(MultiMatch[Px].compare(a, b)), get(MultiMatch[Px].compare(b, a)))
      }
    }
  }

  test("absolute onsets and clock names cannot select a different correspondence") {
    val fixture = MultiMatchAlignmentReference.cases.head
    val a       = scanpath(fixture.left)
    val b       = scanpath(fixture.right)
    val movedA  =
      scanpath(fixture.left, sourceClock = ClockId("other-left"), shiftMicros = 9000000L)
    val movedB =
      scanpath(fixture.right, sourceClock = ClockId("other-right"), shiftMicros = 1000000L)
    assertEquals(get(MultiMatch.alignment(movedA, movedB)), get(MultiMatch.alignment(a, b)))
    assertEquals(get(MultiMatch[Px].compare(movedA, movedB)), get(MultiMatch[Px].compare(a, b)))
  }

  test("finite inputs whose accumulated alignment cost overflows fail as a named value") {
    val giant =
      Frame.of[Px](FrameId("huge-pixels"), get(Bounds.of[Px](0.0, 0.0, 1e308, 1.0)), YAxis.Down)
    val a = scanpath(Vector((0.0, 0.0), (9.5e307, 0.0), (0.0, 0.0)), inFrame = giant)
    val b = scanpath(Vector((9.5e307, 0.0), (0.0, 0.0), (9.5e307, 0.0)), inFrame = giant)
    assertEquals(
      MultiMatch.alignment(a, b),
      Left(
        CompareError.InvalidScore(
          "MultiMatch alignment",
          ComparisonValueError.NonFiniteMeasureDistance(Double.PositiveInfinity)
        )
      )
    )
    assertEquals(MultiMatch[Px].compare(a, b), MultiMatch[Px].compare(b, a))
    assert(MultiMatch[Px].compare(a, b).isLeft)
  }

  test("identity and existing frame/cardinality failures retain their scientific contracts") {
    val path = scanpath(MultiMatchAlignmentReference.cases.head.left)
    assertEquals(values(get(MultiMatch[Px].compare(path, path))), Vector.fill(5)(1.0))
    val foreign =
      get(Scanpath.of(frame.withId(FrameId("different-display")), clock, path.fixations))
    assert(MultiMatch.alignment(path, foreign).left.exists {
      case CompareError.Frames(_) => true
      case _                      => false
    })
    assertEquals(
      MultiMatch[Px].compare(path, foreign).left.toOption,
      MultiMatch.alignment(path, foreign).left.toOption
    )
    val one = scanpath(Vector((100.0, 100.0)))
    assertEquals(
      MultiMatch.alignment(one, path),
      Left(CompareError.TooShort("a scanpath", 1, 2))
    )
    assertEquals(
      MultiMatch.alignment(path, one),
      Left(CompareError.TooShort("b scanpath", 1, 2))
    )
    assertEquals(
      MultiMatch.alignmentConvention,
      MultiMatchAlignmentConvention.CanonicalOperandsExactCostTracebackV1
    )
  }
