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

class CrqaSuite extends munit.FunSuite:

  private val frame = Frame.screen("crqa", 100, 100).toOption.get
  private val clock = ClockId("crqa")

  private def path(points: (Double, Double)*): Scanpath[Px] =
    val fixations = points.zipWithIndex.map { case ((x, y), index) =>
      Event.Fixation
        .of(
          Interval
            .of(
              clock,
              Instant.millis(index.toLong * 20L),
              Instant.millis(index.toLong * 20L + 10L)
            )
            .toOption
            .get,
          Pt[Px](x, y),
          0.0,
          DispersionMethod.RmsRadius,
          1
        )
        .toOption
        .get
    }
    Scanpath.of(frame, clock, IArray.from(fixations)).toOption.get

  private def dimension(value: Int): EmbeddingDimension =
    EmbeddingDimension.of(value).toOption.get

  private def delay(value: Int): EmbeddingDelay =
    EmbeddingDelay.of(value).toOption.get

  private def minimum(value: Int): LineLengthMinimum =
    LineLengthMinimum.of(value).toOption.get

  private def fixed(
      radius: Double,
      embeddingDimension: Int = 1,
      embeddingDelay: Int = 1,
      lineMinimum: Int = 2,
      axis: LaminarAxis = LaminarAxis.AlongLeft
  ): CrqaConfig[Px] =
    CrqaConfig(
      dimension(embeddingDimension),
      delay(embeddingDelay),
      RadiusSelection.FixedInclusive(Distance.px(radius).toOption.get),
      minimum(lineMinimum),
      minimum(lineMinimum),
      axis
    )

  test("parameter smart constructors reject invalid operands") {
    assertEquals(
      EmbeddingDimension.of(0),
      Left(CrqaParameterError.NonPositiveEmbeddingDimension(0))
    )
    assertEquals(
      EmbeddingDelay.of(-1),
      Left(CrqaParameterError.NonPositiveEmbeddingDelay(-1))
    )
    assertEquals(
      LineLengthMinimum.of(0),
      Left(CrqaParameterError.NonPositiveLineMinimum(0))
    )
    TargetRecurrenceRate.of(Double.NaN) match
      case Left(CrqaParameterError.InvalidTargetRecurrenceRate(value)) => assert(value.isNaN)
      case other => fail(s"unexpected result: $other")
    assert(TargetRecurrenceRate.of(0.0).isLeft)
    assert(TargetRecurrenceRate.of(1.01).isLeft)
  }

  test("an identity recurrence matrix has the analytic RR, DET, ENTR, and L_max") {
    val result = Crqa
      .analyse(
        path((0, 0), (1, 0), (2, 0), (3, 0)),
        path((0, 0), (1, 0), (2, 0), (3, 0)),
        fixed(0.0)
      )
      .toOption
      .get

    assertEquals(
      result.matrix.toRows,
      Vector.tabulate(4)(i => Vector.tabulate(4)(j => i == j))
    )
    assertEqualsDouble(result.metrics.recurrenceRate, 0.25, 1e-12)
    assertEqualsDouble(result.metrics.determinism, 1.0, 1e-12)
    assertEqualsDouble(result.metrics.laminarity, 0.0, 1e-12)
    assertEqualsDouble(result.metrics.diagonalEntropyNats, 0.0, 1e-12)
    assertEqualsDouble(result.metrics.trappingTime, 0.0, 1e-12)
    assertEquals(result.metrics.longestDiagonal, 4)
  }

  test("a full rectangular matrix matches hand-counted line histograms") {
    val result = Crqa
      .analyse(
        path((1, 1), (1, 1), (1, 1), (1, 1)),
        path((1, 1), (1, 1), (1, 1)),
        fixed(0.0)
      )
      .toOption
      .get

    assert(result.matrix.toRows.flatten.forall(identity))
    assertEqualsDouble(result.metrics.recurrenceRate, 1.0, 1e-12)
    assertEqualsDouble(result.metrics.determinism, 10.0 / 12.0, 1e-12)
    assertEqualsDouble(result.metrics.laminarity, 1.0, 1e-12)
    assertEqualsDouble(result.metrics.diagonalEntropyNats, math.log(2.0), 1e-12)
    assertEqualsDouble(result.metrics.trappingTime, 4.0, 1e-12)
    assertEquals(result.metrics.longestDiagonal, 3)
  }

  test("embedding dimension and delay change the states before distances are measured") {
    val p     = path((0, 0), (1, 0), (0, 0), (1, 0))
    val other = path((0, 0), (0, 0), (1, 0), (1, 0))

    val unembedded = Crqa.analyse(p, p, fixed(0.0)).toOption.get
    val delayOne   = Crqa.analyse(p, p, fixed(0.0, 2, 1)).toOption.get
    val delayTwo   = Crqa.analyse(p, other, fixed(0.0, 2, 2)).toOption.get

    assertEquals((unembedded.matrix.rowCount, unembedded.matrix.columnCount), (4, 4))
    assertEquals((delayOne.matrix.rowCount, delayOne.matrix.columnCount), (3, 3))
    assertEquals((delayTwo.matrix.rowCount, delayTwo.matrix.columnCount), (2, 2))
    assertNotEquals(unembedded.matrix.toRows, delayOne.matrix.toRows)
    assertEquals(
      delayTwo.matrix.toRows,
      Vector(Vector(false, false), Vector(false, false))
    )
  }

  test("a path too short for its requested embedding is a named failure") {
    val result =
      Crqa.analyse(path((0, 0), (1, 0)), path((0, 0), (1, 0)), fixed(1.0, 3, 2))

    assertEquals(
      result,
      Left(CrqaError.TooShort(CrqaOperand.LeftPath, 2, 5, 3, 2))
    )
  }

  test("combined embedding parameters cannot overflow into a partial path") {
    val config = CrqaConfig(
      EmbeddingDimension.of(Int.MaxValue).toOption.get,
      EmbeddingDelay.of(Int.MaxValue).toOption.get,
      RadiusSelection.FixedInclusive(Distance.px(1.0).toOption.get),
      LineLengthMinimum.two,
      LineLengthMinimum.two,
      LaminarAxis.AlongLeft
    )
    val result = Crqa.analyse(path((0, 0)), path((0, 0)), config)

    result match
      case Left(CrqaError.TooShort(CrqaOperand.LeftPath, 1, required, _, _)) =>
        assert(required > Int.MaxValue.toLong)
      case other => fail(s"unexpected result: $other")
  }

  test("target-rate radius selection uses the empirical distance quantile") {
    val rate   = TargetRecurrenceRate.of(0.5).toOption.get
    val config = CrqaConfig(
      EmbeddingDimension.one,
      EmbeddingDelay.one,
      RadiusSelection.TargetAtLeast[Px](rate),
      LineLengthMinimum.two,
      LineLengthMinimum.two,
      LaminarAxis.AlongLeft
    )
    val result = Crqa
      .analyse(path((0, 0), (10, 0)), path((0, 0), (10, 0)), config)
      .toOption
      .get

    assertEqualsDouble(result.selectedRadius.value, 0.0, 1e-12)
    assertEqualsDouble(result.metrics.recurrenceRate, 0.5, 1e-12)
  }

  test("target-rate selection states its inclusive tie behavior") {
    val rate   = TargetRecurrenceRate.of(0.75).toOption.get
    val config = CrqaConfig(
      EmbeddingDimension.one,
      EmbeddingDelay.one,
      RadiusSelection.TargetAtLeast[Px](rate),
      LineLengthMinimum.two,
      LineLengthMinimum.two,
      LaminarAxis.AlongLeft
    )
    val result = Crqa
      .analyse(path((0, 0), (10, 0)), path((0, 0), (10, 0)), config)
      .toOption
      .get

    assertEqualsDouble(result.selectedRadius.value, 10.0, 1e-12)
    assertEqualsDouble(result.metrics.recurrenceRate, 1.0, 1e-12)
  }

  test("the configured laminar axis is wired for rectangular matrices") {
    val left  = path((1, 1), (1, 1), (1, 1), (1, 1))
    val right = path((1, 1), (1, 1))

    val alongLeft =
      Crqa.analyse(left, right, fixed(0.0, lineMinimum = 3)).toOption.get.metrics
    val alongRight = Crqa
      .analyse(
        left,
        right,
        fixed(0.0, lineMinimum = 3, axis = LaminarAxis.AlongRight)
      )
      .toOption
      .get
      .metrics

    assertEqualsDouble(alongLeft.laminarity, 1.0, 1e-12)
    assertEqualsDouble(alongLeft.trappingTime, 4.0, 1e-12)
    assertEqualsDouble(alongRight.laminarity, 0.0, 1e-12)
    assertEqualsDouble(alongRight.trappingTime, 0.0, 1e-12)
  }

  test("joint translation leaves the recurrence evidence unchanged") {
    val left  = Vector((10.0, 10.0), (12.0, 11.0), (15.0, 12.0), (18.0, 14.0))
    val right = Vector((10.0, 11.0), (13.0, 10.0), (15.0, 13.0), (19.0, 14.0))
    val move  = (point: (Double, Double)) => (point._1 + 20.0, point._2 + 30.0)

    val original =
      Crqa.analyse(path(left*), path(right*), fixed(2.0, 2, 1)).toOption.get
    val translated =
      Crqa.analyse(path(left.map(move)*), path(right.map(move)*), fixed(2.0, 2, 1)).toOption.get

    assertEquals(translated.matrix.toRows, original.matrix.toRows)
    assertEquals(translated.metrics, original.metrics)
  }

  test("different frames are rejected through Agreement") {
    val otherFrame = Frame.screen("other-crqa", 100, 100).toOption.get
    val fixation   = Event.Fixation
      .of(
        Interval.of(clock, Instant.millis(0), Instant.millis(10)).toOption.get,
        Pt[Px](0, 0),
        0.0,
        DispersionMethod.RmsRadius,
        1
      )
      .toOption
      .get
    val other  = Scanpath.of(otherFrame, clock, IArray(fixation)).toOption.get
    val result = Crqa.analyse(path((0, 0)), other, fixed(1.0))

    assert(result.isLeft)
    result.left.foreach(error => assert(error.message.contains("different frames")))
  }

  test("recurrence matrix lookup is total") {
    val matrix = Crqa
      .analyse(path((0, 0)), path((0, 0)), fixed(0.0, lineMinimum = 1))
      .toOption
      .get
      .matrix

    assertEquals(matrix.get(0, 0), Some(true))
    assertEquals(matrix.get(-1, 0), None)
    assertEquals(matrix.get(0, 1), None)
  }

end CrqaSuite
