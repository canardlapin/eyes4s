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

import scala.compiletime.testing.typeCheckErrors

class SyncEvidenceSuite extends munit.FunSuite:

  test("exact time rendering is deterministic down to microseconds") {
    assertEquals(Span.micros(34000L).render, "34.0ms")
    assertEquals(Span.micros(120L).render, "0.12ms")
    assertEquals(Span.micros(12L).render, "0.012ms")
    assertEquals(Span.micros(-500L).render, "-0.5ms")
    assertEquals(Instant.micros(-1500L).render, "-1.5ms")
    assertEquals(Span.micros(Long.MinValue).render, "-9223372036854775.808ms")
  }

  private val tracker  = ClockId("tracker")
  private val stimulus = ClockId("stimulus")

  private def mark(id: String, sourceMicros: Long, targetMicros: Long): SyncMark =
    SyncMark
      .of(id, Instant.micros(sourceMicros), Instant.micros(targetMicros))
      .fold(error => fail(error.message), identity)

  test("offset-only fitting averages all common marks and reports exact residuals") {
    val marks = Vector(
      mark("start", 100000L, 350000L),
      mark("middle", 200000L, 450000L),
      mark("end", 300000L, 550000L)
    )
    val evidence = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.OffsetOnly, marks)
      .fold(error => fail(error.message), identity)

    assertEquals(evidence.source, tracker)
    assertEquals(evidence.target, stimulus)
    assertEquals(evidence.offset, Span.millis(250))
    assertEqualsDouble(evidence.scale, 1.0, 0.0)
    assertEquals(evidence.residuals.map(_.error), Vector.fill(3)(Span.zero))
    assertEquals(evidence.rootMeanSquareResidual, SyncErrorMagnitude.zero)
    assertEquals(evidence.maximumAbsoluteResidual, SyncErrorMagnitude.zero)
    assertEquals(evidence.uncertaintyAt(Instant.millis(500)), SyncErrorMagnitude.zero)
    assertEquals(evidence(Instant.millis(100)), Instant.millis(350))
  }

  test("one common mark proves an offset-only map but not an affine map") {
    val only   = Vector(mark("trigger", 1000L, 2600L))
    val offset = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.OffsetOnly, only)
      .fold(error => fail(error.message), identity)

    assertEquals(offset.offset, Span.micros(1600L))
    assertEquals(offset.usedMarks, only)
    assertEquals(offset.rootMeanSquareResidual, SyncErrorMagnitude.zero)
    assert(
      SyncEvidence
        .fromCommonMarks(tracker, stimulus, SyncFitMode.Affine, only)
        .left
        .exists {
          case SyncEvidenceError.TooFewCommonMarks(`tracker`, `stimulus`, 1, 2) => true
          case _                                                                => false
        }
    )
  }

  test("centered affine fitting recovers a hand-computable offset and drift") {
    val marks = Vector(
      mark("m0", 0L, 5000000L),
      mark("m1", 1000000L, 6000200L),
      mark("m2", 2000000L, 7000400L),
      mark("m3", 3000000L, 8000600L)
    )
    val evidence = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.Affine, marks)
      .fold(error => fail(error.message), identity)

    assertEquals(evidence.offset, Span.seconds(5.0))
    assertEqualsDouble(evidence.scale, 1.0002, 1e-15)
    assertEqualsDouble(evidence.sync.drift, 0.0002, 1e-15)
    assertEquals(evidence(Instant.micros(4000000L)), Instant.micros(9000800L))
    assertEquals(evidence.residuals.map(_.error), Vector.fill(4)(Span.zero))
  }

  test("centered affine fitting remains accurate far from the source epoch") {
    val origin = 1000000000000L
    val marks  = Vector.tabulate(5) { index =>
      val source = origin + index.toLong * 2000000L
      val target = math.round(source.toDouble * 1.000001) + 7000000L
      mark(s"large-$index", source, target)
    }
    val evidence = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.Affine, marks)
      .fold(error => fail(error.message), identity)

    assertEqualsDouble(evidence.scale, 1.000001, 1e-12)
    assert(math.abs(evidence.offset.toMicros - 7000000L) <= 1L, clue(evidence.offset))
    assert(evidence.maximumAbsoluteResidual.toMicros <= 1L)
  }

  test("residual diagnostics retain signed errors and explicit units") {
    val marks = Vector(
      mark("a", 0L, 1000L),
      mark("b", 10000L, 11020L),
      mark("c", 20000L, 20990L)
    )
    val evidence = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.OffsetOnly, marks)
      .fold(error => fail(error.message), identity)

    assertEquals(evidence.offset, Span.micros(1003L))
    assertEquals(evidence.residuals.map(_.error.toMicros), Vector(-3L, 17L, -13L))
    assertEquals(evidence.maximumAbsoluteResidual.span, Span.micros(17L))
    assertEquals(evidence.rootMeanSquareResidual.span, Span.micros(12L))
    assert(evidence.render.contains("rms=0.012ms"))
    assertEquals(
      SyncErrorMagnitude.of(Span.micros(-1L)),
      Left(SyncEvidenceError.NegativeErrorMagnitude(Span.micros(-1L)))
    )
  }

  test("one-pass residual rejection records the excluded named mark and refits") {
    val marks = Vector(
      mark("a", 0L, 1000L),
      mark("b", 10000L, 11000L),
      mark("bad", 20000L, 26000L),
      mark("c", 30000L, 31000L),
      mark("d", 40000L, 41000L)
    )
    val limit    = SyncResidualLimit.of(Span.millis(3)).toOption.get
    val evidence = SyncEvidence
      .fromCommonMarks(tracker, stimulus, SyncFitMode.OffsetOnly, marks, Some(limit))
      .fold(error => fail(error.message), identity)

    assertEquals(evidence.usedMarks.map(_.id), Vector("a", "b", "c", "d"))
    assertEquals(evidence.rejectedMarks.map(_.mark.id), Vector("bad"))
    assertEquals(evidence.rejectedMarks.head.residual.span, Span.millis(4))
    assertEquals(evidence.offset, Span.millis(1))
    assertEquals(evidence.maximumAbsoluteResidual, SyncErrorMagnitude.zero)
  }

  test("mark, residual-limit, cardinality, identity, and order failures are values") {
    assertEquals(
      SyncMark.of("", Instant.epoch, Instant.epoch),
      Left(SyncEvidenceError.EmptyMarkId(""))
    )
    assertEquals(
      SyncResidualLimit.of(Span.micros(-1L)),
      Left(SyncEvidenceError.NegativeResidualLimit(Span.micros(-1L)))
    )
    assert(
      SyncEvidence
        .fromCommonMarks(
          tracker,
          stimulus,
          SyncFitMode.Affine,
          Vector(mark("only", 0L, 1L))
        )
        .left
        .exists {
          case SyncEvidenceError.TooFewCommonMarks(`tracker`, `stimulus`, 1, 2) => true
          case _                                                                => false
        }
    )
    assert(
      SyncEvidence
        .fromCommonMarks(
          tracker,
          stimulus,
          SyncFitMode.OffsetOnly,
          Vector(mark("same", 0L, 1L), mark("same", 2L, 3L))
        )
        .left
        .exists {
          case SyncEvidenceError.DuplicateMarkId(`tracker`, `stimulus`, "same", 0, 1) => true
          case _                                                                      => false
        }
    )
    assert(
      SyncEvidence
        .fromCommonMarks(
          tracker,
          stimulus,
          SyncFitMode.Affine,
          Vector(mark("a", 2L, 1L), mark("b", 1L, 2L))
        )
        .left
        .exists {
          case SyncEvidenceError.NonIncreasingSourceMarks(
                `tracker`,
                `stimulus`,
                1,
                previous,
                current
              ) =>
            previous.toMicros == 2L && current.toMicros == 1L
          case _ => false
        }
    )
  }

  test("rejection cannot make a nominal fit look valid with too little evidence") {
    val marks = Vector(
      mark("a", 0L, 0L),
      mark("b", 1000L, 100000L),
      mark("c", 2000L, 100001L)
    )
    val zero   = SyncResidualLimit.of(Span.zero).toOption.get
    val result = SyncEvidence.fromCommonMarks(
      tracker,
      stimulus,
      SyncFitMode.Affine,
      marks,
      Some(zero)
    )

    assert(
      result.left.exists {
        case SyncEvidenceError.TooFewRetainedMarks(
              `tracker`,
              `stimulus`,
              3,
              retained,
              2
            ) =>
          retained < 2
        case _ => false
      }
    )
  }

  test("evidence and common marks cannot bypass their constructors") {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      val mark = new SyncMark("", Instant.epoch, Instant.epoch)
      val residual = SyncResidual(mark, Instant.epoch, Span.zero)
      val rejected = RejectedSyncMark(mark, Span.zero, SyncResidualLimit.of(Span.zero).toOption.get)
      val evidence = new SyncEvidence(
        SyncFitMode.OffsetOnly,
        Sync.identity(ClockId("x")),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Span.zero,
        Span.zero,
        Span.zero
      )
    """)
    assert(errors.nonEmpty)
  }

end SyncEvidenceSuite
