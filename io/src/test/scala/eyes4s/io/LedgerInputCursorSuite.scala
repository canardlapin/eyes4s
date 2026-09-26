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

package eyes4s.io

import eyes4s.core.*
import eyes4s.design.{SampleQuantum, Trial, Trials}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class LedgerInputCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("frame", 100, 100))
  private val clock                             = ClockId("clock")
  private val key                               = StudyKey("p", "i", "f")
  private val spec                              = get(
    ImportSpec.of(
      SourceKeyColumns.Study("p", "item", "phase"),
      get(
        SourceFixationColumns
          .of("n", "x", "y", "onset", "duration", SampleCountRule.PositiveColumn("samples"))
      ),
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private def fixation(index: Int, on: ClockId) = get(
    Event.Fixation.withoutDispersion(
      get(
        Interval.of(
          on,
          Instant.micros(index.toLong * 1000),
          Instant.micros(index.toLong * 1000 + 500)
        )
      ),
      Pt[Px](10, 20),
      1
    )
  )
  private def input(
      count: Int,
      where: Frame[Px] = frame,
      on: ClockId = clock,
      k: StudyKey = key
  ) =
    StudyInput(
      Trials(
        Vector(
          Trial(
            k,
            (),
            get(Scanpath.of(where, on, IArray.tabulate(count)(i => fixation(i, on))))
          )
        )
      )
    )
  private def drain(
      initial: LedgerInputCursor[StudyKey, Px],
      budget: Int
  ): Either[LedgerResourceError, (Long, Long, Long)] =
    val quantum = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(
        cursor: LedgerInputCursor[StudyKey, Px],
        total: Long
    ): Either[LedgerResourceError, (Long, Long, Long)] =
      cursor.advance(quantum) match
        case Left(error) => Left(error)
        case Right(step) =>
          assert(step.workUnits > 0 && step.workUnits <= budget)
          step.next match
            case Some(next) => loop(next, total + step.workUnits)
            case None => Right((step.retainedUnits, step.fixationCount, total + step.workUnits))
    loop(initial, 0)
  private def cursor(
      value: StudyInput[StudyKey, Px],
      envelope: LedgerExecutionLimits = limits()
  ) =
    get(LedgerInputCursor.start(spec, value, envelope))
  private def subtree(
      value: LedgerInputVisit[StudyKey, Px],
      envelope: LedgerExecutionLimits = limits()
  ) =
    LedgerInputCursor(envelope, LedgerEvidenceKey.Study, List(value), 0L, 0L)

  test("one large trial yields at every fixation and has deterministic work across budgets") {
    val value    = input(4000)
    val initial  = cursor(value)
    val expected = get(drain(initial, 1))
    assertEquals(expected._2, 4000L)
    assert(expected._3 >= 8000L)
    Vector(2, 31, 1024).foreach(b => assertEquals(drain(initial, b), Right(expected)))
    val q = get(SampleQuantum.of(1))
    assertEquals(initial.advance(q), initial.advance(q))
    assertEquals(initial.fixations, 0L)
    assert(drain(cursor(value, limits(LedgerResource.LogicalRecords -> 4000L)), 17).isRight)
    assert(drain(cursor(value, limits(LedgerResource.LogicalRecords -> 3999L)), 17).isLeft)
    assert(
      drain(
        cursor(value, limits(LedgerResource.RetainedEvidenceUnits -> expected._1)),
        7
      ).isRight
    )
    assert(
      drain(
        cursor(value, limits(LedgerResource.RetainedEvidenceUnits -> (expected._1 - 1))),
        7
      ).isLeft
    )
  }

  test("keys, frame identities and clocks are sized before any equality or rendering") {
    val huge   = "q" * 20000
    val values = Vector(
      input(1, where = frame.withId(FrameId(huge))),
      input(1, on = ClockId(huge)),
      input(1, k = StudyKey(huge, "i", "f")),
      input(1, k = StudyKey("p", huge, "f")),
      input(1, k = StudyKey("p", "i", huge))
    )
    values.foreach { value =>
      val initial = cursor(value, limits(LedgerResource.FieldCodeUnits -> 100L))
      Vector(1, 3, 1024).foreach { b =>
        assertEquals(
          drain(initial, b),
          Left(
            LedgerResourceError.Exceeded(
              LedgerResource.FieldCodeUnits,
              100,
              BigInt(20000),
              LedgerResourceLocation(LedgerResourceSource.ExpectedInput)
            )
          )
        )
      }
    }
  }

  test("nested transform evidence uses an explicit frontier, not recursive equality") {
    val depth = 20000
    val tree = (0 until depth).foldLeft[SpatialTransform](SpatialTransform.Identity)((acc, _) =>
      SpatialTransform.Then(acc, SpatialTransform.Identity)
    )
    val initial  = subtree(LedgerInputVisit.Transform(tree))
    val expected = Right((2L * depth + 1, 0L, 2L * depth + 1))
    Vector(1, 17, 1024).foreach(b => assertEquals(drain(initial, b), expected))
    val stopped = subtree(
      LedgerInputVisit.Transform(tree),
      limits(LedgerResource.RetainedEvidenceUnits -> 100L)
    )
    assertEquals(
      drain(stopped, 7),
      Left(
        LedgerResourceError.Exceeded(
          LedgerResource.RetainedEvidenceUnits,
          100,
          BigInt(101),
          LedgerResourceLocation(LedgerResourceSource.ExpectedInput)
        )
      )
    )
  }

  test("source-supported and recomputed paths visit recording and summary identities") {
    val spread = get(
      Event.Fixation.of(
        get(Interval.of(clock, Instant.millis(0), Instant.millis(100))),
        Pt[Px](10, 20),
        2,
        DispersionMethod.RmsRadius,
        2
      )
    )
    val recording = get(
      Recording.of(
        frame,
        clock,
        Rate.Fixed(get(Hz(20))),
        Eye.Left,
        None,
        IArray(
          Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](8, 20), None)),
          Sample(Instant.millis(50), Gaze.Tracked(Pt[Px](12, 20), None))
        )
      )
    )
    def supported(label: String) = get(
      Scanpath.fromEvents(
        get(
          EventSeries.of(
            recording,
            RecordingRef(label),
            Vector(spread),
            Vector(get(SampleRange.of(0, 2)))
          )
        )
      )
    )
    Vector(supported("source"), get(supported("source").warp(Warp.id(frame)))).foreach { path =>
      val value    = StudyInput(Trials(Vector(Trial(key, (), path))))
      val initial  = cursor(value)
      val expected = drain(initial, 1)
      assert(expected.isRight)
      assertEquals(drain(initial, 31), expected)
    }
    val large = StudyInput(Trials(Vector(Trial(key, (), supported("s" * 20000)))))
    assert(drain(cursor(large, limits(LedgerResource.FieldCodeUnits -> 100L)), 1).isLeft)
  }

  test("all recomputed summary metadata is checked before walking transforms") {
    val huge  = "q" * 20000
    val range = get(SampleRange.of(0, 1))
    for i <- 0 until 3 do
      val names    = Vector("source", "from", "to").updated(i, huge)
      val evidence = SummaryEvidence.Recomputed(
        RecordingRef(names(0)),
        range,
        FrameId(names(1)),
        FrameId(names(2)),
        SpatialTransform.Identity
      )
      assert(
        drain(
          subtree(
            LedgerInputVisit.Summary(evidence),
            limits(LedgerResource.FieldCodeUnits -> 100L)
          ),
          1
        ).isLeft
      )
  }

  test(
    "sample lineage and segmentation are accounted without materializing derived recording data"
  ) {
    val lineage = (0 until 20000).foldLeft(SampleLineage.measured)((a, _) => a.projected)
    val sample  = Sample(Instant.micros(0), Gaze.Tracked(Pt[Px](0, 0), None), lineage)
    val initial = subtree(LedgerInputVisit.Samples(IArray(sample), 0))
    assertEquals(drain(initial, 1), Right((20002L, 0L, 1L)))
    val ranges = Vector.fill(20000)(get(SampleRange.of(0, 1)))
    assertEquals(drain(subtree(LedgerInputVisit.Support(ranges)), 1), Right((20001L, 0L, 1L)))
    assert(
      drain(
        subtree(
          LedgerInputVisit.Samples(IArray(sample), 0),
          limits(LedgerResource.RetainedEvidenceUnits -> 20001L)
        ),
        1
      ).isLeft
    )
  }
