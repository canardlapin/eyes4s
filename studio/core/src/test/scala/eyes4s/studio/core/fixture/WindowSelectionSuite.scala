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

package eyes4s.studio.core.fixture

import eyes4s.kernel.Span
import eyes4s.plan.{MapPlacement, OffWindowPolicy, WindowTally}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

class WindowSelectionSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val trial                            = MockStudy.key("P17", "ret_07")
  private val expected                         = ok(
    WindowTally.of(0, 1, 2, Span.zero, Span.micros(60000), Span.micros(100000))
  )
  private def fixation(index: Int, duration: Double, placement: MapPlacement) =
    ok(
      AdmittedFixation.of(
        StudioRef.Fixation(trial, ok(FixationIndex.of(index))),
        index,
        500.0,
        300.0,
        index * 1000.0,
        duration,
        placement
      )
    )

  test("a dropped initial fixation cannot dilute the reporting window share") {
    val fixations = Vector(
      fixation(1, 900.0, MapPlacement.DroppedInitial),
      fixation(2, 60.0, MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial)),
      fixation(3, 40.0, MapPlacement.TrialFailed(expected))
    )
    val view  = ok(TrialFixations.of(AnalysisRevision(4), DatasetRevision(3), trial, fixations))
    val tally = FakeTrialViews.tally(view.fixations).getOrElse(fail("no tally"))
    assertEquals(view.fixations.size, 3)
    assertEquals(tally, expected)
    assertEquals(tally.outsideWindowShare, Some(0.6))
    assert(tally.outsideWindowShare.exists(_ > 0.1))
    assertEquals(view.fixations.last.placement, MapPlacement.TrialFailed(tally))
  }

  test("an all-dropped admitted trial has a zero tally rather than missing evidence") {
    assertEquals(
      FakeTrialViews.tally(Vector(fixation(1, 900.0, MapPlacement.DroppedInitial))),
      Some(ok(WindowTally.of(0, 0, 0, Span.zero, Span.zero, Span.zero)))
    )
  }
