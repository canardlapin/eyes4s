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

package eyes4s.studio.app

import eyes4s.studio.core.backend.{JobId, RunId}
import eyes4s.studio.core.execution.{ExecutionEvent, ExecutionJob, JobPhase, RunReady}
import eyes4s.studio.core.fixture.StoryMoments.*
import eyes4s.studio.core.selection.StudioRef

class RunInProgressSuite extends munit.FunSuite:
  private def step(m: AppModel, i: Intent): AppModel = AppModel.update(m, i)._1
  private val stable                                 =
    Vector(StudioRef.Participant("P17"), StudioRef.Trial(StoryModels.p17ret07))

  private def completed(m: AppModel): AppModel =
    val job = ExecutionJob(
      run8Job,
      run8,
      StoryModels.run8Stamp,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    step(
      step(m, Intent.Execution(ExecutionEvent.Changed(job))),
      Intent.Execution(ExecutionEvent.Ready(RunReady(run8Job, run8, job.stamp)))
    )

  test("E2E-08 completion keeps the shown run and selection until explicit Show") {
    val initial  = StoryModels.t3Summary
    val selected = step(
      initial,
      StoryModels.select(initial, "compare.summary", (stable :+ StoryModels.pair)*)
    )
    val premature = step(selected, Intent.ShowRun(run8))
    assertEquals(premature.document.presentation.shownRun, Some(run7))
    val ready = completed(selected)
    assertEquals(ready.document.presentation.shownRun, Some(run7))
    assertEquals(ready.selection, selected.selection)
    assertEquals(ready.jobs.ready.map(_.run), Some(run8))
    val shown = step(ready, Intent.ShowRun(run8))
    assertEquals(shown.document.presentation.shownRun, Some(run8))
    assertEquals(shown.selection.selected, stable)
    assertEquals(shown.selection.context, selected.selection.context.next)
    assertEquals(shown.jobs.ready, None)
  }

  test("late ready events for the shown run or an older run cannot restore a notice") {
    val shown = step(completed(StoryModels.t3Summary), Intent.ShowRun(run8))
    Vector(run7, run8).foreach { run =>
      val late = step(
        shown,
        Intent.Execution(ExecutionEvent.Ready(RunReady(JobId(10), run, StoryModels.run8Stamp)))
      )
      assertEquals(late.jobs.ready, None)
      assertEquals(late.document.presentation.shownRun, Some(run8))
    }
    val newer = step(
      shown,
      Intent.Execution(
        ExecutionEvent.Ready(RunReady(JobId(11), RunId(9), StoryModels.run8Stamp))
      )
    )
    assertEquals(newer.jobs.ready.map(_.run), Some(RunId(9)))
  }

  test("cancelled and failed runs retain the shown run without a Show offer") {
    Vector(
      JobPhase.Cancelled(None),
      JobPhase.Failed(
        Vector(eyes4s.studio.core.execution.ExecutionDiagnostics.unexplained(run8Job)),
        None
      )
    ).foreach { phase =>
      val initial = StoryModels.t3Summary
      val next    = step(
        initial,
        Intent.Execution(
          ExecutionEvent.Changed(ExecutionJob(run8Job, run8, StoryModels.run8Stamp, phase))
        )
      )
      assertEquals(next.document.presentation.shownRun, Some(run7))
      assertEquals(next.jobs.ready, None)
    }
  }
