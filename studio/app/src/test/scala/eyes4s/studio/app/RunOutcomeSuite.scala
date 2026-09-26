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

import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticOrigin,
  StageKind,
  StudioDiagnostic
}
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{CoreBinding, RunLifecycle}
import eyes4s.studio.core.execution.{ExecutionEvent, ExecutionJob, JobPhase}
import eyes4s.studio.core.fixture.StoryMoments

/** The update records a settled job's run outcome itself (bead
  * bd-01M3FHC9HTKP2SMQTP1HMAHWGK): every shell gets the same rule.
  */
class RunOutcomeSuite extends munit.FunSuite:
  import StoryModels.*

  private val run8     = StoryMoments.run8
  private val progress = run8Progress(21400L)

  private def job(phase: JobPhase): ExecutionJob = run8Job(phase)

  private def after(phase: JobPhase): (AppModel, Vector[AppEffect]) =
    AppModel.update(t3Summary, Intent.Execution(ExecutionEvent.Changed(job(phase))))

  private val diagnostic =
    StudioDiagnostic(
      "study-failure.off-window",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector.empty,
      ""
    )

  test("a terminal phase records the run's lifecycle, journalled and persisted") {
    val cases = Vector(
      JobPhase.Succeeded(progress)                        -> RunLifecycle.Completed,
      JobPhase.Failed(Vector(diagnostic), Some(progress)) -> RunLifecycle.Failed,
      JobPhase.Cancelled(Some(progress)) -> RunLifecycle.Cancelled(Some(StageKind.Comparing)),
      JobPhase.Cancelled(None)           -> RunLifecycle.Cancelled(None),
      JobPhase.Superseded(None, Some(progress)) -> RunLifecycle.Completed,
      JobPhase.Superseded(None, None)           -> RunLifecycle.Cancelled(None)
    )
    cases.foreach { (phase, lifecycle) =>
      val (m, effects) = after(phase)
      assertEquals(m.document.run(run8).map(_.state), Some(lifecycle), phase)
      val recorded = Command.RecordRunOutcome(run8, lifecycle, CoreBinding.unbound)
      assertEquals(
        effects.headOption,
        Some(AppEffect.Journal(JournalEntry.Apply(recorded))),
        phase
      )
      assert(effects.contains(AppEffect.Persist), phase)
      assertEquals(m.notice, None, phase)
      // The job board saw the event too.
      assertEquals(m.jobs.job(StoryMoments.run8Job).map(_.phase), Some(phase), phase)
    }
  }

  test("a live phase records nothing") {
    Vector(JobPhase.Queued, JobPhase.Running(progress), JobPhase.Cancelling(Some(progress)))
      .foreach { phase =>
        val (m, effects) = after(phase)
        assertEquals(m.document, t3Summary.document, phase)
        assertEquals(effects, Vector.empty, phase)
      }
  }

  test("an outcome is recorded only while the run is running") {
    val (once, _)     = after(JobPhase.Cancelled(None))
    val event         = ExecutionEvent.Changed(job(JobPhase.Succeeded(progress)))
    val (twice, more) = AppModel.update(once, Intent.Execution(event))
    assertEquals(twice.document, once.document)
    assertEquals(more, Vector.empty)
    assertEquals(AppModel.outcomeOf(once.document, event), None)
    // A job whose run is not running (run 6 was cancelled) records nothing.
    val stranger =
      ExecutionEvent.Changed(job(JobPhase.Succeeded(progress)).copy(run = StoryMoments.run6))
    assertEquals(AppModel.outcomeOf(t3Summary.document, stranger), None)
  }
