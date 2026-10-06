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

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.artifacts.{NativeArtifactError, NativeBindingFacts}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preview.{PreviewCandidates, PreviewCounts, PreviewId, PreviewReady}

/** Checked metadata fixtures exercise acceptance only; native package bytes
  * and canonical digest provenance are tested in the actual native journeys.
  */
class NativeArtifactsAppSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val requested                    = get(
    History.start(get(StoryMoments.t2)).apply(Command.SaveAndRun(None))
  ).history.document
  private lazy val run   = requested.runs.last
  private lazy val stamp = AppModel.stampOf(requested, run.analysis, run.dataset)
  private val jobId      = JobId(37)
  private def progress: ExecutionProgress =
    val report = StoryModels.run8Progress(44845L).report
    ExecutionProgress(
      get(
        JobProgress.of(jobId, run.id, report.step, report.segment, report.meter, report.totals)
      )
    )
  private def observed(phase: JobPhase): AppModel =
    AppModel
      .update(
        AppModel.open(requested, None),
        Intent.Execution(ExecutionEvent.Changed(ExecutionJob(jobId, run.id, stamp, phase)))
      )
      ._1
  private lazy val facts = get(
    NativeBindingFacts.of(
      run.id,
      stamp.copy(
        plan = CoreBinding.Bound(get(CanonicalDigest.parse[StudyPlanArtifact]("11" * 32))),
        input = CoreBinding.Bound(get(CanonicalDigest.parse[StudyInputArtifact]("22" * 32)))
      ),
      get(SemanticIdentity.of("0123456789abcdef")),
      get(CanonicalDigest.parse[ResultArchiveArtifact]("33" * 32)),
      requested.analyses.last.recipe
    )
  )
  private def stored(model: AppModel, value: NativeBindingFacts = facts, id: JobId = jobId) =
    AppModel.update(model, Intent.Execution(ExecutionEvent.ArtifactsStored(id, value)))

  private def unrelatedEditsPreserveReady(bound: AppModel): Unit =
    val reporting = bound.document.reporting.head
    val renamed   = get(
      ReportingSpec.of(
        reporting.id,
        reporting.name + " verified",
        reporting.groupBy,
        reporting.filters,
        reporting.minimumPerGroup,
        reporting.weighting,
        reporting.contrast
      )
    )
    val theme =
      if bound.document.presentation.theme == Theme.Light then Theme.Dark else Theme.Light
    val perspective = if bound.perspective == Perspective.Compare then Perspective.Figures
    else Perspective.Compare
    val commands = Vector(
      Command.SetTheme(theme),
      Command.PutReporting(renamed),
      Command.SetPerspective(perspective),
      Command.ShowRun(Some(run.id))
    )
    commands.foldLeft(bound) { (model, command) =>
      val (next, effects) = AppModel.update(model, Intent.Dispatch(command))
      assertEquals(next.jobs, bound.jobs)
      assertEquals(next.jobs.ready, bound.jobs.ready)
      assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]), command.name)
      assert(effects.exists(_.isInstanceOf[AppEffect.Journal]), command.name)
      next
    }
    ()

  test(
    "only an observed exact successful job binds completed native artifacts; identical delivery is inert"
  ) {
    val complete = observed(JobPhase.Succeeded(progress))
    val ready    = AppModel
      .update(complete, Intent.Execution(ExecutionEvent.Ready(RunReady(jobId, run.id, stamp))))
      ._1
    val (bound, effects) = stored(ready)
    assertEquals(
      bound.document.run(run.id).map(_.archive),
      Some(CoreBinding.Bound(facts.result))
    )
    assertEquals(
      bound.document.analysis(run.analysis).map(_.plan),
      Some(CoreBinding.Bound(facts.planCanonical))
    )
    assertEquals(
      bound.document.analysis(run.analysis).flatMap(_.recipe.input),
      Some(facts.source)
    )
    assertEquals(bound.document.presentation, ready.document.presentation)
    assertEquals(bound.jobs, ready.jobs)
    assertEquals(bound.selection, ready.selection)
    assert(effects.exists {
      case AppEffect.Journal(
            eyes4s.studio.core.command.JournalEntry.Apply(Command.BindCompletedArtifacts(value))
          ) =>
        value == facts
      case _ => false
    })
    assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]))
    unrelatedEditsPreserveReady(bound)
    val (again, repeated) = stored(bound)
    assertEquals(again, bound)
    assertEquals(repeated, Vector.empty)
  }

  test("unknown jobs and cancelled jobs cannot publish a completed artifact binding") {
    Vector(observed(JobPhase.Cancelled(None)), AppModel.open(requested, None)).foreach {
      model =>
        val (refused, effects) = stored(model)
        assertEquals(refused.document, model.document)
        assertEquals(refused.jobs, model.jobs)
        assert(refused.notice.exists(_.isInstanceOf[Notice.ArtifactsRefused]))
        assertEquals(effects, Vector.empty)
    }
    val complete = observed(JobPhase.Succeeded(progress))
    val wrong    = get(
      NativeBindingFacts.of(
        RunId(99),
        facts.stamp,
        facts.source,
        facts.result,
        facts.recipeSnapshot
      )
    )
    assertEquals(stored(complete, wrong)._1.document, complete.document)
    assertEquals(stored(complete, id = JobId(99))._1.document, complete.document)
  }

  test(
    "a bound preview stamp retains its exact canonical input and Ready identity when files arrive"
  ) {
    val identity = facts.stamp
    val preview  = get(
      PreviewReady.of(
        PreviewId(1),
        identity,
        get(PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, None)),
        get(PreviewCounts.of(8969L, 44845L, 457, 9, 0)),
        Vector.empty,
        Some(facts.recipeSnapshot)
      )
    )
    val initial  = AppModel.open(get(StoryMoments.t2), None)
    val prepared = AppModel
      .update(initial, Intent.DesignPrepared(PreparedDesign(preview, facts.recipeSnapshot)))
      ._1
    val submitted = AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))._1
    val completed = ExecutionJob(jobId, run.id, identity, JobPhase.Succeeded(progress))
    val complete  =
      AppModel.update(submitted, Intent.Execution(ExecutionEvent.Changed(completed)))._1
    val ready = AppModel
      .update(
        complete,
        Intent.Execution(ExecutionEvent.Ready(RunReady(jobId, run.id, identity)))
      )
      ._1
    val (bound, effects) = stored(ready)
    assertEquals(bound.jobs, ready.jobs)
    assertEquals(bound.jobs.shelf.required, Some(identity))
    assertEquals(bound.jobs.ready.map(_.stamp.input), Some(identity.input))
    assertEquals(
      bound.document.run(run.id).map(_.archive),
      Some(CoreBinding.Bound(facts.result))
    )
    assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]))
    unrelatedEditsPreserveReady(bound)
  }

  test(
    "storage refusal reports an exact run value without changing completed science, Ready or shown"
  ) {
    val complete = observed(JobPhase.Succeeded(progress))
    val ready    = AppModel
      .update(complete, Intent.Execution(ExecutionEvent.Ready(RunReady(jobId, run.id, stamp))))
      ._1
    val diagnostic = NativeArtifactError.persistence(run.id, "store", "disk refused").diagnostic
    val (refused, effects) = AppModel.update(
      ready,
      Intent.Execution(ExecutionEvent.ArtifactsRefused(jobId, run.id, diagnostic))
    )
    assertEquals(refused.document, ready.document)
    assertEquals(refused.document.run(run.id).map(_.state), Some(RunLifecycle.Completed))
    assertEquals(refused.jobs, ready.jobs)
    assertEquals(refused.notice, Some(Notice.ArtifactsRefused(jobId, run.id, diagnostic)))
    assertEquals(effects, Vector.empty)
  }

  test(
    "late stored facts bind their own older completed run without replacing the newer request or Ready"
  ) {
    val complete = observed(JobPhase.Succeeded(progress))
    val recipe   = complete.document.analyses.last.recipe
    val changed  = AppModel
      .update(
        complete,
        Intent.Dispatch(
          Command.ChangeRecipe(RecipeChange.Grid(recipe.grid, get(GridSize.of(32, 24))))
        )
      )
      ._1
    val newer       = AppModel.update(changed, Intent.Dispatch(Command.SaveAndRun(None)))._1
    val newRun      = newer.document.runs.last
    val newStamp    = AppModel.stampOf(newer.document, newRun.analysis, newRun.dataset)
    val oldReport   = progress.report
    val newProgress = ExecutionProgress(
      get(
        JobProgress.of(
          JobId(38),
          newRun.id,
          oldReport.step,
          oldReport.segment,
          oldReport.meter,
          oldReport.totals
        )
      )
    )
    val newJob = ExecutionJob(JobId(38), newRun.id, newStamp, JobPhase.Succeeded(newProgress))
    val done   = AppModel.update(newer, Intent.Execution(ExecutionEvent.Changed(newJob)))._1
    val ready  = AppModel
      .update(
        done,
        Intent.Execution(ExecutionEvent.Ready(RunReady(newJob.id, newRun.id, newStamp)))
      )
      ._1
    val (bound, effects) = stored(ready)
    assertEquals(
      bound.document.run(run.id).map(_.archive),
      Some(CoreBinding.Bound(facts.result))
    )
    assertEquals(bound.document.run(newRun.id), ready.document.run(newRun.id))
    assertEquals(bound.jobs, ready.jobs)
    assertEquals(bound.document.presentation, ready.document.presentation)
    assert(!effects.exists(_.isInstanceOf[AppEffect.Execution]))
  }

  test("a conflicting artifact cannot overwrite an existing bound archive") {
    val complete = observed(JobPhase.Succeeded(progress))
    val bound    = stored(complete)._1
    val wrong    = get(
      NativeBindingFacts.of(
        facts.run,
        facts.stamp,
        facts.source,
        get(CanonicalDigest.parse[ResultArchiveArtifact]("44" * 32)),
        facts.recipeSnapshot
      )
    )
    val refused = stored(bound, wrong)._1
    assertEquals(refused.document, bound.document)
    assert(refused.notice.nonEmpty)
  }
