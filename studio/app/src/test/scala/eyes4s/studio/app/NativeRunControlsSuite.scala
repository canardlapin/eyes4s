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

import eyes4s.studio.core.backend.{AnalysisRevision, JobId, ReportRole, Response}
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.document.RunLifecycle
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionEvent, ExecutionJob, JobPhase}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.selection.{QueryCount, ReportGroup, ScaleIndex, StudioRef}

class NativeRunControlsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val requested                    = get(
    History.start(get(StoryMoments.t2)).apply(Command.SaveAndRun(None))
  ).history.document
  private lazy val run   = requested.running.last
  private lazy val stamp = AppModel.stampOf(requested, run.analysis, run.dataset)
  private val job        = JobId(37)
  private def observe(
      phase: JobPhase,
      identity: eyes4s.studio.core.execution.RunStamp = stamp
  ): AppModel =
    AppModel
      .update(
        AppModel.open(requested, None),
        Intent.Execution(ExecutionEvent.Changed(ExecutionJob(job, run.id, identity, phase)))
      )
      ._1

  test("a newly requested live run cancels its observed exact job without a stored JobHandle") {
    assertEquals(requested.job(run.id), None)
    val queued           = observe(JobPhase.Queued)
    val (asked, effects) = AppModel.update(queued, Intent.CancelJob(job))
    assertEquals(asked, queued)
    assertEquals(effects, Vector(AppEffect.Execution(ExecutionEffect.Cancel(job))))
    val settled = AppModel
      .update(
        asked,
        Intent.Execution(
          ExecutionEvent.Changed(ExecutionJob(job, run.id, stamp, JobPhase.Cancelled(None)))
        )
      )
      ._1
    assertEquals(settled.document.run(run.id).map(_.state), Some(RunLifecycle.Cancelled(None)))
    assertEquals(settled.document.presentation.shownRun, requested.presentation.shownRun)
    assertEquals(settled.jobs.ready, None)
    assertEquals(AppModel.update(settled, Intent.CancelJob(job))._2, Vector.empty)
  }

  test("cancellation refuses an observed job whose identity disagrees with its document run") {
    val mismatched         = stamp.copy(revision = AnalysisRevision(99))
    val (refused, effects) =
      AppModel.update(observe(JobPhase.Queued, mismatched), Intent.CancelJob(job))
    assertEquals(effects, Vector.empty)
    assert(refused.notice.exists {
      case Notice.ExecutionRefused(error) => error.code == "studio-execution.stamp-mismatch"
      case _                              => false
    })
  }

  test("explicit Show removes report and query-count selections belonging to the old run") {
    val reporting = get(StoryMoments.byResponseId)
    val scale     = ScaleIndex.first
    val old       = Vector(
      StudioRef.ReportCell(
        StoryMoments.run7,
        reporting,
        scale,
        ReportGroup.Whole,
        ReportRole.Difference
      ),
      StudioRef.ReportParticipant(
        StoryMoments.run7,
        reporting,
        scale,
        ReportGroup.Whole,
        ReportRole.Difference,
        "P17"
      ),
      StudioRef.ReportContrast(
        StoryMoments.run7,
        reporting,
        scale,
        ReportRole.Difference,
        Response.Remembered,
        Response.Forgotten
      ),
      StudioRef.ReportQueryRange(StoryMoments.run7, reporting, scale, ReportRole.Difference),
      StudioRef.QueryTally(StoryMoments.run7, QueryCount.Requested)
    )
    old.foreach(ref => assertEquals(AppModel.runOf(ref), Some(StoryMoments.run7)))
    val base     = StoryModels.t3Summary
    val stable   = StudioRef.Participant("P17")
    val selected =
      AppModel.update(base, StoryModels.select(base, "compare.summary", (stable +: old)*))._1
    val completed = ExecutionJob(
      StoryMoments.run8Job,
      StoryMoments.run8,
      StoryModels.run8Stamp,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    val done = AppModel.update(selected, Intent.Execution(ExecutionEvent.Changed(completed)))._1
    val ready = AppModel
      .update(
        done,
        Intent.Execution(
          ExecutionEvent.Ready(
            eyes4s.studio.core.execution.RunReady(completed.id, completed.run, completed.stamp)
          )
        )
      )
      ._1
    val shown = AppModel.update(ready, Intent.ShowRun(StoryMoments.run8))._1
    assertEquals(shown.selection.selected, Vector(stable))
    assertEquals(shown.selection.context, selected.selection.context.next)
  }

  test(
    "newest completion retains its request and ready notice while an older run is still pending"
  ) {
    val original = StoryModels.t3Summary.document
    val revision = original.analyses.last.copy(id = AnalysisRevision(6))
    val newest   = eyes4s.studio.core.document.RunRef(
      eyes4s.studio.core.backend.RunId(9),
      revision.id,
      revision.dataset,
      RunLifecycle.Running,
      eyes4s.studio.core.document.CoreBinding.unbound
    )
    val document = get(
      eyes4s.studio.core.document.StudioDocument.of(
        original.datasets,
        original.analyses :+ revision,
        original.draft,
        original.runs :+ newest,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )
    val identity = AppModel.stampOf(document, newest.analysis, newest.dataset)
    val report   = StoryModels.run8Progress(44845L).report
    val progress = eyes4s.studio.core.execution.ExecutionProgress(
      get(
        eyes4s.studio.core.backend.JobProgress
          .of(job, newest.id, report.step, report.segment, report.meter, report.totals)
      )
    )
    val completed = ExecutionJob(job, newest.id, identity, JobPhase.Succeeded(progress))
    val (finished, effects) = AppModel.update(
      AppModel.open(document, None),
      Intent.Execution(ExecutionEvent.Changed(completed))
    )
    def requires(effects: Vector[AppEffect]): Boolean = effects.exists {
      case AppEffect.Execution(ExecutionEffect.Require(_)) => true
      case _                                               => false
    }
    assertEquals(finished.document.running.map(_.id), Vector(StoryMoments.run8))
    assertEquals(AppModel.requestedStamp(finished.document), Some(identity))
    assertEquals(requires(effects), false)
    val ready = AppModel
      .update(
        finished,
        Intent.Execution(
          ExecutionEvent.Ready(eyes4s.studio.core.execution.RunReady(job, newest.id, identity))
        )
      )
      ._1
    assertEquals(ready.jobs.ready.map(_.run), Some(newest.id))
    val older = ExecutionJob(
      StoryMoments.run8Job,
      StoryMoments.run8,
      StoryModels.run8Stamp,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    val (settled, oldEffects) =
      AppModel.update(ready, Intent.Execution(ExecutionEvent.Changed(older)))
    assertEquals(requires(oldEffects), false)
    assertEquals(settled.jobs.ready, ready.jobs.ready)
    assertEquals(settled.document.presentation.shownRun, original.presentation.shownRun)
    assertEquals(settled.selection, ready.selection)
  }

  private def nativeRequest(): (AppModel, eyes4s.studio.core.execution.RunStamp) =
    val base     = StoryModels.t2Analysis
    val recipe   = base.document.draftRecipe.getOrElse(fail("no fixture draft"))
    val identity = AppModel
      .stampOf(base.document, StoryMoments.rev5, StoryMoments.r3)
      .copy(
        plan = eyes4s.studio.core.document.CoreBinding.Bound(
          get(
            eyes4s.codec.CanonicalDigest
              .parse[eyes4s.studio.core.document.StudyPlanArtifact]("1" * 64)
          )
        ),
        input = eyes4s.studio.core.document.CoreBinding.Bound(
          get(
            eyes4s.codec.CanonicalDigest
              .parse[eyes4s.studio.core.execution.StudyInputArtifact]("2" * 64)
          )
        )
      )
    val receipt = get(
      eyes4s.studio.core.preview.PreviewReady.of(
        eyes4s.studio.core.preview.PreviewId(71),
        identity,
        get(
          eyes4s.studio.core.preview.PreviewCandidates.of(466, 471, 24, 219486L, 480, 14, None)
        ),
        get(eyes4s.studio.core.preview.PreviewCounts.of(8969L, 44845L, 457, 9, 0)),
        Vector.empty,
        Some(recipe)
      )
    )
    val prepared =
      AppModel.update(base, Intent.DesignPrepared(PreparedDesign(receipt, recipe)))._1
    val (requested, effects) =
      AppModel.update(prepared, Intent.Dispatch(Command.SaveAndRun(None)))
    assert(
      effects.contains(
        AppEffect.Execution(ExecutionEffect.SubmitPreview(receipt, Some(StoryMoments.run8)))
      )
    )
    assertEquals(requested.jobs.shelf.required, Some(identity))
    (requested, identity)

  test(
    "binding the same plan preserves the exact native input request before and after Ready"
  ) {
    Vector(false, true).foreach { alreadyReady =>
      val (requested, identity) = nativeRequest()
      val completed             = ExecutionJob(
        StoryMoments.run8Job,
        StoryMoments.run8,
        identity,
        JobPhase.Succeeded(StoryModels.run8Progress(44845L))
      )
      val notice  = eyes4s.studio.core.execution.RunReady(completed.id, completed.run, identity)
      val initial = if alreadyReady then
        val finished =
          AppModel.update(requested, Intent.Execution(ExecutionEvent.Changed(completed)))._1
        AppModel.update(finished, Intent.Execution(ExecutionEvent.Ready(notice)))._1
      else
        AppModel
          .update(
            requested,
            Intent.Execution(
              ExecutionEvent.Changed(
                completed.copy(phase = JobPhase.Running(StoryModels.run8Progress(10L)))
              )
            )
          )
          ._1
      val plan = identity.plan match
        case eyes4s.studio.core.document.CoreBinding.Bound(value) => value
        case _                                                    => fail("no native plan")
      val (bound, effects) = AppModel.update(
        initial,
        Intent.Dispatch(
          Command.BindPlan(
            identity.revision,
            plan,
            get(eyes4s.studio.core.document.SemanticIdentity.of("00112233445566ff"))
          )
        )
      )
      assert(!effects.exists {
        case AppEffect.Execution(ExecutionEffect.Require(_)) => true
        case _                                               => false
      })
      assertEquals(bound.jobs.shelf.required, Some(identity))
      assertEquals(bound.jobs.ready, initial.jobs.ready)
      val finished =
        AppModel.update(bound, Intent.Execution(ExecutionEvent.Changed(completed)))._1
      val ready = AppModel.update(finished, Intent.Execution(ExecutionEvent.Ready(notice)))._1
      assertEquals(ready.jobs.ready, Some(notice))
      assertEquals(ready.jobs.shelf.required, Some(identity))
      assertEquals(ready.document.presentation.shownRun, initial.document.presentation.shownRun)
    }
  }

  test("binding a contradictory plan withdraws a native Ready request") {
    val (requested, identity) = nativeRequest()
    val completed             = ExecutionJob(
      StoryMoments.run8Job,
      StoryMoments.run8,
      identity,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    val notice   = eyes4s.studio.core.execution.RunReady(completed.id, completed.run, identity)
    val finished =
      AppModel.update(requested, Intent.Execution(ExecutionEvent.Changed(completed)))._1
    val ready = AppModel.update(finished, Intent.Execution(ExecutionEvent.Ready(notice)))._1
    val other = get(
      eyes4s.codec.CanonicalDigest
        .parse[eyes4s.studio.core.document.StudyPlanArtifact]("3" * 64)
    )
    val (changed, effects) = AppModel.update(
      ready,
      Intent.Dispatch(
        Command.BindPlan(
          identity.revision,
          other,
          get(eyes4s.studio.core.document.SemanticIdentity.of("00112233445566ff"))
        )
      )
    )
    val declared = AppModel.stampOf(changed.document, identity.revision, identity.dataset)
    assert(effects.contains(AppEffect.Execution(ExecutionEffect.Require(declared))))
    assertEquals(changed.jobs.shelf.required, Some(declared))
    assertEquals(changed.jobs.ready, None)
    assertEquals(changed.document.presentation.shownRun, ready.document.presentation.shownRun)
  }
