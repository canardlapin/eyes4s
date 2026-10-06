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

import eyes4s.studio.app.keys.{CommandRegistry, Key, KeyChord, Modifier}
import eyes4s.studio.app.layout.{PaneId, StudioLayouts}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.execution.*
import eyes4s.studio.core.command.{Command, CommandGen, HistoryStack}
import eyes4s.studio.core.document.{DocumentGen, Perspective, Preset, SourceRole}
import eyes4s.studio.core.selection.*
import org.scalacheck.Gen

/** Generated app models and intent sequences over them: every intent case,
  * aimed mostly at what the model holds, plus stale and refused inputs.
  */
object AppGen:
  import StoryModels.*

  val start: Gen[AppModel] =
    Gen
      .frequency(
        3 -> DocumentGen.document,
        1 -> Gen.oneOf(t1, t2, t3, empty)
      )
      .flatMap(d => Gen.option(Gen.const(project)).map(AppModel.open(d, _)))

  private val views: Vector[ViewId] =
    Vector("compare.contrast", "explore.trial-view", "figures.page").map(v => ok(ViewId.of(v)))

  private def refs(m: AppModel): Vector[StudioRef] =
    Vector(query, pair, record, panelD, p17Summary, StudioRef.Participant("P17")) ++
      m.document.runs.map(r => StudioRef.QueryContrast(r.id, sigma2, p17ret07))

  private def select(m: AppModel): Gen[Intent] =
    for
      view  <- Gen.oneOf(views)
      stale <- Gen.frequency(5 -> false, 1 -> true)
      delta <- Gen.oneOf(-1L, 0L, 1L, 1L, 2L)
      mode  <- Gen.oneOf(SelectionMode.values.toSeq)
      picks <- Gen.containerOf[Vector, StudioRef](Gen.oneOf(refs(m)))
      context =
        if stale then ContextRevision(m.selection.context.value - 1) else m.selection.context
      last = m.selection.delivered.getOrElse(view, -1L)
    yield Intent.Select(
      SelectionInput(InputStamp(context, view, last + delta, InputCause.Pointer), mode, picks)
    )

  private def trail(m: AppModel): Gen[Vector[Place]] = Gen.oneOf(
    Vector.empty,
    queryTrail,
    queryTrail.take(1),
    queryTrail ++ Vector(Place.At(StudioRef.Fixation(p17enc03, fixation6)), Place.At(record)),
    AppModel.draftTrail(m.document),
    Vector(Place.Figures, Place.At(panelD)),
    Vector(Place.NewProject),
    m.document.datasets.map(d => Place.Dataset(d.id)).take(1) :+ Place.DataView(
      DataSection.Admission
    ),
    Vector(Place.Source(SourceRole.Fixations))
  )

  private def location(m: AppModel): Gen[Location] =
    for
      p <- Gen.oneOf(Perspective.values.toSeq)
      t <- trail(m)
    yield Location(p, t)

  private def run(m: AppModel): Gen[RunId] =
    Gen.oneOf(m.document.runs.map(_.id) :+ RunId(99))

  /** A progress report of `job` running `run`. */
  private def progress(job: JobId, run: RunId): Gen[ExecutionProgress] =
    for
      total <- Gen.oneOf(
        ProgressTotal.Exact(44845L),
        ProgressTotal.AtMost(50000L),
        ProgressTotal.Unknown
      )
      done <- Gen.choose(0L, 44845L)
      step <- Gen.choose(0L, 50L)
    yield ExecutionProgress(
      ok(
        for
          meter <- StageMeter
            .of(StageKind.Comparing, CountUnit.Pairs, 0L, ProgressTotal.Unknown)
          totals <- RunTotals.of(10L, ProgressTotal.Exact(10L), done, total)
          p      <- JobProgress
            .of(job, run, step, Segment.Comparing(0, PairDesign.Matched), meter, totals)
        yield p
      )
    )

  private val failure =
    StudioDiagnostic(
      "studio-execution.lost-job",
      DiagnosticLevel.Error,
      DiagnosticOrigin.Host,
      Vector.empty,
      "lost"
    )

  private def phase(job: JobId, run: RunId): Gen[JobPhase] =
    val p = progress(job, run)
    Gen
      .oneOf(
        Gen.const(JobPhase.Queued),
        p.map(JobPhase.Running(_)),
        Gen.option(p).map(JobPhase.Cancelling(_)),
        p.map(JobPhase.Succeeded(_)),
        Gen.option(p).map(JobPhase.Failed(Vector(failure, failure), _)),
        Gen.option(p).map(JobPhase.Cancelled(_)),
        Gen.option(p).map(JobPhase.Superseded(None, _))
      )
      .flatMap(identity)

  /** A job of one of the document's runs, by its handle when it has one. */
  private def job(m: AppModel): Gen[Option[ExecutionJob]] =
    val runs = m.document.runs
    if runs.isEmpty then Gen.const(None)
    else
      for
        r <- Gen.oneOf(runs)
        id = m.document.job(r.id).getOrElse(JobId(r.id.number + 100))
        stamp <- Gen.frequency(
          4 -> AppModel.stampOf(m.document, r.analysis, r.dataset),
          1 -> AppModel.stampOf(m.document, r.analysis, DatasetRevision(99))
        )
        ph <- phase(id, r.id)
      yield Some(ExecutionJob(id, r.id, stamp, ph))

  /** The notice the shelf is waiting for: a run of the required stamp. */
  private def awaited(m: AppModel): Option[Intent] =
    for
      stamp <- m.jobs.shelf.required
      run   <- m.document.runs.findLast(r =>
        r.analysis == stamp.revision && r.dataset == stamp.dataset
      )
    yield Intent.Execution(
      ExecutionEvent.Ready(
        RunReady(m.document.job(run.id).getOrElse(JobId(run.id.number + 100)), run.id, stamp)
      )
    )

  private def event(m: AppModel): Gen[Intent] =
    awaited(m).fold(Gen.const(Option.empty[Intent]))(i => Gen.oneOf(None, Some(i))).flatMap {
      case Some(i) => Gen.const(i)
      case None    => arbitraryEvent(m)
    }

  private def arbitraryEvent(m: AppModel): Gen[Intent] =
    job(m).flatMap {
      case None    => Gen.const(Intent.JobsChanged(Vector.empty))
      case Some(j) =>
        Gen.oneOf(
          Intent.Execution(ExecutionEvent.Changed(j)),
          Intent.Execution(ExecutionEvent.Ready(RunReady(j.id, j.run, j.stamp))),
          Intent.JobsChanged(Vector(j))
        )
    }

  private def jobIds(m: AppModel): Gen[JobId] =
    Gen.oneOf(m.jobs.jobs.map(_.id) ++ m.document.jobs.map(_.job) :+ JobId(999))

  val chord: Gen[KeyChord] =
    Gen.frequency(
      3 -> Gen.oneOf(CommandRegistry.keymap.keys.toSeq),
      1 -> Gen
        .zip(
          Gen.oneOf(Key.values.toSeq),
          Gen.someOf(Modifier.values.toSeq).map(_.toSet)
        )
        .map(KeyChord(_, _))
    )

  private val panes: Vector[PaneId] = StudioLayouts.spec.all.flatMap(_.panes.map(_.id)).distinct

  private val simple: Gen[Intent] = Gen.oneOf(
    Intent.Back,
    Intent.Forward,
    Intent.ReviewDraft,
    Intent.RequestDiscardDraft,
    Intent.Confirm,
    Intent.Dismiss,
    Intent.OpenDiagnostics,
    Intent.RequestImport,
    Intent.FocusNextPane,
    Intent.ToggleMaximize
  )

  /** One intent for `m`. */
  def intent(m: AppModel): Gen[Intent] = Gen.frequency(
    6 -> CommandGen.command(m.document).map(Intent.Dispatch(_)),
    2 -> Gen.oneOf(HistoryStack.values.toSeq).map(Intent.Undo(_)),
    1 -> Gen.oneOf(HistoryStack.values.toSeq).map(Intent.Redo(_)),
    2 -> Gen.oneOf(Perspective.values.toSeq).map(Intent.SwitchPerspective(_)),
    2 -> location(m).map(Intent.Navigate(_)),
    1 -> Gen.choose(-1, 8).map(Intent.OpenCrumb(_)),
    // S3.4: explain any place a trail can hold, from wherever the model is.
    2 -> trail(m).flatMap(t =>
      if t.isEmpty then Gen.const(Intent.Back) else Gen.oneOf(t).map(Intent.Explain(_))
    ),
    2 -> select(m),
    1 -> Gen.zip(Gen.oneOf(views), Gen.option(Gen.oneOf(refs(m)))).map(Intent.HoverOver(_, _)),
    1 -> jobIds(m).map(Intent.CancelJob(_)),
    2 -> Gen
      .oneOf(m.jobs.ready.map(_.run).toVector ++ Vector(RunId(99)) ++ m.document.runs.map(_.id))
      .map(Intent.ShowRun(_)),
    1 -> run(m).map(Intent.DismissReady(_)),
    3 -> simple,
    1 -> Gen.oneOf(Preset.values.toSeq).map(Intent.ChoosePreset(_)),
    2 -> Gen.oneOf(CommandRegistry.all).map(c => Intent.Invoke(c.id)),
    2 -> chord.map(Intent.KeyPressed(_)),
    1 -> Gen.oneOf(panes).map(Intent.FocusPane(_)),
    1 -> Gen.zip(Gen.oneOf(panes), trail(m)).map(Intent.PaneSubject(_, _)),
    3 -> event(m),
    1 -> Gen
      .zip(Gen.choose(0, 23), Gen.choose(0, 59), Gen.choose(0L, m.save.edits.value))
      .map((h, mm, e) => Intent.Saved(ok(ClockTime.of(h, mm)), EditMark(e))),
    1 -> Gen.const(Intent.SaveFailed("disk full"))
  )

  /** A step of a walk: the model before and the intent applied to it. */
  final case class Trace(
      before: AppModel,
      intent: Intent,
      after: AppModel,
      effects: Vector[AppEffect]
  )

  def walk(m: AppModel, n: Int): Gen[Vector[Trace]] =
    if n == 0 then Gen.const(Vector.empty)
    else
      intent(m).flatMap { i =>
        val (next, effects) = AppModel.update(m, i)
        walk(next, n - 1).map(Trace(m, i, next, effects) +: _)
      }

  /** A legal Save & run starts this population. Its presence in the laws
    * does not depend on selecting one command from a growing command enum.
    */
  def requestingSession(n: Int): Gen[Vector[Trace]] =
    if n == 0 then Gen.const(Vector.empty)
    else
      val before           = AppModel.open(t2, None)
      val request          = Intent.Dispatch(Command.SaveAndRun(None))
      val (after, effects) = AppModel.update(before, request)
      walk(after, n - 1).map(Trace(before, request, after, effects) +: _)

  def session(n: Int): Gen[Vector[Trace]] = Gen.frequency(
    4 -> start.flatMap(walk(_, n)),
    1 -> requestingSession(n)
  )
