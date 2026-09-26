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

import eyes4s.studio.app.jobs.{JobBoard, JobPhase, JobSummary, MeterTotal, PairMeter}
import eyes4s.studio.app.keys.{CommandRegistry, Key, KeyChord, Modifier}
import eyes4s.studio.app.layout.{PaneId, StudioLayouts}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.{RunId, StageKind}
import eyes4s.studio.core.command.{CommandGen, HistoryStack}
import eyes4s.studio.core.document.{DocumentGen, Perspective, SourceRole}
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

  private val meter: Gen[PairMeter] =
    for
      stage <- Gen.oneOf(StageKind.values.toSeq)
      total <- Gen.oneOf(
        MeterTotal.Exact(44845L),
        MeterTotal.AtMost(50000L),
        MeterTotal.Counting
      )
      done <- Gen.choose(0L, 44845L)
    yield ok(PairMeter.of(stage, done, total))

  private val phase: Gen[JobPhase] = Gen.oneOf(
    JobPhase.Queued,
    JobPhase.Running,
    JobPhase.Cancelling,
    JobPhase.Succeeded,
    JobPhase.Failed(2),
    JobPhase.Failed(0),
    JobPhase.Cancelled,
    JobPhase.Superseded
  )

  private def board(m: AppModel): Gen[JobBoard] =
    val runs = m.document.runs
    for
      jobs <- Gen.sequence[Vector[JobSummary], JobSummary](
        runs.map(r =>
          Gen.zip(phase, Gen.option(meter)).map((p, mt) => JobSummary(r.id, r.analysis, p, mt))
        )
      )
      ready <- Gen.someOf(runs.map(_.id)).map(_.toVector)
    yield JobBoard(jobs, ready)

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
    2 -> select(m),
    1 -> Gen.zip(Gen.oneOf(views), Gen.option(Gen.oneOf(refs(m)))).map(Intent.HoverOver(_, _)),
    1 -> run(m).map(Intent.CancelJob(_)),
    1 -> run(m).map(Intent.ShowRun(_)),
    3 -> simple,
    2 -> Gen.oneOf(CommandRegistry.all).map(c => Intent.Invoke(c.id)),
    2 -> chord.map(Intent.KeyPressed(_)),
    1 -> Gen.oneOf(panes).map(Intent.FocusPane(_)),
    1 -> Gen.zip(Gen.oneOf(panes), trail(m)).map(Intent.PaneSubject(_, _)),
    1 -> board(m).map(Intent.JobsChanged(_)),
    1 -> Gen
      .zip(Gen.choose(0, 23), Gen.choose(0, 59))
      .map((h, mm) => Intent.Saved(ok(ClockTime.of(h, mm))))
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

  def session(n: Int): Gen[Vector[Trace]] = start.flatMap(walk(_, n))
