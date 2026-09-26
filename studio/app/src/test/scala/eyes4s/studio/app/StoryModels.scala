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

import eyes4s.studio.app.layout.PaneId
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.execution.{ExecutionJob, ExecutionProgress, JobPhase, RunStamp}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{MockStudy, StoryMoments}
import eyes4s.studio.core.freshness.{DraftCheck, SessionFacts}
import eyes4s.studio.core.selection.*

/** App models at the story moments of FIXTURE.md, arranged as each board
  * shows them: the same intents a user (or the S3.6 driver) would dispatch.
  */
object StoryModels:

  def ok[E, A](e: Either[E, A]): A = e.fold(err => sys.error(s"fixture: $err"), identity)

  lazy val t1: StudioDocument = ok(StoryMoments.t1)
  lazy val t2: StudioDocument = ok(StoryMoments.t2)
  lazy val t3: StudioDocument = ok(StoryMoments.t3)

  lazy val empty: StudioDocument = ok(
    StudioDocument.of(
      Vector.empty,
      Vector.empty,
      None,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      Vector.empty
    )
  )

  lazy val items: TrialItems =
    TrialItems(ok(MockStudy.load).inventory.map(e => e.trial -> e.item).toMap)

  val project: ProjectName = ok(ProjectName.of("memory-study"))
  val savedAt: ClockTime   = ok(ClockTime.of(10, 24))

  val p17ret07               = MockStudy.key("P17", "ret_07")
  val p17enc03               = MockStudy.key("P17", "enc_03")
  val sigma2                 = ok(ScaleIndex.of(2))
  val reporting: ReportingId = ok(StoryMoments.byResponseId)
  val remembered: Response   = Response("Remembered")

  val query: StudioRef = StudioRef.QueryContrast(StoryMoments.run7, sigma2, p17ret07)
  val pair: StudioRef  =
    StudioRef.Pair(StoryMoments.run7, sigma2, PairDesign.Matched, p17ret07, p17enc03)
  val fixation6: FixationIndex = ok(FixationIndex.of(6))
  val record: StudioRef        = StudioRef.SourceRecord(
    p17enc03,
    Some(fixation6),
    SourceRole.Fixations,
    ok(RecordNumber.of(7214))
  )
  val panelD: StudioRef     = StudioRef.FigurePanel(ok(FigureId.of(1)), ok(PanelLetter.of("D")))
  val p17Summary: StudioRef =
    StudioRef.ParticipantSummary(StoryMoments.run7, reporting, sigma2, None, "P17")

  /** Main's trail: summary → group → participant → query → pair. */
  val queryTrail: Vector[Place] = Vector(
    Place.Summary(reporting),
    Place.Group(reporting, remembered),
    Place.At(StudioRef.Participant("P17")),
    Place.At(query),
    Place.At(pair)
  )

  /** A selection input stamped against the model's current context, with
    * the view's next sequence number.
    */
  def select(model: AppModel, view: String, refs: StudioRef*): Intent =
    val id       = ok(ViewId.of(view))
    val sequence = model.selection.delivered.get(id).fold(0L)(_ + 1)
    Intent.Select(
      SelectionInput(
        InputStamp(model.selection.context, id, sequence, InputCause.Pointer),
        SelectionMode.Replace,
        refs.toVector
      )
    )

  /** Apply intents one at a time, each built from the model it meets. */
  def play(model: AppModel, steps: (AppModel => Intent)*): AppModel =
    steps.foldLeft(model)((m, step) => AppModel.update(m, step(m))._1)

  private def opened(document: StudioDocument): AppModel =
    play(AppModel.open(document, Some(project)), _ => Intent.ItemsLoaded(items))

  private def saved(model: AppModel): AppModel =
    AppModel.update(model, Intent.Saved(savedAt))._1

  private def draftReady(document: StudioDocument): Intent =
    Intent.SessionChanged(
      SessionFacts.empty.copy(draftCheck = DraftCheck.Checked(document.draft.get, Vector.empty))
    )

  /** DataEmpty.dc.html: a new, untitled project. */
  def firstRun: AppModel = AppModel.open(empty, None)

  /** Data.dc.html, t1: verifying r3's admission, fixations.csv in focus. */
  def t1Data: AppModel = saved(
    play(
      opened(t1),
      m =>
        Intent.Navigate(
          Location(Perspective.Data, m.location.trail :+ Place.DataView(DataSection.Admission))
        ),
      _ =>
        Intent.PaneSubject(
          ok(PaneId.of("data.admission")),
          Vector(Place.Source(SourceRole.Fixations), Place.DataView(DataSection.Admission))
        )
    )
  )

  /** Main.dc.html, t2: the matched pair of P17 ret_07 selected. */
  def t2Compare: AppModel = saved(
    play(
      opened(t2),
      _ => draftReady(t2),
      _ => Intent.Navigate(Location(Perspective.Compare, queryTrail)),
      m => select(m, "compare.contrast", pair)
    )
  )

  /** Explore.dc.html, t2: fixation 6 of enc_03 and its source record. */
  def t2Explore: AppModel = saved(
    play(
      t2Compare,
      _ =>
        Intent.Navigate(
          Location(
            Perspective.Explore,
            queryTrail ++ Vector(
              Place.At(StudioRef.Fixation(p17enc03, fixation6)),
              Place.At(record)
            )
          )
        ),
      m => select(m, "explore.trial-view", record)
    )
  )

  /** Analysis.dc.html, t2: draft rev 5, the Scales field in focus. */
  def t2Analysis: AppModel = saved(
    play(
      opened(t2),
      _ => draftReady(t2),
      _ => Intent.ReviewDraft,
      _ =>
        Intent.PaneSubject(
          ok(PaneId.of("analysis.recipe")),
          Vector(Place.Revision(StoryMoments.rev5), Place.Field(RecipeField.Scales))
        )
    )
  )

  /** Figures.dc.html, t2: Figure 1, panel D selected. */
  def t2Figures: AppModel = saved(
    play(
      opened(t2),
      _ => draftReady(t2),
      _ =>
        Intent.Navigate(
          Location(
            Perspective.Figures,
            Vector(Place.Figures, Place.Figure(ok(FigureId.of(1))), Place.At(panelD))
          )
        ),
      m => select(m, "figures.page", panelD)
    )
  )

  /** Run 8's stamp: rev 5 on r3, as t3 saves it. */
  lazy val run8Stamp: RunStamp = AppModel.stampOf(t3, StoryMoments.rev5, StoryMoments.r3)

  /** A progress report of run 8's job: `pairs` of 44,845 compared, or of a
    * total still being counted.
    */
  def run8Progress(pairs: Long, counting: Boolean = false): ExecutionProgress =
    val total = if counting then ProgressTotal.Unknown else ProgressTotal.Exact(44845L)
    ExecutionProgress(
      ok(
        for
          meter <- StageMeter
            .of(StageKind.Comparing, CountUnit.Pairs, 0L, ProgressTotal.Unknown)
          totals <- RunTotals.of(4685L, ProgressTotal.Exact(4685L), pairs, total)
          p      <- JobProgress.of(
            StoryMoments.run8Job,
            StoryMoments.run8,
            1L,
            Segment.Comparing(2, PairDesign.Matched),
            meter,
            totals
          )
        yield p
      )
    )

  def run8Job(phase: JobPhase): ExecutionJob =
    ExecutionJob(StoryMoments.run8Job, StoryMoments.run8, run8Stamp, phase)

  /** Run 8 at 21,400 of 44,845 pairs. */
  lazy val run8Running: ExecutionJob = run8Job(JobPhase.Running(run8Progress(21400L)))

  /** Results.dc.html, t3: the summary, P17 selected, run 8 running. */
  def t3Summary: AppModel = saved(
    play(
      opened(t3),
      _ => Intent.JobsChanged(Vector(run8Running)),
      m => select(m, "compare.participant-plot", p17Summary)
    )
  )
