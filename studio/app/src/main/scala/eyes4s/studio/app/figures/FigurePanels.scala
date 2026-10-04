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

package eyes4s.studio.app.figures

import eyes4s.studio.app.plot.{
  ColumnFormat,
  ParticipantLines,
  ColumnId,
  ParticipantColumns,
  ParticipantMeans,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue,
  ProfileColumns,
  ScaleProfile
}
import eyes4s.studio.app.text.Format
import eyes4s.studio.core.assets.{AssetRegistry, DisplayKind}
import eyes4s.studio.core.backend.{Phase, ResultSummary, RunId, TrialKey}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.{PairScore, ReferenceScores}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** What a panel shows, read from the only two choices a panel makes, its
  * scale and its trial selection (DESIGN_SPEC section 12): the five panel
  * templates of the Figures board (S9.2a).
  */
enum PanelTemplate derives CanEqual:
  /** A: one encoding trial's gaze; B: one retrieval trial's gaze, with what
    * its screen displayed.
    */
  case Gaze(trial: TrialKey)

  /** C: one query's density maps beside its matched reference and its
    * highest-scoring control, at one scale.
    */
  case DensityMaps(sigma: Sigma, query: TrialKey)

  /** D: every participant's D by reporting group, at one scale. */
  case ParticipantD(sigma: Sigma)

  /** E: the grand means of D at every scale of the run. */
  case ScaleProfile

  /** A scale and selection no template draws, with why. */
  case NoTemplate(scale: PanelScale, selection: PanelSelection)

  def retrieval: Boolean = this match
    case Gaze(trial) => trial.phase == Phase.Retrieval
    case _           => false

object PanelTemplate:
  def of(panel: PanelSpec): PanelTemplate = (panel.scale, panel.selection) match
    case (PanelScale.Unscaled, PanelSelection.Trial(key))              => Gaze(key)
    case (PanelScale.At(s), PanelSelection.QueryWithReferences(query)) => DensityMaps(s, query)
    case (PanelScale.At(s), PanelSelection.AllQueries)                 => ParticipantD(s)
    case (PanelScale.AllScales, PanelSelection.AllQueries)             => ScaleProfile
    case (scale, selection) => NoTemplate(scale, selection)

/** The highest-scoring of the `scored` control pairs eyes4s scored, of the
  * `members` the control mean B is over. It is only ever shown with its rank
  * ("highest of 19 controls", or "highest of 12 scored of 19 controls" when
  * the backend scored fewer), so a single control's score is never read as
  * the control mean B.
  */
final case class HighestControl private (pair: PairScore, scored: Int, members: Int)
    derives CanEqual:
  /** "highest of 19 controls", "highest of 12 scored of 19 controls". */
  def rank: String =
    if scored == members then s"highest of $members controls"
    else s"highest of $scored scored of $members controls"

object HighestControl:
  /** The highest of `controls`, of `members` in all, when there is one. */
  def of(controls: Vector[PairScore], members: Int): Option[HighestControl] =
    controls
      .sortBy(c => (-c.score, c.reference.trial))
      .headOption
      .map(HighestControl(_, controls.size, members))

/** A drawn tile of panel C: its title and its score label. */
final case class MapTileVM(title: String, label: String, ref: StudioRef) derives CanEqual

/** Panel C: three tiles and the caption that says what each number is. The
  * one control tile is the highest control, labelled with its rank; B is
  * said only as "control mean B", in the caption.
  */
final case class DensityMapsVM(
    tiles: Vector[MapTileVM],
    caption: String,
    maps: String
) derives CanEqual

/** Which plot draws a panel's values. */
enum PlotKind derives CanEqual:
  case Participant, Profile

/** Panels D and E: the plot that draws them, its value source and notes. */
final case class PlotPanelVM(
    kind: PlotKind,
    source: PlotSource,
    notes: Vector[String],
    lines: ParticipantLines = ParticipantLines.Shown
) derives CanEqual

/** Panels A and B: the trial, what its screen displayed, and the gaze. */
final case class GazePanelVM(heading: String, displayed: String, gaze: String) derives CanEqual

/** What a panel's body shows. */
enum PanelBody derives CanEqual:
  case Gaze(vm: GazePanelVM)
  case Maps(vm: DensityMapsVM)
  case Plot(vm: PlotPanelVM)

  /** The panel cannot be drawn yet or at all; `reason` says why. */
  case Waiting(reason: String)
  case Unavailable(reason: String)

/** The panel bodies of the figure composer (S9.2a), from what the backend
  * served; nothing here is computed beyond ranking the control pair scores
  * eyes4s returned.
  */
object FigurePanels:

  /** Panel C's maps until the density grids are served (UI-E). */
  val MapsPending: String =
    "No density maps yet: they are drawn when the backend serves density grids (UI-E). " +
      "The tiles show their scores."

  /** Panels A and B's gaze until the trial-fixations view is served (S6.2). */
  val GazePending: String =
    "No gaze yet: fixations are drawn when the backend serves the trial-fixations view (S6.2)."

  private def two(v: Double) = Format.decimal(v, 2)

  /** The scale index of `sigma` in the bound run's scale set. */
  def scaleIndex(scales: ScaleSet, sigma: Sigma): Option[ScaleIndex] =
    Option(scales.values.indexOf(sigma)).filter(_ >= 0).flatMap(i => ScaleIndex.of(i).toOption)

  /** Panel C (Figures board): "Matched 0.73 · highest of 19 controls
    * street-112 0.61 · control mean B 0.35 · D +0.38".
    */
  def densityMaps(
      run: RunId,
      scale: ScaleIndex,
      sigma: Sigma,
      query: TrialKey,
      scores: ReferenceScores
  ): DensityMapsVM =
    val highest = HighestControl.of(scores.controls, scores.controlMembers)
    val pair    = (design: eyes4s.studio.core.backend.PairDesign, ref: TrialKey) =>
      StudioRef.Pair(run, scale, design, query, ref)
    val tiles = Vector(
      MapTileVM(
        s"Query ${query.trial}",
        s"${query.participant} · ${sigma.render}",
        StudioRef.QueryContrast(run, scale, query)
      ),
      MapTileVM(
        s"Matched ${scores.matched.reference.trial}",
        two(scores.matched.score),
        pair(eyes4s.studio.core.backend.PairDesign.Matched, scores.matched.reference)
      )
    ) ++ highest.map(h =>
      MapTileVM(
        s"${h.rank.capitalize} · ${h.pair.item}",
        two(h.pair.score),
        pair(eyes4s.studio.core.backend.PairDesign.Control, h.pair.reference)
      )
    )
    val control = highest.fold(
      s"no control pair score served of ${scores.controlMembers} controls"
    )(h => s"${h.rank} ${h.pair.item} ${two(h.pair.score)}")
    DensityMapsVM(
      tiles,
      s"Matched ${two(scores.matched.score)} · $control · control mean B ${two(scores.b)} · " +
        s"D ${Format.signed(scores.d, 2)}",
      // Follow-up bd-01M42K7ZYDRR90JXZYNDD2HVXP: the maps from the served grids.
      FigurePanels.MapsPending
    )

  /** Panel C's table: the contrast, the matched pair, every control pair. */
  def referenceTable(
      run: RunId,
      scale: ScaleIndex,
      query: TrialKey,
      scores: ReferenceScores
  ): Either[String, PlotSource] =
    for
      what  <- ColumnId.of("what").left.map(_.message)
      trial <- ColumnId.of("reference").left.map(_.message)
      value <- ColumnId.of("score").left.map(_.message)
      columns = Vector(
        PlotColumn(what, "Score", ColumnFormat.Label),
        PlotColumn(trial, "Reference", ColumnFormat.Label),
        PlotColumn(value, "Value", ColumnFormat.Label)
      )
      text     = (s: String) => PlotValue.Text(s)
      contrast = StudioRef.QueryContrast(run, scale, query)
      pair     = (design: eyes4s.studio.core.backend.PairDesign, p: PairScore) =>
        PlotRow(
          StudioRef.Pair(run, scale, design, query, p.reference),
          Vector(
            text(design.render),
            text(s"${p.reference.trial} · ${p.item}"),
            text(two(p.score))
          )
        )
      // One row per ref: the contrast row holds M, B and D together.
      rows = Vector(
        PlotRow(
          contrast,
          Vector(
            text("contrast"),
            text(s"${scores.controlMembers} controls"),
            text(
              s"M ${two(scores.m)} · control mean B ${two(scores.b)} · " +
                s"D = M − B ${Format.signed(scores.d, 2)}"
            )
          )
        ),
        pair(eyes4s.studio.core.backend.PairDesign.Matched, scores.matched)
      ) ++ scores.controls.map(pair(eyes4s.studio.core.backend.PairDesign.Control, _))
      source <- PlotSource(
        s"${query.label} at scale ${scale.value}",
        columns,
        rows
      ).left
        .map(_.message)
    yield source

  /** Panel D: every participant's D in every group at `scale`, and the
    * group and per-participant n the summary serves.
    */
  def participantD(
      summary: ResultSummary,
      reporting: ReportingId,
      scale: ScaleIndex,
      lines: ParticipantLines
  ): Either[String, PlotPanelVM] =
    for
      means   <- ParticipantMeans.of(summary, reporting, scale).left.map(_.message)
      columns <- ParticipantColumns.standard.left.map(_.message)
      source  <- ParticipantMeans.source(means, columns).left.map(_.message)
    yield PlotPanelVM(
      PlotKind.Participant,
      source,
      Vector(
        nEach(summary),
        "bars: grand mean of participant means, equal weight",
        FigureCaption.participantD(summary, lines)
      ),
      lines
    )

  /** Panel E: the grand means of D at each declared scale. */
  def scaleProfile(
      summary: ResultSummary,
      reporting: ReportingId,
      scales: ScaleSet
  ): Either[String, PlotPanelVM] =
    for
      profile <- ScaleProfile.of(summary, reporting, scales).left.map(_.message)
      columns <- ProfileColumns.standard.left.map(_.message)
      source  <- ScaleProfile.source(profile, columns).left.map(_.message)
    yield PlotPanelVM(
      PlotKind.Profile,
      source,
      Vector(nEach(summary), "Each scale computed separately.")
    )

  /** "n = 24 each · paired n = 24", or each group's n when they differ. */
  def nEach(summary: ResultSummary): String =
    val ns   = summary.groups.map(_.n).distinct
    val each =
      if ns.size == 1 then s"n = ${ns.head} each"
      else summary.groups.map(g => s"${g.label.label} n = ${g.n}").mkString(", ")
    s"$each · paired n = ${summary.pairedN}"

  /** Panels A and B: the heading ("Retrieval · ret_07"), and what the screen
    * displayed, from the revision's asset registry when it is served.
    */
  def gaze(trial: TrialKey, registry: Either[String, Option[AssetRegistry]]): GazePanelVM =
    val display = registry.toOption.flatten.flatMap(_.display(trial))
    val shown   = display.map(_.kind) match
      case Some(DisplayKind.Image)                  => "Displayed: image."
      case Some(DisplayKind.Blank)                  => "Displayed: blank."
      case Some(DisplayKind.BlankWithFixationCross) => "Displayed: blank + fixation cross."
      case Some(DisplayKind.Cue)                    => "Displayed: cue."
      case Some(DisplayKind.Unknown)                => "Displayed: unknown display."
      case None                                     =>
        registry.fold(
          why => s"What the screen displayed could not be read: $why",
          _ => "What the screen displayed is not served."
        )
    val remembered =
      // Said only when the display is known: an unread one claims nothing.
      if trial.phase == Phase.Retrieval && display.exists(_.kind != DisplayKind.Image) then
        " The remembered image was not shown."
      else ""
    // The item names what was shown, so only an image display carries it.
    val item =
      display
        .filter(_.kind == DisplayKind.Image)
        .flatMap(_.item)
        .fold("")(i => s" · ${i.value}")
    GazePanelVM(
      s"${trial.phase.label} · ${trial.trial}$item",
      shown + remembered,
      // Follow-up bd-01M42K7ZNCC5J9R9RPR7H6ZHNT: the gaze from trialFixations.
      FigurePanels.GazePending
    )
