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

import eyes4s.studio.app.plot.ParticipantLines
import eyes4s.studio.app.tokens.FontFace
import eyes4s.studio.core.backend.ResultSummary
import eyes4s.studio.core.document.PanelLetter
import eyes4s.studio.core.engine.Eyes4sVersion
import eyes4s.studio.core.figures.FigureSource

/** The figure's body text size (Figures board, Appearance: "6 pt 7 pt 8 pt"). */
enum FigureTextSize(val pt: Int) derives CanEqual:
  case Six   extends FigureTextSize(6)
  case Seven extends FigureTextSize(7)
  case Eight extends FigureTextSize(8)

  def label: String = s"$pt pt"

/** The figure typography (DESIGN_SPEC section 12, Figures board): Plex Sans
  * at the chosen body size, panel letters at 8 pt in the semibold face. Sizes
  * are in points on the page; [[FigureType.px]] places them at a zoom.
  */
object FigureType:
  val LetterPt: Int               = 8
  val BodyFace: FontFace          = FontFace.SansRegular
  val LetterFace: FontFace        = FontFace.SansSemiBold
  val MillimetresPerPoint: Double = 25.4 / 72.0

  /** `pt` points on a page drawn at `pxPerMm` pixels per millimetre. */
  def px(pt: Double, pxPerMm: Double): Double = pt * MillimetresPerPoint * pxPerMm

/** How a figure looks, which never changes its science (the board's
  * "Appearance · View only"): its text size, whether the participant plot
  * joins a participant's means, each panel's width when set, and whether
  * its export's project snapshot includes the stimulus images. It is the
  * session's, like the page's width and zoom.
  */
final case class FigureAppearance(
    text: FigureTextSize,
    lines: ParticipantLines,
    widths: Map[PanelLetter, Int],
    includeImages: Boolean
) derives CanEqual

object FigureAppearance:
  val default: FigureAppearance =
    FigureAppearance(
      FigureTextSize.Seven,
      ParticipantLines.Shown,
      Map.empty,
      includeImages = true
    )

  /** A panel's narrowest width, in millimetres. */
  val MinPanelMm: Int = 30

/** The figure's generated text: every number in it comes from the figure's
  * binding or its run's summary, never typed.
  */
object FigureCaption:

  /** "Figure 1. Matched-minus-control spatial similarity of retrieval gaze
    * (dataset r3, analysis rev 4, run 7). Spatial correspondence, not
    * sequential replay."
    */
  def figure(s: FigureSource): String =
    val phase = s.bound.analysis.recipe.phases.focal.label.toLowerCase
    s"${s.figure.id.label}. Matched-minus-control spatial similarity of $phase gaze " +
      s"(dataset ${s.bound.dataset.id.label}, analysis ${s.bound.analysis.id.label}, " +
      s"${s.run.id.label}). Spatial correspondence, not sequential replay."

  /** Panel D's caption: what a dot (and a line) is, and the per-group n
    * range the summary serves.
    */
  def participantD(summary: ResultSummary, lines: ParticipantLines): String =
    val dots = lines match
      case ParticipantLines.Shown  => "Each pair of dots is one participant"
      case ParticipantLines.Hidden => "Each dot is one participant's mean"
    s"$dots; per participant, ${summary.groupNMinimum}–${summary.groupNMaximum} queries per " +
      "group. Descriptive only: no intervals or tests."

  /** The provenance stamp: "Analysis rev 4 · run 7 · data r3 · reporting
    * “By retrieval response” · eyes4s 0.1".
    */
  def stamp(s: FigureSource): String =
    s"Analysis ${s.bound.analysis.id.label} · ${s.run.id.label} · data " +
      s"${s.bound.dataset.id.label} · reporting “${s.reporting.name}” · eyes4s " +
      Eyes4sVersion.value
