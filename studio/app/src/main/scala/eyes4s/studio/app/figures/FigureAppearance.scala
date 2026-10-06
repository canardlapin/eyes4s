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
import eyes4s.studio.core.backend.{ReportRole, ReportView}
import eyes4s.studio.core.document.{MethodSpec, PanelLetter}
import eyes4s.studio.core.engine.StudioBuild
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
    widthsMm: Map[PanelLetter, Int],
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

  /** What the run's comparison method measures: cosine similarity of maps
    * is their spatial similarity; any other method is named.
    */
  def measure(method: MethodSpec): String = method.definition.name match
    case "eyes4s.cosine" => "spatial similarity"
    case _               => s"similarity by ${method.render}"

  /** "Figure 1. Matched-minus-control spatial similarity of retrieval gaze
    * (dataset r3, analysis rev 4, run 7). Spatial correspondence, not
    * sequential replay."
    */
  def figure(s: FigureSource): String =
    val recipe = s.bound.analysis.recipe
    val phase  = recipe.phases.focal.label.toLowerCase
    s"${s.figure.id.label}. Matched-minus-control ${measure(recipe.method)} of $phase gaze " +
      s"(dataset ${s.bound.dataset.id.label}, analysis ${s.bound.analysis.id.label}, " +
      s"${s.run.id.label}). Spatial correspondence, not sequential replay."

  /** Panel D's caption: what a dot (and a line) is, and the per-group n
    * range the summary serves.
    */
  def participantD(report: ReportView, lines: ParticipantLines): String =
    val dots = lines match
      case ParticipantLines.Shown  => "Each pair of dots is one participant"
      case ParticipantLines.Hidden => "Each dot is one participant's mean"
    val range = report
      .queryRange(ReportRole.Difference)
      .fold("group query counts unavailable")(r => s"${r.fewest}–${r.most} queries per group")
    s"$dots; per participant, $range. Descriptive only: no intervals or tests."

  /** The provenance stamp: "Analysis rev 4 · run 7 (archive unbound) · data r3
    * · reporting “By retrieval response” (sha256:1a2b…9f0) · studio build
    * eyes4s 0.1".
    *
    * A run records no producer version yet, so the version is the studio
    * build's eyes4s release line, labelled as such; the run's archive binding
    * identifies the result itself. A reporting spec has no revision and is
    * edited in place, so the CR3 digest of its content identifies the version
    * a figure was exported with (decision on bead S8.7); the bundle's README
    * gives its id and the full digest.
    */
  def stamp(s: FigureSource): String =
    s"Analysis ${s.bound.analysis.id.label} · ${s.run.id.label} (archive " +
      s"${s.run.archive.render}) · data ${s.bound.dataset.id.label} · reporting " +
      s"“${s.reporting.name}” (${specDigest(s.reporting, short = true)})" +
      " · studio build eyes4s " + StudioBuild.eyes4sBaseVersion

  /** `spec`'s digest as "sha256:<hex>", or "sha256:1a2b…9f0" when `short`;
    * "digest unavailable: why" if it cannot be computed.
    */
  def specDigest(spec: eyes4s.studio.core.document.ReportingSpec, short: Boolean): String =
    eyes4s.studio.core.document.ReportingSpec
      .digest(spec)
      .fold(
        e => s"digest unavailable: ${e.message}",
        d =>
          val hex = d.sha256.hex
          if short then s"sha256:${hex.take(4)}…${hex.takeRight(3)}" else s"sha256:$hex"
      )
