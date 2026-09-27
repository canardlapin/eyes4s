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

package eyes4s.studio.core.geometry

import cats.syntax.all.*
import eyes4s.kernel.{Correction, Grid, HalfOpenPlacement, PointMeasure, Pt, Unit2D}
import eyes4s.plan.{AdmissionPolicy, AppliedCorrection, CorrectionScope}
import eyes4s.studio.core.backend.{DatasetRevision, TrialKey}
import eyes4s.studio.core.document.{
  AdmissionChoice,
  CoordinateCorrection,
  CorrectionTarget,
  DatasetRevisionSpec
}

/** Where a corrected position falls, as eyes4s decides it: outside the
  * screen (the admission frame); on the screen but outside the half-open
  * image frame (the analysis window); or inside it, at `image` pixels.
  */
enum Placement derives CanEqual:
  case OutsideScreen
  case OutsideWindow
  case Inside(image: Pt[Unit2D.Px])

  def isInside: Boolean = this match
    case Inside(_) => true
    case _         => false

/** A source position with the correction rule that covers its trial, if
  * any, applied by eyes4s. `source` is the position as recorded; the
  * correction is never written back into it.
  */
final case class PlacedPosition(
    source: SourcePosition,
    rule: Option[Int],
    corrected: Pt[Unit2D.Px],
    placement: Placement
) derives CanEqual

/** A dataset revision's recorded corrections, the admission policy its
  * ledger records (UI-A), read by eyes4s: the rule covering a trial is
  * eyes4s `AdmissionPolicy.correctionFor`, and a corrected position is
  * eyes4s `Correction.correct` on the screen frame. The document records
  * rules; applying them derives a view and never rewrites a source
  * coordinate.
  */
final class CorrectionLedger private (
    val frames: DisplayFrames,
    val policy: AdmissionPolicy[TrialKey]
):
  def dataset: DatasetRevision = frames.dataset

  /** The rule covering `trial`, by index, or the conflict eyes4s refuses. */
  def ruleFor(trial: TrialKey): Either[GeometryProblem, Option[(Int, Correction)]] =
    policy
      .correctionFor(trial, _.participant)
      .left
      .map((a, b) => GeometryProblem.CorrectionConflict(trial, a, b))

  /** `p` corrected and placed. */
  def place(p: SourcePosition): Either[GeometryProblem, PlacedPosition] =
    ruleFor(p.trial).flatMap {
      case None                => Right(PlacedPosition(p, None, p.point, placementOf(p.point)))
      case Some((index, rule)) =>
        rule
          .correct(frames.screen, p.point)
          .toRight(GeometryProblem.Uncorrectable(p.record, index))
          .map(c => PlacedPosition(p, Some(index), c, placementOf(c)))
    }

  /** Every position placed, in order; the first refusal stops. */
  def placeAll(ps: Vector[SourcePosition]): Either[GeometryProblem, Vector[PlacedPosition]] =
    val out                              = Vector.newBuilder[PlacedPosition]
    var problem: Option[GeometryProblem] = None
    val it                               = ps.iterator
    while problem.isEmpty && it.hasNext do
      place(it.next()) match
        case Right(p) => out += p: Unit
        case Left(e)  => problem = Some(e)
    problem.toLeft(out.result())

  private def placementOf(p: Pt[Unit2D.Px]): Placement =
    if !frames.screen.contains(p) then Placement.OutsideScreen
    else
      frames.image.locate(p) match
        case HalfOpenPlacement.Inside(local) => Placement.Inside(local)
        case HalfOpenPlacement.Outside(_)    => Placement.OutsideWindow

object CorrectionLedger:

  def of(spec: DatasetRevisionSpec): Either[GeometryProblem, CorrectionLedger] =
    for
      frames <- DisplayFrames.of(spec.id, spec.geometry)
      policy <- policy(spec.id, spec.admission)
    yield new CorrectionLedger(frames, policy)

  /** A revision's admission choice as the eyes4s policy it records. */
  def policy(
      dataset: DatasetRevision,
      choice: AdmissionChoice
  ): Either[GeometryProblem, AdmissionPolicy[TrialKey]] =
    choice.corrections
      .traverse { rule =>
        val scope: CorrectionScope[TrialKey] = rule.target match
          case CorrectionTarget.AllTrials      => CorrectionScope.AllTrials()
          case CorrectionTarget.Participant(p) => CorrectionScope.Participant(p.value)
          case CorrectionTarget.Trial(key)     => CorrectionScope.Trial(key)
        correction(dataset, rule.correction).map(AppliedCorrection(scope, _))
      }
      .map(AdmissionPolicy(choice.offScreen.core, _))

  /** A recorded correction as eyes4s's. */
  def correction(
      dataset: DatasetRevision,
      c: CoordinateCorrection
  ): Either[GeometryProblem, Correction] = c match
    case CoordinateCorrection.FlipX        => Right(Correction.FlipX)
    case CoordinateCorrection.FlipY        => Right(Correction.FlipY)
    case CoordinateCorrection.Translate(o) =>
      Correction.translate(o.dx, o.dy).left.map(GeometryProblem.Frames(dataset, _))

/** One record's coordinates, raw → corrected → image frame → degrees
  * (DESIGN_SPEC section 9: "show raw and transformed coordinates"). Every
  * step is eyes4s's: `image` is the position in the image frame's own
  * coordinates (eyes4s `Subframe.enter`, also for a position outside it),
  * and `degrees` applies eyes4s's declared linear scale on that frame, from
  * its centre with x right and y up.
  */
final case class WorkedExample(
    placed: PlacedPosition,
    image: Pt[Unit2D.Px],
    degrees: Option[Pt[Unit2D.Deg]]
) derives CanEqual:
  def record: Int = placed.source.record

object WorkedExample:
  def of(ledger: CorrectionLedger, p: SourcePosition): Either[GeometryProblem, WorkedExample] =
    ledger.place(p).map { placed =>
      val image = placed.placement match
        case Placement.Inside(local) => local
        case _ => ledger.frames.image.enter(placed.corrected).getOrElse(placed.corrected)
      WorkedExample(placed, image, ledger.frames.toDegrees(image))
    }

/** Every placed record binned by eyes4s (`PointMeasure.binned`) on a grid
  * over the screen, counts per cell in eyes4s's row-major order (row 0 at
  * the top of the screen). A position off the screen falls in no cell:
  * eyes4s never clamps it to an edge.
  */
final case class PlacementDensity(
    columns: Int,
    rows: Int,
    counts: IArray[Double],
    placed: Int
):
  /** How many placed records fell in a cell. */
  def binned: Int = counts.foldLeft(0.0)(_ + _).round.toInt

object PlacementDensity:
  /** Columns across the screen; rows keep the cells near square. */
  val Columns: Int = 64

  def of(
      frames: DisplayFrames,
      placed: Vector[PlacedPosition]
  ): Either[GeometryProblem, PlacementDensity] =
    val rows = math.max(1, math.round(Columns * frames.screen.height / frames.screen.width).toInt)
    for
      grid <- Grid
        .over(frames.screen, Columns, rows)
        .left
        .map(GeometryProblem.Frames(frames.dataset, _))
      measure <- PointMeasure
        .of(frames.screen, IArray.from(placed.map(_.corrected)), IArray.fill(placed.size)(1.0))
        .left
        .map(GeometryProblem.Measure(frames.dataset, _))
      counts <- measure.binned(grid).left.map(GeometryProblem.Frames(frames.dataset, _))
    yield PlacementDensity(Columns, rows, counts, placed.size)
