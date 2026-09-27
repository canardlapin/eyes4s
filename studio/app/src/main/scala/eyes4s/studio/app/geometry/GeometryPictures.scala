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

package eyes4s.studio.app.geometry

import eyes4s.studio.core.backend.{DatasetRevision, TrialKey}
import eyes4s.studio.core.document.{AdmissionChoice, CorrectionRule, DatasetRevisionSpec, Geometry}
import eyes4s.studio.core.geometry.*

/** Where a drawn record falls, as eyes4s placed it. */
enum MarkPlace derives CanEqual:
  case Inside, OutsideWindow, OutsideScreen

object MarkPlace:
  def of(p: Placement): MarkPlace = p match
    case Placement.Inside(_)     => Inside
    case Placement.OutsideWindow => OutsideWindow
    case Placement.OutsideScreen => OutsideScreen

/** One record drawn in a thumbnail, at its corrected screen position. */
final case class MarkPicture(x: Double, y: Double, place: MarkPlace, corrected: Boolean)
    derives CanEqual

/** The screen and the image frame on it, in screen pixels. */
final case class FramePicture(
    screenWidth: Int,
    screenHeight: Int,
    left: Int,
    top: Int,
    width: Int,
    height: Int
) derives CanEqual

object FramePicture:
  def of(g: Geometry): FramePicture =
    FramePicture(
      g.screen.width,
      g.screen.height,
      g.image.left,
      g.image.top,
      g.image.width,
      g.image.height
    )

/** One representative trial: its records and how many eyes4s places outside
  * the image frame and outside the screen.
  */
final case class ThumbnailPicture(
    trial: TrialKey,
    records: Int,
    outsideWindow: Int,
    outsideScreen: Int,
    marks: Vector[MarkPicture]
) derives CanEqual:
  def outside: Int = outsideWindow + outsideScreen

/** eyes4s's binned counts of every placed record as ramp levels, by the
  * cell's share of the fullest cell: below [[DensityPicture.Floor]] it is
  * not drawn (0), then the four map-ramp stops (DESIGN_SPEC section 6) at
  * shares from the floor, 1/4, 1/2 and 3/4. A lone stray record does not
  * paint the screen; a participant's displaced mass does. Row 0 is the top
  * row.
  */
final case class DensityPicture(columns: Int, rows: Int, levels: Vector[Int], records: Int)
    derives CanEqual

object DensityPicture:
  val Levels: Int = 4

  /** The smallest share of the fullest cell that is drawn. */
  val Floor: Double = 0.05

  def of(d: PlacementDensity): DensityPicture =
    val max    = d.counts.foldLeft(0.0)(_ max _)
    val levels = d.counts.toVector.map { c =>
      val share = if max <= 0.0 then 0.0 else c / max
      if c <= 0.0 || share < Floor then 0
      else math.min(Levels, 1 + math.floor(Levels * share).toInt)
    }
    DensityPicture(d.columns, d.rows, levels, d.placed)

/** The worked example of one record, raw → corrected → image → degrees. */
final case class ExamplePicture(
    record: Int,
    trial: TrialKey,
    rawX: Double,
    rawY: Double,
    rule: Option[(Int, CorrectionRule)],
    correctedX: Double,
    correctedY: Double,
    imageX: Double,
    imageY: Double,
    place: MarkPlace,
    degrees: Option[(Double, Double)]
) derives CanEqual

/** What the panel draws from a revision's records: derived off the UI
  * thread by [[GeometryPictures.of]], shown when ready. `key` says which
  * revision content, records and choices it was drawn for.
  */
final case class GeometryPictures(
    key: PicturesKey,
    frame: FramePicture,
    thumbnails: Vector[ThumbnailPicture],
    density: DensityPicture,
    example: Option[ExamplePicture],
    unplaced: Int
) derives CanEqual

/** Everything the pictures depend on; equal keys draw equal pictures. */
final case class PicturesKey(
    dataset: DatasetRevision,
    geometry: Geometry,
    admission: AdmissionChoice,
    positions: PositionsKey,
    marked: Option[TrialKey],
    focusTrial: Option[TrialKey],
    focusRecord: Option[Int]
) derives CanEqual

object GeometryPictures:

  /** Thumbnails in the placement check (the board's two-by-two grid). */
  val Thumbnails: Int = 4

  /** The key of the pictures `panel` shows for `model`, once its records are read. */
  def keyOf(panel: GeometryPanel, model: eyes4s.studio.app.AppModel): Option[PicturesKey] =
    val (trial, record) = GeometryPanel.focus(model)
    for
      spec <- panel.shown.flatMap(s => model.document.dataset(s.id))
      key  <- panel.positionsKey
      _    <- panel.positions.toOption
    yield PicturesKey(spec.id, spec.geometry, spec.admission, key, panel.marked, trial, record)

  /** The pictures of `spec`'s records: every record placed by eyes4s under
    * the revision's geometry and recorded corrections, the representative
    * trials, the all-trials density and the worked example.
    *
    * The representative trials are, in order and without repeats: the
    * source's first trial; the selected trial; the marked trial; each trial
    * a trial-scoped rule corrects; then the trials with the most records
    * outside the image frame (ties in source order), then the next trials
    * in source order.
    */
  def of(
      key: PicturesKey,
      spec: DatasetRevisionSpec,
      positions: SourcePositions
  ): Either[GeometryProblem, GeometryPictures] =
    for
      ledger  <- CorrectionLedger.of(spec)
      placed  <- ledger.placeAll(positions.positions)
      density <- PlacementDensity.of(ledger.frames, placed)
      example <- exampleOf(key, positions, ledger)
    yield
      val byTrial  = placed.groupBy(_.source.trial)
      val trials   = positions.trials
      val outside  = trials.map(t => t -> byTrial(t).count(!_.placement.isInside))
      val worst    = outside.filter(_._2 > 0).sortBy((_, n) => -n).map(_._1)
      val ruled    = spec.admission.corrections.collect {
        case CorrectionRule(eyes4s.studio.core.document.CorrectionTarget.Trial(k), _) => k
      }
      val chosen = (trials.take(1) ++ key.focusTrial ++ key.marked ++ ruled ++ worst ++ trials)
        .filter(byTrial.contains)
        .distinct
        .take(Thumbnails)
      GeometryPictures(
        key,
        FramePicture.of(spec.geometry),
        chosen.map(t => thumbnail(t, byTrial(t))),
        DensityPicture.of(density),
        example,
        positions.unplaced.size
      )

  private def thumbnail(trial: TrialKey, placed: Vector[PlacedPosition]): ThumbnailPicture =
    val places = placed.map(p => MarkPlace.of(p.placement))
    ThumbnailPicture(
      trial,
      placed.size,
      places.count(_ == MarkPlace.OutsideWindow),
      places.count(_ == MarkPlace.OutsideScreen),
      placed.zip(places).map((p, place) =>
        MarkPicture(p.corrected.x, p.corrected.y, place, p.rule.isDefined)
      )
    )

  /** The selected record, else the marked trial's first, else the first. */
  private def exampleOf(
      key: PicturesKey,
      positions: SourcePositions,
      ledger: CorrectionLedger
  ): Either[GeometryProblem, Option[ExamplePicture]] =
    val chosen = key.focusRecord
      .flatMap(positions.position)
      .orElse(key.marked.flatMap(t => positions.byTrial.get(t).flatMap(_.headOption)))
      .orElse(key.focusTrial.flatMap(t => positions.byTrial.get(t).flatMap(_.headOption)))
      .orElse(positions.positions.headOption)
    chosen match
      case None    => Right(None)
      case Some(p) =>
        WorkedExample.of(ledger, p).map { w =>
          val rule = w.placed.rule.flatMap(i => key.admission.corrections.lift(i).map(i -> _))
          Some(
            ExamplePicture(
              p.record,
              p.trial,
              p.x,
              p.y,
              rule,
              w.placed.corrected.x,
              w.placed.corrected.y,
              w.image.x,
              w.image.y,
              MarkPlace.of(w.placed.placement),
              w.degrees.map(d => (d.x, d.y))
            )
          )
        }
