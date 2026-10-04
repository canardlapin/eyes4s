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

import eyes4s.studio.core.backend.{
  DatasetRevision,
  PlacedRecord,
  PlacementDensityGrid,
  PlacementPreview,
  RecordPlacement,
  TrialKey
}
import eyes4s.studio.core.document.{
  AdmissionChoice,
  CorrectionRule,
  DatasetRevisionSpec,
  Geometry
}

/** Where a drawn record falls, as eyes4s placed it. */
enum MarkPlace derives CanEqual:
  case Inside, OutsideWindow, OutsideScreen

object MarkPlace:
  def of(p: RecordPlacement): MarkPlace = p match
    case RecordPlacement.Inside        => Inside
    case RecordPlacement.OutsideWindow => OutsideWindow
    case RecordPlacement.OutsideScreen => OutsideScreen

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

  def of(d: PlacementDensityGrid): DensityPicture =
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
    placement: PlacementKey,
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
      key  <- panel.placementKey
      _    <- panel.placement.toOption
    yield PicturesKey(spec.id, spec.geometry, spec.admission, key, panel.marked, trial, record)

  /** The pictures of `spec`'s records as the backend placed them (eyes4s,
    * under the revision's geometry and recorded corrections): the
    * representative trials, the all-trials density and the worked example.
    * Nothing is placed here; the counts are the backend's trial tallies.
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
      preview: PlacementPreview
  ): GeometryPictures =
    val byTrial = preview.byTrial
    val tallies = preview.trials
    val trials  = tallies.map(_.trial)
    val worst   = tallies
      .filter(t => t.outsideWindow + t.outsideScreen > 0)
      .sortBy(t => -(t.outsideWindow + t.outsideScreen))
      .map(_.trial)
    val ruled = spec.admission.corrections.collect {
      case CorrectionRule(eyes4s.studio.core.document.CorrectionTarget.Trial(k), _) => k
    }
    val chosen = (trials.take(1) ++ key.focusTrial ++ key.marked ++ ruled ++ worst ++ trials)
      .filter(byTrial.contains)
      .distinct
      .take(Thumbnails)
    val tallyOf = tallies.map(t => t.trial -> t).toMap
    GeometryPictures(
      key,
      FramePicture.of(spec.geometry),
      chosen.map(t => thumbnail(tallyOf(t), byTrial(t))),
      DensityPicture.of(preview.density),
      exampleOf(key, preview),
      preview.unplaced.size
    )

  private def thumbnail(
      tally: eyes4s.studio.core.backend.TrialPlacement,
      placed: Vector[PlacedRecord]
  ): ThumbnailPicture =
    ThumbnailPicture(
      tally.trial,
      tally.records,
      tally.outsideWindow,
      tally.outsideScreen,
      placed.map(p =>
        MarkPicture(p.correctedX, p.correctedY, MarkPlace.of(p.placement), p.rule.isDefined)
      )
    )

  /** The selected record, else the marked trial's first, else the first. */
  private def exampleOf(key: PicturesKey, preview: PlacementPreview): Option[ExamplePicture] =
    key.focusRecord
      .flatMap(preview.record)
      .orElse(key.marked.flatMap(t => preview.byTrial.get(t).flatMap(_.headOption)))
      .orElse(key.focusTrial.flatMap(t => preview.byTrial.get(t).flatMap(_.headOption)))
      .orElse(preview.records.headOption)
      .map { p =>
        ExamplePicture(
          p.record,
          p.trial,
          p.rawX,
          p.rawY,
          p.rule.flatMap(i => key.admission.corrections.lift(i).map(i -> _)),
          p.correctedX,
          p.correctedY,
          p.imageX,
          p.imageY,
          MarkPlace.of(p.placement),
          p.degrees
        )
      }
