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

import eyes4s.studio.core.assets.AssetFile
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

/** What a representative trial's image frame showed, as its displays say
  * (DESIGN_SPEC section 9): an asset that is missing or unreadable is drawn
  * hatched with an X, never as the plain frame.
  */
enum FrameState derives CanEqual:
  /** No asset of the trial is missing: the frame is drawn plain (thumbnails
    * draw no image art).
    */
  case Present

  /** The trial's display names `file`, which the project does not hold. */
  case Missing(file: AssetFile)

  /** The project holds `file` but it cannot be read as an image. */
  case Unreadable(file: AssetFile, reason: String)

/** One representative trial: its records, how many eyes4s places outside
  * the image frame and outside the screen, and what its frame showed.
  */
final case class ThumbnailPicture(
    trial: TrialKey,
    records: Int,
    outsideWindow: Int,
    outsideScreen: Int,
    marks: Vector[MarkPicture],
    frame: FrameState
) derives CanEqual:
  def outside: Int = outsideWindow + outsideScreen

/** The four density levels of one dataset revision (DESIGN_SPEC section
  * 15.1): they start at 1, `step`, `4 × step` and `16 × step` records per
  * cell, and the last is the overflow level, with no upper bound. `step` is
  * a power of 4, at least 4, so a cell of one record is always at level 1
  * and never shares a level with a cell of `step` records or more.
  */
final case class DensitySteps private (step: Long) derives CanEqual:

  /** The first count of each level, lowest first. */
  def starts: Vector[Long] = Vector(1L, step, 4L * step, 16L * step)

  /** The level of a cell of `count` records: 0 (not drawn) for an empty
    * cell, else the highest level whose start it reaches.
    */
  def level(count: Long): Int =
    if count <= 0L then 0 else starts.lastIndexWhere(_ <= count) + 1

object DensitySteps:

  /** The smallest step, and the step of a study with no record on screen. */
  val Default: DensitySteps = DensitySteps(4L)

  /** The step anchored at `median`, the median count of the non-empty
    * cells: the largest `4^j` (`j ≥ 1`) whose half is at most `median`,
    * else 4. A tie (`median` = half the step) takes the larger step.
    */
  def anchored(median: Long): DensitySteps =
    // The next step 4s has half 2s; 2s ≤ median is s ≤ median / 2 in whole
    // numbers. The cap keeps 16 × step, the overflow start, a Long.
    @annotation.tailrec
    def grow(step: Long): Long =
      if step <= median / 2L && step <= Long.MaxValue / 64L then grow(4L * step) else step
    DensitySteps(grow(Default.step))

  /** The steps of `counts`, one whole count per cell: anchored at the
    * median of the non-empty cells (the lower middle one of an even number),
    * or [[Default]] when every cell is empty.
    */
  def of(counts: Vector[Long]): DensitySteps =
    val filled = counts.filter(_ > 0L).sorted
    if filled.isEmpty then Default else anchored(filled((filled.size - 1) / 2))

/** eyes4s's binned counts of every placed record as ramp levels (DESIGN_SPEC
  * section 15.1): every cell holding a record is drawn, at the level its
  * whole count reaches under `steps`; an empty cell is 0 and not drawn. The
  * levels are the four map-ramp stops (section 6). `steps` is computed once
  * from the whole grid of the shown revision, never from a selection or a
  * view, so the cells, the key and the semantics text read the same
  * numbers. Row 0 is the top row.
  */
final case class DensityPicture(
    columns: Int,
    rows: Int,
    levels: Vector[Int],
    records: Int,
    steps: DensitySteps
) derives CanEqual

object DensityPicture:
  val Levels: Int = 4

  /** A cell's whole number of records: the backend's count rounded half up,
    * and at least 1 when the count is above zero.
    */
  def count(binned: Double): Long =
    if binned <= 0.0 then 0L else math.max(1L, math.floor(binned + 0.5).toLong)

  def of(d: PlacementDensityGrid): DensityPicture =
    val counts = d.counts.map(count)
    val steps  = DensitySteps.of(counts)
    DensityPicture(d.columns, d.rows, counts.map(steps.level), d.placed, steps)

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

/** Everything the pictures depend on; equal keys draw equal pictures.
  * `frames` holds the trials whose frame is not [[FrameState.Present]], as
  * the revision's displays state them (none before they are read).
  */
final case class PicturesKey(
    dataset: DatasetRevision,
    geometry: Geometry,
    admission: AdmissionChoice,
    placement: PlacementKey,
    marked: Option[TrialKey],
    focusTrial: Option[TrialKey],
    focusRecord: Option[Int],
    frames: Map[TrialKey, FrameState]
) derives CanEqual:
  def frameOf(trial: TrialKey): FrameState = frames.getOrElse(trial, FrameState.Present)

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
    yield PicturesKey(
      spec.id,
      spec.geometry,
      spec.admission,
      key,
      panel.marked,
      trial,
      record,
      GeometryPanel.frames(panel, model)
    )

  /** The pictures of `spec`'s records as the backend placed them (eyes4s,
    * under the revision's geometry and recorded corrections): the
    * representative trials, the all-trials density and the worked example.
    * Nothing is placed here; the counts are the backend's trial tallies.
    *
    * The representative trials are, in order and without repeats: the
    * source's first trial; the selected trial; the marked trial; each trial
    * a trial-scoped rule corrects; the first trial in source order whose
    * frame is not present (the Data board's missing-asset thumbnail); then
    * the trials with the most records outside the image frame (ties in
    * source order), then the next trials in source order.
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
    val unshown = trials.find(t => key.frameOf(t) != FrameState.Present)
    val chosen  =
      (trials.take(1) ++ key.focusTrial ++ key.marked ++ ruled ++ unshown ++ worst ++ trials)
        .filter(byTrial.contains)
        .distinct
        .take(Thumbnails)
    val tallyOf = tallies.map(t => t.trial -> t).toMap
    GeometryPictures(
      key,
      FramePicture.of(spec.geometry),
      chosen.map(t => thumbnail(tallyOf(t), byTrial(t), key.frameOf(t))),
      DensityPicture.of(preview.density),
      exampleOf(key, preview),
      preview.unplaced.size
    )

  private def thumbnail(
      tally: eyes4s.studio.core.backend.TrialPlacement,
      placed: Vector[PlacedRecord],
      frame: FrameState
  ): ThumbnailPicture =
    ThumbnailPicture(
      tally.trial,
      tally.records,
      tally.outsideWindow,
      tally.outsideScreen,
      placed.map(p =>
        MarkPicture(p.correctedX, p.correctedY, MarkPlace.of(p.placement), p.rule.isDefined)
      ),
      frame
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
