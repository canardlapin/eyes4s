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

package eyes4s.studio.core.fixture

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.plan.{MapPlacement, OffWindowPolicy, WindowTally}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  Geometry,
  InitialFixationChoice,
  OffWindowChoice,
  Recipe,
  WeightChoice
}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.surface.{EdgePolicy, Smoother}

/** One fixation of fixtures/studio-golden as fixations.csv states it: its
  * data record (from 1), its centre in screen pixels, onset and duration.
  */
final case class GoldenFixation(
    record: Int,
    x: Double,
    y: Double,
    onsetMs: Double,
    durationMs: Double
) derives CanEqual

/** The admitted trials' fixations of fixtures/studio-golden, read at build
  * time (`GoldenInventory.fixations`), in scanpath (ordinal) order.
  */
object GoldenFixations:

  /** Every admitted trial's fixations, or the first line that cannot be read. */
  lazy val byTrial: Either[String, Map[TrialKey, Vector[GoldenFixation]]] =
    GoldenInventory.fixations.linesIterator.toVector
      .traverse { line =>
        line.split("\t", -1).toList match
          case p :: phase :: trial :: occurrence :: records :: Nil =>
            for
              occ <- occurrence.toIntOption.toRight(s"fixation line '$line': bad occurrence")
              fs  <- records.split(",").toVector.traverse(fixation(line, _))
            yield TrialKey(p, Phase(phase), trial, occ) -> fs
          case _ => Left(s"fixation line '$line' is not five fields")
      }
      .map(_.toMap)

  private def fixation(line: String, token: String): Either[String, GoldenFixation] =
    token.split("@").toList match
      case r :: x :: y :: onset :: duration :: Nil =>
        (
          r.toIntOption,
          x.toDoubleOption,
          y.toDoubleOption,
          onset.toDoubleOption,
          duration.toDoubleOption
        ).mapN(GoldenFixation.apply)
          .toRight(s"fixation line '$line': bad fixation '$token'")
      case _ => Left(s"fixation line '$line': bad fixation '$token'")

/** The fake backend's trial views (protocol 1.6, S6.2) over fixtures/studio-golden:
  * a trial's admitted fixations and its σ 2° preview, under a revision of the
  * fake's story moment, whose saved recipe and dataset geometry state the
  * study.
  *
  * Placement is decided by the kernel's screen frame and half-open window
  * ([[Subframe.locate]]) in eyes4s's order (outside the screen, outside the
  * window, in the map); the fixture has no correction rules, so the admitted
  * centre is the recorded one, and only `KeepAll` initial fixations are
  * served, so none is dropped. `TrialViewsJvmSuite` holds every placement to
  * eyes4s's own `CoordinateProvenance` on the fixture. The preview is
  * eyes4s-surface's Gaussian smoother ([[Smoother]]) over the recipe's grid
  * on the window, with highest-density isoline levels from [[MassLevels]];
  * the fake writes no density arithmetic. A study the fake cannot state is
  * `Unavailable`; every other refusal is a typed [[TrialViewError]].
  */
object FakeTrialViews:

  /** The preview's bandwidth, in degrees of visual angle. */
  val PreviewSigmaDegrees: Double = 2.0

  /** The highest-density coverages the preview's isolines enclose. */
  val PreviewCoverages: Vector[Double] = Vector(0.5, 0.8)

  private def refused(e: TrialViewError): BackendError = BackendError.TrialViewRefused(e)

  /** The saved recipe of `revision` at `moment`, and its dataset's geometry. */
  private[fixture] def study(
      moment: StoryMoment,
      revision: AnalysisRevision
  ): Either[BackendError, (Recipe, Geometry)] =
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Revision(revision))
    for
      doc  <- StorySeed.document(moment).leftMap(_ => unavailable)
      spec <- doc.analysis(revision).toRight(unavailable)
      data <- doc.dataset(spec.dataset).toRight(unavailable)
      _    <- Either.cond(
        spec.recipe.initialFixations == InitialFixationChoice.KeepAll,
        (),
        unavailable
      )
    yield (spec.recipe, data.geometry)

  /** The recipe's off-window choice as eyes4s's policy; a recipe without a
    * window has no off-window fixation, and eyes4s's default is `Exclude`.
    */
  private[fixture] def policy(recipe: Recipe): OffWindowPolicy = recipe.offWindow match
    case Some(OffWindowChoice.FailTrial) => OffWindowPolicy.FailTrial
    case _                               => OffWindowPolicy.Exclude

  /** The screen frame and the analysis window (the whole screen when the
    * recipe states none).
    */
  private def frames(
      trial: TrialKey,
      recipe: Recipe,
      geometry: Geometry
  ): Either[BackendError, (Frame[Unit2D.Px], Subframe[Unit2D.Px])] =
    (for
      screen <- Frame.screen("screen", geometry.screen.width, geometry.screen.height)
      region <- recipe.window.fold(Right(screen.bounds))(w =>
        Bounds.of[Unit2D.Px](w.xMin, w.yMin, w.xMax, w.yMax)
      )
      window <- Subframe.of(screen, FrameId("window"), region)
    yield (screen, window)).leftMap(e =>
      refused(TrialViewError.Study(trial, "window", e.message))
    )

  /** Where the study places a fixation centred at `(x, y)`, in eyes4s's
    * order, with no initial fixation dropped.
    */
  private[fixture] def placement(
      screen: Frame[Unit2D.Px],
      window: Subframe[Unit2D.Px],
      offWindow: OffWindowPolicy,
      x: Double,
      y: Double
  ): MapPlacement =
    val centre = Pt[Unit2D.Px](x, y)
    if !screen.bounds.contains(centre) then MapPlacement.OutsideScreen
    else if !window.locate(centre).isInside then MapPlacement.OutsideWindow(offWindow)
    else MapPlacement.InWindow

  /** As eyes4s marks a trial the study fails: under `FailTrial`, a trial with
    * any fixation outside the window fails, and its fixations in the window
    * are `TrialFailed` with the tally of the trial's fixations.
    */
  private[fixture] def trialPlacements(
      placed: Vector[(MapPlacement, Double)]
  ): Either[String, Vector[MapPlacement]] =
    val fails = placed.exists(_._1 == MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial))
    if !fails then Right(placed.map(_._1))
    else
      def micros(ms: Double)                   = Span.micros(math.round(ms * 1000.0))
      def duration(p: MapPlacement => Boolean) =
        placed.filter(f => p(f._1)).map(f => micros(f._2)).foldLeft(Span.zero)(_ + _)
      WindowTally
        .of(
          placed.count(_._1 == MapPlacement.OutsideScreen),
          placed.count(_._1.isInstanceOf[MapPlacement.OutsideWindow]),
          placed.size,
          duration(_ == MapPlacement.OutsideScreen),
          duration(_.isInstanceOf[MapPlacement.OutsideWindow]),
          duration(_ => true)
        )
        .bimap(
          _.message,
          tally =>
            placed.map(_._1).map {
              case MapPlacement.InWindow => MapPlacement.TrialFailed(tally)
              case other                 => other
            }
        )

  /** The trial's fixations in the fixture; an inventory trial without an
    * admitted scanpath has none to serve.
    */
  private def golden(trial: TrialKey): Either[BackendError, Vector[GoldenFixation]] =
    GoldenFixations.byTrial
      .leftMap(e => refused(TrialViewError.Study(trial, "fixation source", e)))
      .flatMap(_.get(trial).toRight(BackendError.Unavailable(DiagnosticLocus.Trial(trial))))

  /** `trial`'s admitted fixations under `revision`, on `dataset`. */
  def fixations(
      moment: StoryMoment,
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, TrialFixations] =
    study(moment, revision).flatMap((recipe, geometry) =>
      under(recipe, geometry, revision, dataset, trial)
    )

  /** `trial`'s admitted fixations under a stated study: `recipe` (its window
    * and off-window policy) on `geometry`.
    */
  private[fixture] def under(
      recipe: Recipe,
      geometry: Geometry,
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, TrialFixations] =
    for
      (screen, window) <- frames(trial, recipe, geometry)
      records          <- golden(trial)
      placements       <- trialPlacements(
        records.map(g => placement(screen, window, policy(recipe), g.x, g.y) -> g.durationMs)
      ).leftMap(e => refused(TrialViewError.Study(trial, "window tally", e)))
      fixations <- records.zipWithIndex.traverse { (g, i) =>
        FixationIndex
          .of(i + 1)
          .leftMap(e => refused(TrialViewError.Study(trial, "fixation position", e.message)))
          .flatMap(index =>
            AdmittedFixation
              .of(
                StudioRef.Fixation(trial, index),
                g.record,
                g.x,
                g.y,
                g.onsetMs,
                g.durationMs,
                placements(i)
              )
              .leftMap(refused)
          )
      }
      view <- TrialFixations.of(revision, dataset, trial, fixations).leftMap(refused)
    yield view

  /** eyes4s's σ 2° density of `trial`'s in-map fixations under `revision`,
    * over the recipe's grid on the window it covers, with its isoline levels.
    * A trial the study fails has none.
    */
  def preview(
      moment: StoryMoment,
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, TrialPreview] =
    def study(step: String)(reason: String) = refused(TrialViewError.Study(trial, step, reason))
    for
      (recipe, geometry) <- FakeTrialViews.study(moment, revision)
      view               <- fixations(moment, revision, dataset, trial)
      failing = view.fixations.collect {
        case f if f.placement == MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) =>
          f.ref.index.value
      }
      _ <- Either.cond(failing.isEmpty, (), refused(TrialViewError.TrialFails(trial, failing)))
      inMap = view.fixations.filter(_.placement == MapPlacement.InWindow)
      _ <- Either.cond(inMap.nonEmpty, (), study("preview")("no fixation lies in the map"))
      (screen, window) <- frames(trial, recipe, geometry)
      perDegree = recipe.angularScale.getOrElse(geometry.pixelsPerDegree).value
      // Each in-map fixation in the window's own frame, with its weight.
      located = inMap.flatMap(f =>
        window.locate(Pt[Unit2D.Px](f.screenX, f.screenY)) match
          case HalfOpenPlacement.Inside(local) =>
            val weight = recipe.weighting match
              case WeightChoice.Duration => f.durationMs
              case WeightChoice.Uniform  => 1.0
            Some(local -> weight)
          case HalfOpenPlacement.Outside(_) => None
      )
      grid <- Grid
        .of(GridId("preview"), window.frame, recipe.grid.columns, recipe.grid.rows)
        .leftMap(e => study("grid")(e.message))
      sigma <- LinearAngularScale
        .of(screen, perDegree)
        .flatMap(scale => Sigma.deg(PreviewSigmaDegrees).flatMap(scale.sigma))
        .leftMap(e => study("bandwidth")(e.message))
      measure <- PointMeasure
        .of(window.frame, IArray.from(located.map(_._1)), IArray.from(located.map(_._2)))
        .leftMap(e => study("fixation measure")(e.message))
      mass <- Smoother
        .gaussian(sigma, EdgePolicy.Truncate)
        .density(measure, grid)
        .leftMap(e => study("density")(e.message))
      levels <- MassLevels.of(mass, PreviewCoverages).leftMap(e => study("isolines")(e.message))
      covered = window.region
      region <- ScreenRegion
        .of(trial, covered.xMin, covered.yMin, covered.xMax, covered.yMax)
        .leftMap(refused)
      preview <- TrialPreview
        .of(
          revision,
          trial,
          PreviewSigmaDegrees,
          region,
          recipe.grid.columns,
          recipe.grid.rows,
          // The window's frame runs y down from its top edge, so row 0 is the top.
          RowOrder.TopFirst,
          mass.values.toVector.map(Some(_)),
          levels.map(_.threshold)
        )
        .leftMap(refused)
    yield preview
