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
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
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
  * story (`StoryMoments`), whose recipe and the dataset's geometry state the
  * study.
  *
  * Every classification and estimate is eyes4s's own: placement goes through
  * the kernel's screen frame and half-open window ([[Subframe.locate]]), and
  * the preview through eyes4s-surface's Gaussian smoother ([[Smoother]]) over
  * the recipe's grid on the window, with highest-density isoline levels from
  * [[MassLevels]]. The fake writes no density arithmetic.
  *
  * Supported studies: every initial fixation kept (the story's recipes), any
  * off-window policy and weighting. Another initial-fixation choice is
  * `Unavailable` here; the real backend (S3.7) applies eyes4s's rule.
  */
object FakeTrialViews:

  /** The preview's bandwidth, in degrees of visual angle. */
  val PreviewSigmaDegrees: Double = 2.0

  /** The highest-density coverages the preview's isolines enclose. */
  val PreviewCoverages: Vector[Double] = Vector(0.5, 0.8)

  /** The recipe of `revision` in the story, and the dataset's geometry. */
  private def study(revision: AnalysisRevision): Either[BackendError, (Recipe, Geometry)] =
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Revision(revision))
    for
      doc  <- StoryMoments.t3.leftMap(_ => unavailable)
      spec <- doc.analysis(revision).toRight(unavailable)
      data <- doc.dataset(spec.dataset).toRight(unavailable)
      _    <- Either.cond(
        spec.recipe.initialFixations == InitialFixationChoice.KeepAll,
        (),
        unavailable
      )
    yield (spec.recipe, data.geometry)

  private def policy(recipe: Recipe): OffWindowPolicy = recipe.offWindow match
    case Some(OffWindowChoice.FailTrial) => OffWindowPolicy.FailTrial
    case _                               => OffWindowPolicy.Exclude

  /** The screen frame and the analysis window (the whole screen when the
    * recipe states none).
    */
  private def frames(
      recipe: Recipe,
      geometry: Geometry
  ): Either[GeometryError, (Frame[Unit2D.Px], Subframe[Unit2D.Px])] =
    for
      screen <- Frame.screen("screen", geometry.screen.width, geometry.screen.height)
      region <- recipe.window.fold(Right(screen.bounds))(w =>
        Bounds.of[Unit2D.Px](w.xMin, w.yMin, w.xMax, w.yMax)
      )
      window <- Subframe.of(screen, FrameId("window"), region)
    yield (screen, window)

  /** Where the study places a fixation centred at `(x, y)`: eyes4s's order,
    * with no initial fixation dropped.
    */
  private def placement(
      screen: Frame[Unit2D.Px],
      window: Subframe[Unit2D.Px],
      offWindow: OffWindowPolicy,
      x: Double,
      y: Double
  ): MapPlacement =
    val centre = Pt[Unit2D.Px](x, y)
    if !screen.bounds.contains(centre) then MapPlacement.OutsideScreen
    else if !window.locate(centre).isInside then MapPlacement.OutsideWindow(offWindow)
    else MapPlacement.InMap

  private def golden(trial: TrialKey): Either[BackendError, Vector[GoldenFixation]] =
    GoldenFixations.byTrial
      .leftMap(_ => BackendError.Unavailable(DiagnosticLocus.Trial(trial)))
      .flatMap(_.get(trial).toRight(BackendError.Unavailable(DiagnosticLocus.Trial(trial))))

  /** `trial`'s admitted fixations under `revision`, on `dataset`. */
  def fixations(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, TrialFixations] =
    val defect = BackendError.Unavailable(DiagnosticLocus.Trial(trial))
    for
      (recipe, geometry) <- study(revision)
      (screen, window)   <- frames(recipe, geometry).leftMap(_ => defect)
      records            <- golden(trial)
      fixations          <- records.zipWithIndex.traverse { (g, i) =>
        FixationIndex
          .of(i + 1)
          .leftMap(_ => defect)
          .flatMap(index =>
            AdmittedFixation
              .of(
                StudioRef.Fixation(trial, index),
                g.record,
                g.x,
                g.y,
                g.onsetMs,
                g.durationMs,
                placement(screen, window, policy(recipe), g.x, g.y)
              )
              .leftMap(_ => defect)
          )
      }
      view <- TrialFixations.of(revision, dataset, trial, fixations).leftMap(_ => defect)
    yield view

  /** eyes4s's σ 2° density of `trial`'s in-map fixations under `revision`,
    * over the recipe's grid on the window, with its isoline levels.
    */
  def preview(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey
  ): Either[BackendError, TrialPreview] =
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Trial(trial))
    for
      (recipe, geometry) <- study(revision)
      view               <- fixations(revision, dataset, trial)
      inMap = view.fixations.filter(_.placement == MapPlacement.InMap)
      _                <- Either.cond(inMap.nonEmpty, (), unavailable)
      (screen, window) <- frames(recipe, geometry).leftMap(_ => unavailable)
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
      mass <- (for
        grid  <- Grid.of(GridId("preview"), window.frame, recipe.grid.columns, recipe.grid.rows)
        scale <- LinearAngularScale.of(screen, perDegree)
        sigma <- Sigma.deg(PreviewSigmaDegrees).flatMap(scale.sigma)
      yield (grid, sigma)).leftMap(_ => unavailable).flatMap { (grid, sigma) =>
        PointMeasure
          .of(window.frame, IArray.from(located.map(_._1)), IArray.from(located.map(_._2)))
          .leftMap(_ => unavailable)
          .flatMap(m =>
            Smoother
              .gaussian(sigma, EdgePolicy.Truncate)
              .density(m, grid)
              .leftMap(_ => unavailable)
          )
      }
      levels  <- MassLevels.of(mass, PreviewCoverages).leftMap(_ => unavailable)
      preview <- TrialPreview
        .of(
          revision,
          trial,
          PreviewSigmaDegrees,
          recipe.grid.columns,
          recipe.grid.rows,
          // The window's frame runs y down from its top edge, so row 0 is the top.
          RowOrder.TopFirst,
          mass.values.toVector.map(Some(_)),
          levels.map(_.threshold)
        )
        .leftMap(_ => unavailable)
    yield preview
