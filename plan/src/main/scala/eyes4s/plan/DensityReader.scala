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

package eyes4s.plan

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.surface.*

/** Estimate one trial under a plan's admission, initial-fixation, window and
  * weighting policies, without constructing pair schedules or running comparisons.
  * The caller chooses the estimation scale explicitly; archive readers bind it
  * to the saved scale and verify the returned map's canonical identity.
  */
object StudyDensity:
  def estimate[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      estimate: StudyEstimate[U],
      key: K,
      path: Scanpath[U]
  ): Either[StudyFailure[K], Mass[U]] =
    for
      _ <- Agreement
        .frames(plan.geometry.admission, path.frame)
        .left
        .map(StudyFailure.Frame(key, _))
      kept      <- plan.initialFixationRule.kept(key, path)
      _         <- StudyWindowing.check(plan.geometry, key, kept)
      occupancy <- kept.occupancy(plan.weight).left.map(StudyFailure.Occupancy(key, _))
      mass      <- fromOccupancy(plan.geometry, estimate, key, occupancy)
    yield mass

  private[plan] def fromOccupancy[K, U <: Unit2D](
      geometry: StudyGeometry[U],
      estimate: StudyEstimate[U],
      key: K,
      whole: PointMeasure[U]
  ): Either[StudyFailure[K], Mass[U]] =
    val grid = geometry.grid
    for
      occupancy <- StudyWindowing.restrict(geometry, key, whole)
      mass      <- estimate match
        case StudyEstimate.Anisotropic(x, y, edges) =>
          Smoother
            .anisotropic(x, y, edges)
            .density(occupancy, grid)
            .left
            .map(StudyFailure.Estimation(key, _))
        case StudyEstimate.Binned() =>
          for
            cells     <- occupancy.binned(grid).left.map(StudyFailure.Frame(key, _))
            intensity <- Surface
              .intensity(grid, cells, occupancy.provenance)
              .left
              .map(StudyFailure.Occupancy(key, _))
            result <- intensity.normalised.left.map(StudyFailure.Occupancy(key, _))
          yield result
        case StudyEstimate.Gaussian(sigma, edges) =>
          Smoother
            .gaussian(sigma, edges)
            .density(occupancy, grid)
            .left
            .map(StudyFailure.Estimation(key, _))
    yield mass
