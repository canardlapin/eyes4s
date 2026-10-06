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

package eyes4s.io

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

/** Analytic non-square surface fits and constant-residual associations. */
object ConstrainedExportFixtures:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => sys.error(e.toString), identity)
  val grid = get(Grid.over(get(Frame.angular("constrained-export", 12, 8)), 3, 2))
  def mass(values: Vector[Double]): Mass[Deg] =
    val raw = IArray.from(values)
    get(Surface.mass(grid, raw, Provenance.raw(ContentHash.of(raw))))
  val predictors = get(
    PredictorSet.of(
      Vector(
        get(PredictorId.of("background")) -> mass(Vector(1, 0, 0, 0, 0, 0)),
        get(PredictorId.of("item"))       -> mass(Vector(0, 1, 0, 0, 0, 0))
      )
    )
  )
  val response       = mass(Vector(7, 13, 1, 1, 1, 1).map(_ / 24.0))
  val nnls           = get(Template.decomposeNonNegative(response, predictors))
  val mixture        = get(Template.decomposeMixture(response, predictors, Intercept.Include))
  val noBackground   = get(Template.decomposeMixture(response, predictors, Intercept.Exclude))
  val uniform        = mass(Vector.fill(6)(1.0 / 6.0))
  val uniformMixture = get(Template.decomposeMixture(uniform, predictors, Intercept.Include))
  val x              = mass(Vector(1, 2, 3, 4, 5, 6).map(_ / 21.0))
  val covariates     = get(
    PredictorSet.of(
      Vector(
        get(PredictorId.of("alternating")) -> mass(Vector(1.0 / 3, 0, 1.0 / 3, 0, 1.0 / 3, 0))
      )
    )
  )
  val pearson   = get(PartialAssociation.of(x, x, covariates, AssociationMethod.Pearson))
  val spearman  = get(PartialAssociation.of(x, x, covariates, AssociationMethod.Spearman))
  val undefined = get(PartialAssociation.of(uniform, x, covariates, AssociationMethod.Pearson))
  def tables: Vector[ResultTable] =
    get(ResultExports.nnls(nnls)) ++ get(ResultExports.mixture(mixture)) ++
      get(ResultExports.mixture(noBackground)) ++ get(ResultExports.mixture(uniformMixture)) ++
      Vector(pearson, spearman, undefined).map(a => get(ResultExports.partialAssociation(a)))
