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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.plan.{AttributeValue, TrialKey as CoreKey}
import eyes4s.results.{
  Covariate,
  CovariateName,
  CovariateSchema,
  CovariateType,
  Levels,
  Report,
  ReportSource
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingFilter, ReportingSpec}
import eyes4s.studio.core.reports.{ReportEvaluation, ReportRefusal}

/** Reports over a retained eyes4s result, bound to its plan, input and ledger.
  * Categorical values come from declared inventory columns. Grouping serves
  * cells; within-participant subtraction uses only explicitly saved operands.
  * Observation order cannot establish a scientific direction.
  * The library reads, filters and weights every query.
  */
object RealReports:
  private def bound(
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      held: RealStudyBackend.RealRun
  ): Either[BackendError, (ReportSource[CoreKey], Map[String, Vector[String]])] =
    val p                            = held.prepared
    def refused(e: ReportRefusal)    = BackendError.ReportRefused(run, e)
    def sourceError(message: String) = ReportRefusal.Source(reporting.id.value, message)
    val requested                    = reporting.filters
      .collect { case ReportingFilter.Keep(attribute, _) =>
        attribute.label
      }
      .concat(reporting.groupBy.toVector.map(_.label))
      .distinct
    for
      _ <- Either.cond(
        scale >= 0 && scale < held.result.scales.size,
        (),
        BackendError.UnknownScale(run, scale, p.summary.scales)
      )
      declarations <- requested
        .traverse { name =>
          for
            inventory <- p.admitted.evidence.inventory.toRight(
              sourceError(s"Categorical covariate $name needs the admitted trial inventory.")
            )
            _ <- Either.cond(
              inventory.attributeColumns.exists(_.name == name),
              (),
              ReportRefusal.UndeclaredCovariate(
                reporting.id.value,
                name,
                inventory.attributeColumns.map(_.name)
              )
            )
            values = inventory.trials
              .flatMap(_.attributes.get(name))
              .collect {
                case AttributeValue.Text(value) if value.trim.nonEmpty => value
                case AttributeValue.Integer(value)                     => value.toString
              }
              .distinct
              .sorted
            covariate <- CovariateName.of(name).leftMap(e => sourceError(e.message))
            levels    <- Levels.of(values).leftMap(e => sourceError(s"$name: ${e.message}"))
          yield Covariate(covariate, CovariateType.Categorical(levels))
        }
        .leftMap(refused)
      schema <- CovariateSchema.of(declarations).leftMap(e => refused(sourceError(e.message)))
      source <- held
        .reportSource(schema)
        .leftMap(e => refused(sourceError(e.message)))
      levels = declarations.collect { case Covariate(name, CovariateType.Categorical(found)) =>
        name.value -> found.values
      }.toMap
    yield (source, levels)

  /** Native report membership for navigation, sharing the exact retained
    * source and checked specification used by the served view.
    */
  def native(
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      held: RealStudyBackend.RealRun
  ): Either[BackendError, Report[CoreKey]] =
    for
      (source, levels) <- bound(run, reporting, scale, held)
      spec             <- ReportEvaluation
        .spec(reporting, scale, levels)
        .leftMap(BackendError.ReportRefused(run, _))
      report <- Report
        .evaluate(spec, source)
        .leftMap(e =>
          BackendError
            .ReportRefused(run, ReportRefusal.Evaluation(reporting.id.value, e.message))
        )
    yield report

  /** Serve a view and retain the exact report behind its membership in one evaluation. */
  def evaluateWithNative(
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      held: RealStudyBackend.RealRun
  ): Either[BackendError, (ReportView, Report[CoreKey])] =
    bound(run, reporting, scale, held).flatMap { (source, levels) =>
      ReportEvaluation
        .evaluateWithNative(run, reporting, scale, levels, source)
        .leftMap(BackendError.ReportRefused(run, _))
    }

  def evaluate(
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      held: RealStudyBackend.RealRun
  ): Either[BackendError, ReportView] =
    evaluateWithNative(run, reporting, scale, held).map(_._1)
