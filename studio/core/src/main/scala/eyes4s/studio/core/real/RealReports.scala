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
import eyes4s.codec.ReportSources
import eyes4s.plan.AttributeValue
import eyes4s.results.{Covariate, CovariateName, CovariateSchema, CovariateType, Levels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingFilter, ReportingSpec}
import eyes4s.studio.core.reports.{ReportEvaluation, ReportRefusal}

/** Reports over a retained eyes4s result, bound to its plan, input and ledger.
  * Categorical filter values come from declared inventory columns. Grouped
  * reports remain refused until contrast operands or ordered levels are
  * recorded: observation order cannot establish a scientific direction.
  * The library reads, filters and weights every query.
  */
object RealReports:
  def evaluate(
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      held: RealStudyBackend.RealRun
  ): Either[BackendError, ReportView] =
    val p                            = held.prepared
    def refused(e: ReportRefusal)    = BackendError.ReportRefused(run, e)
    def sourceError(message: String) = ReportRefusal.Source(reporting.id.value, message)
    val requested = reporting.filters.collect { case ReportingFilter.Keep(attribute, _) =>
      attribute.label
    }.distinct
    for
      _ <- Either.cond(
        scale >= 0 && scale < held.result.scales.size,
        (),
        BackendError.UnknownScale(run, scale, p.summary.scales)
      )
      _ <- reporting.groupBy.fold[Either[BackendError, Unit]](Right(())) { attribute =>
        Left(
          refused(
            ReportRefusal.Spec(
              reporting.id.value,
              s"Grouping covariate ${attribute.label} has no recorded ordered levels or contrast operands."
            )
          )
        )
      }
      declarations <- requested
        .traverse { name =>
          for
            inventory <- p.admitted.evidence.inventory.toRight(
              sourceError(s"Categorical filter $name needs the admitted trial inventory.")
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
      source <- ReportSources
        .study(p.plans, p.inputs, p.results)(
          p.plan,
          p.admitted.input,
          held.result,
          Some(p.admitted.evidence),
          schema
        )
        .leftMap(e => refused(sourceError(e.message)))
      levels = declarations.collect { case Covariate(name, CovariateType.Categorical(found)) =>
        name.value -> found.values
      }.toMap
      view <- ReportEvaluation.evaluate(run, reporting, scale, levels, source).leftMap(refused)
    yield view
