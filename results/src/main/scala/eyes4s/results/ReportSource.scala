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

package eyes4s.results

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** What a report is evaluated over: a completed study result read through
  * its layout, bound to the plan, input, result and covariate source it
  * reads. The public way to obtain one is `eyes4s.codec.ReportSources.study`,
  * which computes the binding from those same values; the constructors
  * here take a binding on trust and are for the codec and law suites.
  * A source also carries the plan's per-trial window tallies, the trial
  * covariates and the method's score components.
  *
  * A source reads the stored rows of a result; it never reruns a pair score.
  * The eligible queries of a scale are its focal trials: every key of its
  * matched and control reductions and of its contrast, and every focal trial
  * the matched pairing left unmatched.
  */
final class ReportSource[K] private (
    val binding: ReportBinding,
    val covariates: CovariateSchema,
    val scales: Int,
    build: Int => Either[ReportError[K], QueryTable[K]]
)(using val ordering: Ordering[K]):
  /** The query table of scale `scale`. */
  def queries(scale: Int): Either[ReportError[K], QueryTable[K]] =
    if scale < 0 || scale >= scales then Left(ReportError.UnknownScale(scale, scales))
    else build(scale)

object ReportSource:
  /** A source over `result`, read through `layout`. `tallies` are the plan's
    * window tallies over the result's input (`StudyPlan.windowTallies`); a
    * trial without one has no window values. `covariates` joins by key.
    */
  private[eyes4s] def of[K, U <: Unit2D, S, D](
      result: StudyResult[K, U, S, D],
      layout: StudyLayout[K],
      tallies: Vector[(K, Either[GeometryError, WindowTally])],
      covariates: Option[CovariateTable[K]],
      scores: ScoreSchema[S, D],
      binding: ReportBinding
  ): ReportSource[K] =
    given Ordering[K] = layout.ordering
    val windows       = tallies.toMap
    val schema        = covariates.fold(CovariateSchema.empty)(_.schema)
    new ReportSource[K](
      binding,
      schema,
      result.scales.size,
      index => table(result.scales(index), index, layout, windows, covariates, schema, scores)
    )

  /** A source over query tables already read, one per scale in order: for a
    * host that caches them, and for law suites.
    */
  private[eyes4s] def fromQueries[K](tables: Vector[QueryTable[K]], binding: ReportBinding)(
      using Ordering[K]
  ): ReportSource[K] =
    val schema = tables.headOption.fold(CovariateSchema.empty)(_.covariates)
    new ReportSource[K](binding, schema, tables.size, index => Right(tables(index)))

  /** A source over the result `plan` computed on `input`: the result must
    * carry the plan's description and refer to the input, and the plan's
    * described score components name the method's contrast components.
    */
  private[eyes4s] def study[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      result: StudyResult[K, U, S, D],
      covariates: Option[CovariateTable[K]],
      binding: ReportBinding
  ): Either[ReportError[K], ReportSource[K]] =
    for
      _ <- Either.cond(
        result.description == plan.description,
        (),
        ReportError.PlanMismatch(
          PlanChange.between(plan.description, result.description).map(_.field)
        )
      )
      _ <- Either.cond(
        result.input == input.reference,
        (),
        ReportError.InputMismatch(result.input.digest, input.reference.digest)
      )
      scores <- ScoreSchema.study(plan).left.map(ReportError.Components.apply)
    yield of(result, plan.layout, plan.windowTallies(input), covariates, scores, binding)

  private def table[K, U <: Unit2D, S, D](
      scale: StudyScaleResult[K, U, S, D],
      index: Int,
      layout: StudyLayout[K],
      windows: Map[K, Either[GeometryError, WindowTally]],
      covariates: Option[CovariateTable[K]],
      schema: CovariateSchema,
      scores: ScoreSchema[S, D]
  )(using Ordering[K]): Either[ReportError[K], QueryTable[K]] =
    val matched  = scale.analyses.matched.entries.map(r => r.key -> r).toMap
    val control  = scale.analyses.control.entries.map(r => r.key -> r).toMap
    val contrast = scale.contrast.map(_.rows.map(r => r.key -> r).toMap)
    val focal    = (scale.analyses.matched.entries.map(_.key) ++
      scale.analyses.control.entries.map(_.key) ++
      scale.analyses.matchedSource.diagnostics.unmatchedLeft ++
      contrast.toOption.toVector.flatMap(_.keys)).distinct.sorted

    def failed[E](error: E)(using diagnose: Diagnose[E, K]): RoleOutcome =
      val d = diagnose(error)
      RoleOutcome.Failed(d.code, d.message)
    def reduced(row: Option[ReductionRow[K, S]]): RoleOutcome = row match
      case None    => RoleOutcome.NotStored
      case Some(r) =>
        r.result match
          case Right(s) => RoleOutcome.Scored(scores.score(s).components.map(_.value))
          case Left(e)  => failed(e)
    def difference(key: K): RoleOutcome = contrast match
      case Left(e)     => failed(e)
      case Right(rows) =>
        rows.get(key) match
          case None      => RoleOutcome.NotStored
          case Some(row) =>
            row.difference match
              case Right(d) => RoleOutcome.Scored(scores.difference(d).components.map(_.value))
              case Left(e)  => failed(e)
    def window(key: K): Vector[(WindowMeasure, Value[Double])] = windows.get(key) match
      case None          => Vector.empty
      case Some(Left(e)) =>
        val outcome = failed(e)(using summon[Diagnose[GeometryError, Nothing]])
        WindowMeasure.values.toVector.map(_ -> outcome.value(0))
      case Some(Right(t)) =>
        def share(v: Option[Double]) =
          v.fold(Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration)))(
            Value.Present(_)
          )
        Vector(
          WindowMeasure.OutsideWindowShare -> share(t.outsideWindowShare),
          WindowMeasure.OutsideScreenShare -> share(t.outsideScreenShare),
          WindowMeasure.OutsideWindowCount -> Value.Present(t.outsideWindow.toDouble),
          WindowMeasure.OutsideScreenCount -> Value.Present(t.outsideScreen.toDouble),
          WindowMeasure.FixationCount      -> Value.Present(t.total.toDouble)
        )

    val built = focal.foldLeft[Either[ReportError[K], Vector[Query[K]]]](Right(Vector.empty)) {
      (acc, key) =>
        acc.flatMap { queries =>
          val row = covariates.flatMap(_.row(key))
          Query
            .of(
              key,
              layout.participant(key),
              layout.stimulus(key),
              layout.phase(key),
              layout.occurrence.fold(1)(_(key).value),
              schema.names
                .map(n => n -> row.fold(Value.Missing(Absence.NotRecorded))(_.value(n))),
              row.fold(Vector.empty)(_.unparsed),
              window(key),
              reduced(matched.get(key)),
              reduced(control.get(key)),
              difference(key)
            )
            .map(queries :+ _)
        }
    }
    built.flatMap(QueryTable.of(index, scores.ids, schema, _))
