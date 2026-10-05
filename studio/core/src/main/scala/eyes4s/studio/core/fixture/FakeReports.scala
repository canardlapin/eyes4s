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
import eyes4s.codec.ReportSources
import eyes4s.plan.{AttributeValue, Attributes, DiagnosticCode}
import eyes4s.results.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.ReportingSpec
import eyes4s.studio.core.reports.{ReportEvaluation, ReportRefusal}

/** The fake's reports (protocol 1.11): every reporting spec is evaluated by
  * eyes4s-results (`Report.evaluate`) over run 7's stored rows as the
  * fixture holds them, a query table per scale. The fake computes nothing
  * itself: the rows are the fixture's M, B and D, the response covariate is
  * the fixture's, and each query's outside-window share is eyes4s's
  * `WindowTally` of the fixations the trial view serves.
  */
private[fixture] object FakeReports:

  /** The categorical covariates the fixture declares, with their levels. */
  val Levels: Map[String, Vector[String]] =
    Map(MockStudy.GroupAttribute -> Vector(Response.Remembered.label, Response.Forgotten.label))

  private given Ordering[TrialKey] =
    Ordering.by(k => (k.participant, k.phase.label, k.trial, k.occurrence))

  /** The outside-window share of a query trial's fixations, as eyes4s tallies it. */
  private def share(moment: StoryMoment, key: TrialKey): Value[Double] =
    FakeTrialViews
      .tally(moment, AnalysisRevision(4), DatasetRevision(3), key)
      .fold(Value.Missing(Absence.NotRecorded))(t =>
        t.outsideWindowShare
          .fold(Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration)))(
            Value.Present(_)
          )
      )

  /** A failed query's stored outcome: its diagnostic's code, refused when
    * it is not one.
    */
  private def failed(
      reporting: String,
      key: TrialKey,
      d: StudioDiagnostic
  ): Either[ReportRefusal, RoleOutcome] =
    val (family, name) = d.code.span(_ != '.')
    DiagnosticCode
      .host(family, name.drop(1))
      .bimap(
        _ => ReportRefusal.UnreadableFailure(reporting, key, d.code),
        RoleOutcome.Failed(_, d.message)
      )

  /** Run 7's query tables, one per scale: every focal query the run tried,
    * with its stored outcomes, bound by eyes4s-codec. Queries the run did
    * not admit are not focal. `reporting` names the spec a refusal is for.
    */
  def source(
      reporting: String,
      moment: StoryMoment,
      queries: Vector[(MockQuery, QueryStatus)],
      scales: Int
  ): Either[ReportRefusal, ReportSource[TrialKey]] =
    def refuse(why: String) = ReportRefusal.Source(reporting, why)
    val focal  = queries.filterNot((_, status) => status.isInstanceOf[QueryStatus.NotAdmitted])
    val shares = focal.map((q, _) => q.key -> share(moment, q.key)).toMap
    for
      name   <- CovariateName.of(MockStudy.GroupAttribute).leftMap(e => refuse(e.message))
      levels <- eyes4s.results.Levels
        .of(Levels(MockStudy.GroupAttribute))
        .leftMap(e => refuse(e.message))
      schema <- CovariateSchema
        .of(Vector(Covariate(name, CovariateType.Categorical(levels))))
        .leftMap(e => refuse(e.message))
      outcomes <- focal.traverse { (q, status) =>
        status match
          case QueryStatus.Failed(diag) => failed(reporting, q.key, diag).map(f => q -> Left(f))
          case QueryStatus.Contributing(m, b, d) => Right(q -> Right((m, b, d)))
          case _                                 => Right(q -> Left(RoleOutcome.NotStored))
      }
      tables <- (0 until scales).toVector.traverse { s =>
        outcomes
          .traverse { (q, stored) =>
            val (m, b, d) = stored match
              case Left(outcome)    => (outcome, outcome, outcome)
              case Right((m, b, d)) =>
                def at(v: Vector[Double]) =
                  v.lift(s).fold(RoleOutcome.NotStored)(x => RoleOutcome.Scored(Vector(x)))
                (at(m), at(b), at(d))
            Query.of(
              q.key,
              q.participant,
              q.item,
              q.key.phase.label,
              q.key.occurrence,
              Vector(name -> Value.Present(CovariateValue.Level(q.response.label))),
              Vector.empty,
              Vector(WindowMeasure.OutsideWindowShare -> shares(q.key)),
              m,
              b,
              d
            )
          }
          .flatMap(QueryTable.of(s, Vector(ReportEvaluation.Component), schema, _))
          .leftMap(e => refuse(e.message))
      }
      entries <- focal.traverse((q, _) =>
        Attributes
          .of(Vector(MockStudy.GroupAttribute -> AttributeValue.Text(q.response.label)))
          .bimap(e => refuse(e.message), q.key -> _)
      )
      covariates <- CovariateTable.of(schema, entries).leftMap(e => refuse(e.message))
      bound      <- ReportSources
        .tables(io.circe.Encoder[TrialKey])(tables, Some(covariates))
        .leftMap(e => refuse(e.message))
    yield bound

  def report(
      moment: StoryMoment,
      run: RunId,
      reporting: ReportingSpec,
      scale: Int,
      queries: Vector[(MockQuery, QueryStatus)],
      scales: Vector[String]
  ): Either[BackendError, ReportView] =
    if !scales.indices.contains(scale) then Left(BackendError.UnknownScale(run, scale, scales))
    else
      source(reporting.id.value, moment, queries, scales.size)
        .flatMap(
          ReportEvaluation
            .evaluate(run, reporting, scale, Levels, _)
        )
        .leftMap(BackendError.ReportRefused(run, _))
