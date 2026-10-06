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

package eyes4s.studio.app.plot

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}
import scala.concurrent.{ExecutionContext, Future}

/** Synthetic report payloads for adapter tests, and native fake reports for
  * the illustrative fixture. Neither re-derives a scientific report.
  */
object PlotReports:
  val run: RunId             = RunId(7)
  val reporting: ReportingId = ReportingId.of("by-retrieval-response").toOption.get
  val overallId: ReportingId = ReportingId.of("by-retrieval-response-overall").toOption.get
  val spec: ReportingSpec    = StoryMoments.byResponse.toOption.get
  val overall: ReportingSpec = ReportingSpec
    .of(overallId, "Overall", None, Vector.empty, None, ReportingWeight.ParticipantMeans)
    .toOption
    .get
  val scales: ScaleSet =
    StoryMoments.t2.toOption.get.analysis(StoryMoments.rev4).get.recipe.scales

  def read(using
      ExecutionContext
  ): Future[(ResultSummary, Vector[ReportView], Vector[ReportView])] =
    HeadlessSession.open(StoryMoment.T2).flatMap { session =>
      (for
        summary <- session
          .result(run)
          .map(_.fold(e => throw new AssertionError(e.message), identity))
        grouped <- Future.sequence(
          summary.scales.indices.map(i =>
            session
              .report(run, spec, i)
              .map(_.fold(e => throw new AssertionError(e.message), identity))
          )
        )
        whole <- Future.sequence(
          summary.scales.indices.map(i =>
            session
              .report(run, overall, i)
              .map(_.fold(e => throw new AssertionError(e.message), identity))
          )
        )
      yield (summary, grouped.toVector, whole.toVector))
        .transformWith(result => session.close.transform(_ => result))
    }

  def synthetic(
      scale: Int = 2,
      group: Option[Response] = Some(Response.Remembered),
      estimate: Option[Double] = Some(0.2),
      values: Vector[(String, Option[Double], Int)] =
        Vector(("A", Some(0.0), 3), ("B", None, 0)),
      id: ReportingId = reporting
  ): ReportView =
    val at    = ScaleIndex.of(scale).toOption.get
    val locus = group.fold(ReportGroup.Whole)(ReportGroup.Level(_))
    val cell  = ReportCellView(
      group,
      ReportRole.Difference,
      estimate,
      Option.when(estimate.isEmpty)(ReportAbsence.EmptyGroup),
      1,
      3,
      0,
      StudioRef.ReportCell(run, id, at, locus, ReportRole.Difference)
    )
    val participants = values.map { (person, value, n) =>
      ReportParticipantView(
        group,
        ReportRole.Difference,
        person,
        n,
        value,
        Option.when(value.isEmpty)(ReportAbsence.EmptyGroup),
        StudioRef.ReportParticipant(run, id, at, locus, ReportRole.Difference, person)
      )
    }
    ReportView(
      run,
      id,
      scale,
      Vector(cell),
      participants,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )
