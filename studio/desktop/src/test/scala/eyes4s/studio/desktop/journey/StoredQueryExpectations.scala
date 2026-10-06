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

package eyes4s.studio.desktop.journey

import eyes4s.studio.core.backend.{ReportRole, Response}
import eyes4s.studio.core.fixture.MockStudy

/** Independent decimal arithmetic over the stored illustrative query rows.
  * The old Python summaries rounded twice; those metadata pins remain intact.
  */
object StoredQueryExpectations:
  private val study     = MockStudy.load.fold(e => throw new AssertionError(e), identity)
  val Precision: Double = 1e-12

  def decimalMean(
      participant: String,
      role: ReportRole,
      scale: Int,
      group: Option[Response] = None
  ): BigDecimal =
    val values = study.queries
      .filter(q =>
        q.participant == participant && q.status == "ok" &&
          group.forall(_ == q.response)
      )
      .flatMap { q =>
        val scores = role match
          case ReportRole.Matched    => q.m
          case ReportRole.Control    => q.b
          case ReportRole.Difference => q.d
        scores.flatMap(_.lift(scale)).map(BigDecimal(_))
      }
    if values.isEmpty then
      throw new AssertionError(s"No stored values for $participant/$group/$role/$scale")
    values.sum / BigDecimal(values.size)

  def mean(
      participant: String,
      role: ReportRole,
      scale: Int,
      group: Option[Response] = None
  ): Double = decimalMean(participant, role, scale, group).toDouble

  def grand(role: ReportRole, scale: Int, group: Option[Response] = None): Double =
    val values =
      study.summary.participants.map(p => decimalMean(p.participant, role, scale, group))
    (values.sum / BigDecimal(values.size)).toDouble

  /** Python's frozen displayed metadata, independently decoded from fixture.json. */
  val legacyParticipants: Vector[FixtureRow] = study.summary.participants.map { p =>
    def group(label: Response) =
      val g = p.groups
        .find(_.label == label)
        .getOrElse(throw new AssertionError(s"Missing $label for ${p.participant}"))
      (BigDecimal(g.d), g.n)
    FixtureRow(
      p.participant,
      p.requested,
      p.contributing,
      p.failed,
      p.noMatch,
      p.notAdmitted,
      BigDecimal(p.all.m),
      BigDecimal(p.all.b),
      BigDecimal(p.all.d),
      group(Response.Remembered),
      group(Response.Forgotten)
    )
  }

  /** Exact decimal query means at the four binary rounding boundaries, and
    * the report's pinned half-up display after native floating accumulation.
    */
  val boundaries: Vector[(String, Response, BigDecimal, BigDecimal)] = Vector(
    ("P05", Response.Forgotten, BigDecimal("-0.235"), BigDecimal("-0.24")),
    ("P13", Response.Forgotten, BigDecimal("0.025"), BigDecimal("0.02")),
    ("P16", Response.Remembered, BigDecimal("0.445"), BigDecimal("0.44")),
    ("P24", Response.Forgotten, BigDecimal("0.175"), BigDecimal("0.18"))
  )

  val evaluatedParticipants: Vector[FixtureRow] = legacyParticipants.map { p =>
    boundaries.filter(_._1 == p.participant).foldLeft(p) { case (row, (_, group, _, shown)) =>
      if group == Response.Remembered then row.copy(remembered = (shown, row.remembered._2))
      else row.copy(forgotten = (shown, row.forgotten._2))
    }
  }
