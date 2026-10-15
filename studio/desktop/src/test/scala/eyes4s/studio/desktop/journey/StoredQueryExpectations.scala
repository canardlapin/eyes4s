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

/** Independent decimal arithmetic over the stored query rows, which are
  * SCORES.json's (make_fixture.py copies them).
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

  /** fixture.json's participant summary (SCORES.json's means), at
    * FIXTURE.md's two places, half up, independently decoded from fixture.json.
    */
  val fixtureParticipants: Vector[FixtureRow] = study.summary.participants.map { p =>
    def two(v: Double)         = BigDecimal(v).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    def group(label: Response) =
      val g = p.groups
        .find(_.label == label)
        .getOrElse(throw new AssertionError(s"Missing $label for ${p.participant}"))
      (two(g.d), g.n)
    FixtureRow(
      p.participant,
      p.requested,
      p.contributing,
      p.failed,
      p.noMatch,
      p.notAdmitted,
      two(p.all.m),
      two(p.all.b),
      two(p.all.d),
      group(Response.Remembered),
      group(Response.Forgotten)
    )
  }
