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

import eyes4s.studio.core.backend.Response
import io.circe.{ACursor, Decoder, Json}

/** The mock study's science values are the golden fixture's (DESIGN_SPEC §15:
  * fixtures/studio-golden's SCORES.json, eyes4s's own output, is authoritative
  * for science values). make_fixture.py copies them; this suite fails when
  * fixture.json drifts from SCORES.json: every query's M, B and D at every
  * scale, the focus query's control scores, each participant's means overall
  * and by response group, and the grand means. Both sides are the same
  * decimals, so they are compared exactly.
  */
class MockScoresGoldenJvmSuite extends munit.FunSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val mock   = get(MockStudy.load)
  private val golden = get(io.circe.parser.parse(GoldenScores.text)).hcursor

  /** SCORES.json's scale keys, in the mock's scale order (0.5°, 1°, 2°, 4°). */
  private val scales = Vector("0.5", "1", "2", "4")

  /** The scale of the mock's single-scale summaries (σ 2°). */
  private val focusScale = "2"

  private def at[A: Decoder](c: ACursor, field: String): A = get(c.get[A](field))

  test("the mock and the golden fixture use the same scales") {
    assertEquals(mock.summary.scales, Vector("0.5°", "1°", "2°", "4°"))
    assertEquals(golden.downField("summaries").keys.map(_.toVector.sorted), Some(scales.sorted))
  }

  test("every query's M, B and D at every scale is SCORES.json's") {
    val byKey = at[Vector[Json]](golden, "queries").map { q =>
      val h = q.hcursor
      (at[String](h, "participant"), at[String](h, "trial")) -> h
    }.toMap
    val scored = mock.queries.filter(_.m.nonEmpty)
    assertEquals(scored.size, 454)
    for q <- mock.queries do
      byKey.get((q.participant, q.trial)) match
        case None =>
          assertEquals(q.status, "query not admitted", s"${q.participant} ${q.trial}")
        case Some(g) =>
          def role(r: String) =
            val values =
              scales.map(s => at[Option[Double]](g.downField("scales").downField(s), r))
            Option.when(values.forall(_.nonEmpty))(values.flatten)
          assertEquals(
            (q.m, q.b, q.d),
            (role("M"), role("B"), role("D")),
            s"${q.participant} ${q.trial}"
          )
  }

  test("the focus query's control scores at 2° are SCORES.json's control pairs") {
    val focus = golden.downField("focusQuery")
    val q     = mock.queries
      .find(q =>
        q.participant == at[String](focus, "participant") &&
          q.trial == at[String](focus, "trial")
      )
      .getOrElse(fail("no focus query in the mock"))
    val pairs = at[Vector[Json]](focus.downField("controlPairs"), focusScale).map { p =>
      val h = p.hcursor
      ControlScore(at[String](h, "reference"), at[String](h, "item"), at[Double](h, "score"))
    }
    assertEquals(pairs.size, 19)
    assertEquals(q.controlScores2deg.sortBy(_.trial), pairs.sortBy(_.trial))
    // Ranked highest first, as the boards list them.
    assertEquals(q.controlScores2deg, pairs.sortBy(-_.score))
  }

  test("each participant's means, overall and by response, are SCORES.json's") {
    val summaries = golden.downField("summaries")
    assertEquals(mock.summary.participants.size, 24)
    for p <- mock.summary.participants do
      def of(s: String) =
        summaries.downField(s).downField("participants").downField(p.participant)
      val two = of(focusScale)
      assertEquals(
        p.all,
        ScoreMeans(
          at[Double](two, "M"),
          at[Double](two, "B"),
          at[Double](two, "D"),
          scales.map(s => at[Double](of(s), "D"))
        ),
        p.participant
      )
      assertEquals(p.contributing, at[Int](two, "queries"), p.participant)
      assertEquals(p.groups.map(_.label), Vector(Response.Remembered, Response.Forgotten))
      for g <- p.groups do
        val c = two.downField("byResponse").downField(g.label.label)
        assertEquals(
          g,
          GroupMeans(
            g.label,
            at[Int](c, "queries"),
            at[Double](c, "M"),
            at[Double](c, "B"),
            at[Double](c, "D")
          ),
          s"${p.participant} ${g.label.label}"
        )
  }

  test("the grand means, overall and by response, are SCORES.json's") {
    val summaries        = golden.downField("summaries")
    def grand(s: String) =
      at[Double](summaries.downField(s).downField("grand").downField("D"), "estimate")
    def group(s: String, g: Response) =
      summaries.downField(s).downField("byResponse").downField(g.label)
    assertEquals(mock.summary.grandD, grand(focusScale))
    assertEquals(mock.summary.grandDByScale, scales.map(grand))
    assertEquals(
      mock.summary.groups.map(_.label),
      Vector(Response.Remembered, Response.Forgotten)
    )
    for g <- mock.summary.groups do
      assertEquals(g.d, at[Double](group(focusScale, g.label), "estimate"), g.label.label)
      assertEquals(
        g.dByScale,
        scales.map(s => at[Double](group(s, g.label), "estimate")),
        g.label.label
      )
      assertEquals(g.n, at[Int](group(focusScale, g.label), "participants"), g.label.label)
    assertEquals(
      mock.summary.pairedN,
      at[Int](summaries.downField(focusScale).downField("rememberedMinusForgotten"), "paired")
    )
  }
