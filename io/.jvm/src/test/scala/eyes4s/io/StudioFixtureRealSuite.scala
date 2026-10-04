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

import io.circe.{Json, parser}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The Eyes Studio acceptance fixture through real eyes4s (ticket S0.7b):
  * every count FIXTURE.md states is recomputed by the library from
  * `fixtures/studio-golden`, `SCORES.json` regenerates byte for byte, and
  * the focus query P17 ret_07 × enc_03 is scored at all four scales.
  *
  * Where the library's count differs from FIXTURE.md the suite pins the
  * library's value and names FIXTURE.md's beside it; neither side is edited
  * to agree.
  */
class StudioFixtureRealSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private lazy val tables = StudioScores.tables
  private lazy val body   = StudioScores.body(tables)

  private lazy val stored: Json =
    parser
      .parse(Files.readString(StudioScores.scoresFile, StandardCharsets.UTF_8))
      .fold(e => fail(s"SCORES.json does not parse: $e"), identity)

  private def counts(j: Json): Map[String, Json] =
    j.hcursor.downField("counts").focus.flatMap(_.asObject).fold(Map.empty)(_.toMap)

  private def int(j: Json): Long =
    j.asNumber.flatMap(_.toLong).getOrElse(fail(s"not a count: $j"))

  /** FIXTURE.md's counts (and the golden README's), by SCORES.json name. */
  private val fixtureCounts: Map[String, Long] = Map(
    "inventoryTrials"                  -> 960,
    "admittedTrials"                   -> 937,
    "quarantinedTrials"                -> 17,
    "absentTrials"                     -> 6,
    "records"                          -> 11520,
    "admittedRecords"                  -> 11311,
    "rejectedRecords"                  -> 209,
    "recordsOutsideWindow"             -> 543,
    "trialsOutsideWindow"              -> 409,
    "recordsOutsideScreen"             -> 0,
    "items"                            -> 259,
    "requestedQueries"                 -> 480,
    "queriesNotAdmitted"               -> 14,
    "noMatchQueries"                   -> 9,
    "multipleMatchQueries"             -> 0,
    "failedQueries"                    -> 3,
    "contributingQueries"              -> 454,
    "eligibleQueries"                  -> 457,
    "matchedPairsPerScale"             -> 457,
    "pairRowsOfMatchedQueriesPerScale" -> 8969
  )

  /** Counts where the library and FIXTURE.md differ: (FIXTURE.md, eyes4s). */
  private val differences: Map[String, (Long, Long)] = Map(
    // FIXTURE.md: 480 retrieval x 480 encoding inventory trials; eyes4s counts
    // the admitted focal (466) x reference (471) trials.
    "candidatePairsPerScale" -> (230400L, 219486L),
    // FIXTURE.md counts the pair rows of the 457 matched queries only; eyes4s
    // also schedules the 171 control rows of the 9 queries without a match.
    "pairRowsPerScale"  -> (8969L, 9140L),
    "pairRowsAllScales" -> (35876L, 36560L),
    // eyes4s StudyCounts counts every focal query as eligible (466), the
    // 9 without a match included; FIXTURE.md's 457 excludes them.
    "studyCountsEligibleQueries" -> (457L, 466L)
  )

  test("every FIXTURE.md count reproduces through eyes4s") {
    val c = counts(body)
    fixtureCounts.toVector.sortBy(_._1).foreach { (name, expected) =>
      assertEquals(c.get(name).map(int), Some(expected), name)
    }
    assertEquals(
      c("quarantinedByCause"),
      Json.obj(
        "DuplicateOrdinals" -> Json.fromInt(4),
        "Overlap"           -> Json.fromInt(6),
        "RejectedRecords"   -> Json.fromInt(2),
        "no fixations"      -> Json.fromInt(5)
      )
    )
    // Controls: 19 per query, 18 where an encoding trial was not admitted.
    assertEquals(
      c("controlsPerQuery"),
      Json.obj("18" -> Json.fromInt(171), "19" -> Json.fromInt(286))
    )
    // 4 scales; 3 failed queries are P05's three off-image trials.
    val failed = queries(body).filter(q => str(q, "status").startsWith("failed"))
    assertEquals(
      failed.map(q => (str(q, "participant"), str(q, "trial"))).toSet,
      StudioFixture.offImage
    )
  }

  test("where eyes4s and FIXTURE.md count differently, eyes4s's count is pinned") {
    val c = counts(body)
    differences.toVector.sortBy(_._1).foreach { case (name, (fixture, library)) =>
      assertEquals(c.get(name).map(int), Some(library), s"$name (FIXTURE.md: $fixture)")
      assertNotEquals(fixture, library, name)
    }
  }

  test("SCORES.json regenerates byte for byte") {
    val generatedWith =
      stored.hcursor.downField("generatedWith").focus.getOrElse(fail("no generatedWith"))
    val regenerated = StudioScores.render(body, generatedWith)
    val file        = Files.readString(StudioScores.scoresFile, StandardCharsets.UTF_8)
    if regenerated != file then
      val at = regenerated.zip(file).indexWhere(_ != _)
      val i  = if at < 0 then math.min(regenerated.length, file.length) else at
      fail(
        s"SCORES.json differs from its regeneration at character $i: " +
          s"regenerated '${regenerated.slice(i - 60, i + 60)}' vs stored '${file.slice(i - 60, i + 60)}'"
      )
  }

  private def queries(j: Json): Vector[Json] =
    j.hcursor.downField("queries").focus.flatMap(_.asArray).getOrElse(fail("no queries"))
  private def str(j: Json, field: String): String =
    j.hcursor.downField(field).as[String].getOrElse(fail(s"no $field in $j"))

  test("the focus query P17 ret_07 x enc_03 is scored at all four scales") {
    val focus = queries(body)
      .find(q => str(q, "participant") == "P17" && str(q, "trial") == "ret_07")
      .getOrElse(fail("no P17 ret_07"))
    assertEquals(str(focus, "item"), "beach-042")
    assertEquals(str(focus, "response"), "Remembered")
    assertEquals(str(focus, "status"), "contributing")
    assertEquals(focus.hcursor.downField("matched").as[Vector[String]], Right(Vector("enc_03")))
    assertEquals(focus.hcursor.downField("controls").as[Int], Right(19))
    val scales = focus.hcursor.downField("scales")
    assertEquals(scales.keys.map(_.toVector), Some(Vector("0.5", "1", "2", "4")))
    Vector("0.5", "1", "2", "4").foreach { s =>
      val v = scales.downField(s)
      val m = v.downField("M").as[Double].getOrElse(fail(s"no M at $s"))
      val b = v.downField("B").as[Double].getOrElse(fail(s"no B at $s"))
      val d = v.downField("D").as[Double].getOrElse(fail(s"no D at $s"))
      // D is eyes4s's stored contrast, M minus B to the rounding of each.
      assert(math.abs(d - (m - b)) <= 1.5e-6, s"$s: D $d, M $m, B $b")
    }
    // The stored file holds the same query.
    assertEquals(
      queries(stored).find(q => str(q, "participant") == "P17" && str(q, "trial") == "ret_07"),
      Some(focus)
    )
  }

  test("the stored file names its inputs, recipe and the JVM it was generated on") {
    val c = stored.hcursor
    assertEquals(c.downField("format").as[String], Right(StudioScores.Format))
    assertEquals(c.downField("generator").as[String], Right(StudioScores.Generator))
    Vector("java.vendor", "java.version", "os.arch").foreach { k =>
      assert(c.downField("generatedWith").downField(k).as[String].isRight, k)
    }
    val sigmas = c.downField("recipe").downField("scales").as[Vector[Double]]
    assertEquals(sigmas, Right(StudioScores.Sigmas))
    assertEquals(
      c.downField("summaries").keys.map(_.toVector),
      Some(Vector("0.5", "1", "2", "4"))
    )
    // Every participant has a summary at every scale.
    Vector("0.5", "1", "2", "4").foreach { s =>
      assertEquals(
        c.downField("summaries").downField(s).downField("participants").keys.map(_.size),
        Some(24),
        s
      )
    }
  }
