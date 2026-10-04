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
  * FIXTURE.md's scalar counts, its per-participant table and its group and
  * window figures are all checked; neither side is edited to agree. Out of
  * scope: images found 257 and missing 2 (asset files, not eyes4s).
  */
class StudioFixtureRealSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private lazy val tables = StudioScores.tables
  private lazy val study  = StudioScores.Study(tables.source)
  private lazy val body   = StudioScores.body(tables, study)

  private lazy val fixtureMd: String =
    Files.readString(
      StudioScores.directory.getParent.getParent.resolve("docs/studio/fixture/FIXTURE.md"),
      StandardCharsets.UTF_8
    )

  private lazy val stored: Json =
    parser
      .parse(Files.readString(StudioScores.scoresFile, StandardCharsets.UTF_8))
      .fold(e => fail(s"SCORES.json does not parse: $e"), identity)

  private def counts(j: Json): Map[String, Json] =
    j.hcursor.downField("counts").focus.flatMap(_.asObject).fold(Map.empty)(_.toMap)

  private def int(j: Json): Long =
    j.asNumber.flatMap(_.toLong).getOrElse(fail(s"not a count: $j"))

  /** FIXTURE.md's counts (and the golden README's), by SCORES.json name.
    * Since the owner's decision on bead S0.7b, a query without a match is not
    * eligible and has no control pairs, and candidates are counted over
    * admitted trials; every count agrees.
    */
  private val fixtureCounts: Map[String, Long] = Map(
    "inventoryTrials"      -> 960,
    "admittedTrials"       -> 937,
    "quarantinedTrials"    -> 17,
    "absentTrials"         -> 6,
    "records"              -> 11520,
    "admittedRecords"      -> 11311,
    "rejectedRecords"      -> 209,
    "recordsOutsideWindow" -> 543,
    "trialsOutsideWindow"  -> 409,
    "recordsOutsideScreen" -> 0,
    "items"                -> 259,
    "requestedQueries"     -> 480,
    "queriesNotAdmitted"   -> 14,
    "noMatchQueries"       -> 9,
    // Every no-match query's encoding trial is in trials.csv and was not
    // admitted: none is unmatched by design in this encoding-retrieval study.
    "byDesignQueries"             -> 0,
    "noMatchReferenceNotAdmitted" -> 9,
    "noMatchReferenceNotPairable" -> 0,
    "multipleMatchQueries"        -> 0,
    "failedQueries"               -> 3,
    "contributingQueries"         -> 454,
    "eligibleQueries"             -> 457,
    "queriesWithAMatch"           -> 457,
    "matchedPairsPerScale"        -> 457,
    "controlPairsPerScale"        -> 8512,
    "pairRowsPerScale"            -> 8969,
    "pairRowsAllScales"           -> 35876,
    "candidatePairsPerScale"      -> 219486,
    "focalTrials"                 -> 466,
    "referenceTrials"             -> 471
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
    // The recipe is the plan's own description, not prose.
    assert(c.downField("recipe").downField("description").as[Vector[Json]].exists(_.nonEmpty))
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

  /** FIXTURE.md's participant table: P, requested, contributing, failed, no
    * match, not admitted and the Remembered and Forgotten n at σ 2°.
    */
  private lazy val table: Vector[(String, Vector[Int])] =
    val row =
      raw"\| (P\d\d) \| (\d+) \| (\d+) \| (\d+) \| (\d+) \| (\d+) \| [^|]+ \| [^|]+ \| [^|]+ \| [^|(]+\((\d+)\) \| [^|(]+\((\d+)\) \|".r
    fixtureMd.linesIterator.collect { case row(p, counts*) =>
      p -> counts.map(_.toInt).toVector
    }.toVector

  test("FIXTURE.md's participant table reproduces through eyes4s") {
    assertEquals(table.size, 24)
    val qs          = queries(body)
    val notAdmitted = body.hcursor
      .downField("notAdmitted")
      .focus
      .flatMap(_.asArray)
      .getOrElse(fail("no notAdmitted"))
    val participants =
      body.hcursor.downField("summaries").downField("2").downField("participants")
    table.foreach { case (p, expected) =>
      val mine                  = qs.filter(q => str(q, "participant") == p)
      def count(prefix: String) = mine.count(q => str(q, "status").startsWith(prefix))
      val absent                = notAdmitted.count(q => str(q, "participant") == p)
      def n(group: String)      =
        participants
          .downField(p)
          .downField("byResponse")
          .downField(group)
          .downField("queries")
          .as[Int]
          .getOrElse(fail(s"no $group n for $p"))
      val actual = Vector(
        mine.size + absent,
        count("contributing"),
        count("failed"),
        count("no-match"),
        absent,
        n("Remembered"),
        n("Forgotten")
      )
      assertEquals(actual, expected, p)
    }
  }

  test("group, paired and per-group n agree with FIXTURE.md") {
    val two = body.hcursor.downField("summaries").downField("2")
    Vector("Remembered", "Forgotten").foreach { g =>
      assertEquals(
        two.downField("byResponse").downField(g).downField("participants").as[Int],
        Right(24),
        g
      )
    }
    assertEquals(
      two.downField("rememberedMinusForgotten").downField("paired").as[Int],
      Right(24)
    )
    val ns = table.flatMap((_, row) => Vector(row(5), row(6)))
    assertEquals((ns.min, ns.max), (2, 17))
    // FIXTURE.md: "Per-group n range across participants (Remembered/Forgotten): [2, 17]."
    assert(
      fixtureMd.contains(
        "Per-group n range across participants (Remembered/Forgotten): [2, 17]."
      )
    )
  }

  test("window figures and enc_03's role agree with FIXTURE.md") {
    val qs                            = queries(body)
    def outside(p: String, t: String) =
      qs.find(q => str(q, "participant") == p && str(q, "trial") == t)
        .flatMap(_.hcursor.downField("outsideWindow").focus)
        .getOrElse(fail(s"no $p $t"))
    def tally(fixations: Int, of: Int) =
      Json.obj("fixations" -> Json.fromInt(fixations), "of" -> Json.fromInt(of))
    // "ret_07 1 of 12 fixations"; "P05's 3 failed queries: 11 of 11 fixations outside".
    assertEquals(outside("P17", "ret_07"), tally(1, 12))
    Vector("ret_04", "ret_11", "ret_16").foreach(t =>
      assertEquals(outside("P05", t), tally(11, 11), t)
    )
    // "enc_03 1 of 13 fixations", from the plan's own window tally.
    val enc03 = study.preview.windowTallies.collectFirst {
      case (k, Right(t)) if k.participant == "P17" && k.trial == "enc_03" =>
        (t.outsideWindow, t.total)
    }
    assertEquals(enc03, Some((1, 13)))
    // "enc_03 (beach-042) is used by ret_07 as the matched reference and by the 18
    // other admitted P17 queries as a control."
    val asControl = study.pairs(study.preview.controls).collect {
      case (q, r) if r.participant == "P17" && r.trial == "enc_03" => q
    }
    assertEquals(asControl.size, 18)
    assert(asControl.forall(q => q.participant == "P17" && q.trial != "ret_07"))
    assert(fixtureMd.contains("by the 18 other admitted P17 queries as a control"))
  }

  test("at every scale the grand M minus B is the grand D, all over the same queries") {
    Vector("0.5", "1", "2", "4").foreach { s =>
      val grand = body.hcursor.downField("summaries").downField(s).downField("grand")
      def cell(r: String, f: String) = grand.downField(r).downField(f)
      val (m, b, d)                  = (
        cell("M", "estimate").as[Double],
        cell("B", "estimate").as[Double],
        cell("D", "estimate").as[Double]
      )
      (m, b, d) match
        case (Right(m), Right(b), Right(d)) =>
          assert(math.abs(m - b - d) <= 1.5e-6, s"$s: $m - $b vs $d")
        case other => fail(s"$s: $other")
      Vector("M", "B", "D").foreach(r =>
        assertEquals(cell(r, "queries").as[Int], Right(454), s"$s $r")
      )
    }
  }
