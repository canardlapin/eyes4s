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

import eyes4s.studio.core.backend.*

/** The FIXTURE.md counts, asserted on the embedded acceptance fixture
  * (fixtures/studio-golden, tallied at build time into [[GoldenInventory]]) and
  * cross-checked against the mock study (fixture.json) trial by trial.
  * Carried over from S0.7a; runs on the JVM and Scala.js without file I/O.
  */
class StudioFixtureCountsSuite extends munit.FunSuite:

  private val study: MockStudy =
    MockStudy.load.fold(e => throw new AssertionError(e), identity)

  private val inventory = study.inventory
  private val summary   = study.summary
  private val byKey     = inventory.map(e => e.trial -> e).toMap

  private def admitted(k: TrialKey): Boolean =
    byKey.get(k).exists(_.disposition == TrialDisposition.Admitted)

  test("inventory: 24 participants, each enc_01–enc_20 then ret_01–ret_20") {
    assertEquals(inventory.size, 960)
    val participants = inventory.map(_.trial.participant).distinct
    assertEquals(participants, (1 to 24).map(i => f"P$i%02d").toVector)
    val expected = (1 to 20).map(i => f"enc_$i%02d") ++ (1 to 20).map(i => f"ret_$i%02d")
    participants.foreach { p =>
      assertEquals(
        inventory.filter(_.trial.participant == p).map(_.trial.trial),
        expected.toVector
      )
    }
    assert(inventory.forall(_.trial.occurrence == 1))
    assert(inventory.forall(e => (e.trial.phase == Phase.Retrieval) == e.response.isDefined))
  }

  test("admission: 937 admitted, 17 quarantined by cause, 6 absent") {
    val tally = inventory.groupMapReduce(_.disposition)(_ => 1)(_ + _)
    assertEquals(
      tally,
      Map(
        TrialDisposition.Admitted                                       -> 937,
        TrialDisposition.Absent                                         -> 6,
        TrialDisposition.Quarantined(QuarantineCause.DuplicateOrdinals) -> 4,
        TrialDisposition.Quarantined(QuarantineCause.NoFixations)       -> 5,
        TrialDisposition.Quarantined(QuarantineCause.Overlap)           -> 6,
        TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)   -> 2
      )
    )
    assertEquals(
      (summary.inventoryTrials, summary.admitted, summary.quarantined, summary.absent),
      (960, 937, 17, 6)
    )
    summary.quarantineByCause.foreach { q =>
      assertEquals(tally(TrialDisposition.Quarantined(q.cause)), q.trials, q.cause)
    }
  }

  test("records, items, images and the analysis window") {
    assertEquals(GoldenInventory.fixationRecords, 11520)
    assertEquals(summary.fixationRecords, GoldenInventory.fixationRecords)
    assertEquals(GoldenInventory.distinctItems, 259)
    assertEquals(inventory.map(_.item).distinct.size, 259)
    assertEquals(summary.itemsInPool, 259)
    assertEquals((GoldenInventory.imagesPresent, summary.imagesFound), (257, 257))
    assertEquals(GoldenInventory.missingImages, Vector("forest-044", "kitchen-081"))
    assertEquals(summary.missingImages.map(_.item), GoldenInventory.missingImages)
    summary.missingImages.foreach { m =>
      val users = inventory.filter(e => e.item == m.item && e.trial.phase == Phase.Encoding)
      assertEquals(users.map(_.trial.participant), m.participants, m.item)
      assertEquals(users.size, m.encodingTrials, m.item)
    }
    assertEquals(
      (GoldenInventory.outsideWindowRecords, GoldenInventory.outsideWindowTrials),
      (543, 409)
    )
    assertEquals((summary.outsideWindowRecords, summary.outsideWindowTrials), (543, 409))
  }

  test("every fixture.json query agrees with the golden inventory") {
    assertEquals(study.queries.size, 480)
    study.queries.foreach { q =>
      val entry = byKey(q.key)
      assertEquals(entry.item, q.item, q.key)
      assertEquals(entry.response, Some(q.response), q.key)
      assertEquals(byKey(q.matchedKey).item, q.item, q.key)
      q.status match
        case "query not admitted" =>
          val expected = entry.disposition match
            case TrialDisposition.Absent         => "absent"
            case TrialDisposition.Quarantined(c) => c.code.stripPrefix("quarantine.")
            case TrialDisposition.Admitted       => "admitted"
          assertEquals(q.reason, Some(expected), q.key)
        case "no match" =>
          assert(admitted(q.key), q.key)
          assert(!admitted(q.matchedKey), q.key)
        case _ =>
          assert(admitted(q.key) && admitted(q.matchedKey), q.key)
          // Controls: the participant's other admitted encoding trials.
          val others = inventory.count(e =>
            e.trial.participant == q.participant && e.trial.phase == Phase.Encoding &&
              e.trial != q.matchedKey && e.disposition == TrialDisposition.Admitted
          )
          assertEquals(q.controls, Some(others), q.key)
    }
  }

  test("query contrasts: 480 requested = 14 not admitted + 9 no match + 3 failed + 454") {
    val retrieval   = inventory.filter(_.trial.phase == Phase.Retrieval)
    val notAdmitted = retrieval.count(e => !admitted(e.trial))
    val noMatch     = study.queries.count(q => admitted(q.key) && !admitted(q.matchedKey))
    val failed      = study.queries.count(_.status == "failed")
    assertEquals(retrieval.size, 480)
    assertEquals(
      QueryContrasts(480, notAdmitted, noMatch, failed, 480 - notAdmitted - noMatch - failed),
      QueryContrasts(480, 14, 9, 3, 454)
    )
    assertEquals(summary.contrasts, QueryContrasts(480, 14, 9, 3, 454))
    assertEquals(summary.eligibleQueries, 457)
    assertEquals(study.queries.count(_.status == "ok"), 454)
  }

  test("pair rows and scales") {
    assertEquals(summary.scales, Vector("0.5°", "1°", "2°", "4°"))
    assertEquals(summary.pairRowsPerScale, 8969L)
    assertEquals(summary.pairRowsAllScales, 35876L)
    assertEquals(summary.pairRowsRev5, 44845L)
    assertEquals(summary.candidatePairsPerScale, 230400L)
    // Pair rows are one matched pair plus the controls of every eligible query.
    val eligible = study.queries.filter(q => q.status == "ok" || q.status == "failed")
    assertEquals(
      eligible.map(q => 1 + q.controls.getOrElse(0)).sum.toLong,
      summary.pairRowsPerScale
    )
  }

  test("the focus story: P17 ret_07 · beach-042, enc_03 matched, ret_09 absent") {
    val focus = study.queries.find(_.key == MockStudy.key("P17", "ret_07"))
    assertEquals(
      focus.map(q => (q.item, q.matchTrial, q.response)),
      Some(("beach-042", "enc_03", Response.Remembered))
    )
    assertEquals(byKey(MockStudy.key("P17", "ret_09")).disposition, TrialDisposition.Absent)
    // enc_03 is a control of every other admitted P17 query: 18 of them.
    val asControl = study.queries.count(q =>
      q.participant == "P17" && q.key != MockStudy.key("P17", "ret_07") &&
        (q.status == "ok" || q.status == "failed")
    )
    assertEquals(asControl, 18)
  }
