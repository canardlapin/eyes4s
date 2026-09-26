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

import eyes4s.core.Weight
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** End to end, the Eyes Studio mock study: trial-keyed fixations on a
  * 1920 x 1080 screen admitted through `FixationCsv.admitInventory` against
  * the trials table, which supplies each trial's item and attributes and
  * accounts for absent trials, a 1024 x 768 image
  * window on a 64 x 48 grid, a 2-degree scale at a declared 35 px per degree,
  * and matched-minus-control contrasts under the default pairing (exactly one
  * matched reference, controls selected alike, unmatched focal trials
  * reported).
  *
  * Every count is read back through public API from the admission, the
  * prepared study and the result, so the suite can read the canonical
  * `fixtures/studio-golden` tables unchanged by replacing `source`.
  */
abstract class StudioAcceptance(source: => StudioFixture.Source) extends munit.FunSuite:
  override val munitTimeout                 = scala.concurrent.duration.Duration(10, "min")
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  /** The records in quarantined trials, where the fixture states it. */
  protected def rejectedRecords: Option[Int] = None

  private val screen = get(Frame.screen("studio-screen", 1920, 1080))
  private val image  =
    get(Subframe.centred(screen, FrameId("studio-image"), get(Extent.of[Px](1024, 768))))
  private val grid         = get(Grid.over(image.frame, 64, 48))
  private val trialColumns =
    get(TrialColumns.of("participant", "phase", "trial", Some("occurrence")))
  private val table = get(
    FixationTable.of(
      trialColumns,
      "ordinal",
      "x",
      "y",
      TimeColumns("onset_ms", "duration_ms", TimestampUnit.Milliseconds),
      SampleCountRule.PositiveColumn("sample_count")
    )
  )
  private val inventoryColumns = get(
    TrialInventoryColumns.of(
      trialColumns,
      Some("item"),
      Vector("display_kind", "image_file", "response").map(
        AttributeColumn(_, AttributeKind.Text)
      )
    )
  )

  private lazy val admission = get(
    FixationCsv.admitInventory(
      source.fixations,
      table,
      get(TrialInventory.read(source.trials, inventoryColumns)),
      screen
    )
  )
  private lazy val imported = admission.fixations
  private lazy val ledger   = get(
    FixationEvidence.ledger(
      "fixations.csv",
      "trials.csv",
      admission,
      AdmissionDecision.ReviewExclusions
    )
  )
  private lazy val inventory = ledger.inventory.getOrElse(fail("the ledger has no inventory"))
  private lazy val input     = StudyInput(imported.accepted)

  private lazy val plan = get(
    StudyPlan.configure(
      input.reference,
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      get(StudyGeometry.windowed(image, grid, OffWindowPolicy.Exclude)),
      "Retrieval",
      "Encoding",
      Weight.Duration,
      Vector(
        StudyScale.Angular(
          StudyEstimate.Gaussian[Deg](get(Sigma.deg(2)), EdgePolicy.Truncate)
        )
      ),
      Some(get(LinearAngularScale.of(screen, 35))),
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      ()
    )
  )
  private lazy val work    = get(plan.prepare(input))
  private lazy val result  = get(work.run)
  private lazy val tallies = work.windowTallies.collect { case (k, Right(t)) => k -> t }.toMap

  private def trialIds(ks: Iterable[TrialKey]) = ks.map(k => (k.participant, k.trial)).toSet
  private def identities(ts: Iterable[InventoryTrial]) =
    ts.map(t => (t.identity.participant, t.identity.trial)).toSet
  private def key(id: (String, String)): TrialKey =
    input.trials.rows
      .map(_.key)
      .find(k => (k.participant, k.trial) == id)
      .getOrElse(fail(s"no admitted trial $id"))

  private def pairs(s: DirectedPairSchedule[TrialKey, TrialKey]) =
    @annotation.tailrec
    def loop(
        c: PairCursor[TrialKey, TrialKey],
        acc: Vector[(TrialKey, TrialKey)]
    ): Vector[(TrialKey, TrialKey)] =
      get(c.advance(PairQuantum.default)) match
        case PairPage.More(ps, _, next) => loop(next, acc ++ ps.map(p => p.left -> p.right))
        case PairPage.Done(ps, _, _)    => acc ++ ps.map(p => p.left -> p.right)
    loop(s.start, Vector.empty)

  test("inventory: 960 trials = 937 admitted + 17 quarantined + 6 absent; 11,520 records") {
    // Read from the ledger alone: the importer joins the inventory.
    val trials = inventory.trials
    assertEquals(trials.size, 960)
    assertEquals(ledger.records.size, 11520)
    assertEquals(inventory.admitted.size, 937)
    assertEquals(inventory.quarantined.size, 17)
    assertEquals(inventory.absent.size, 6)
    assertEquals(inventory.unlisted, Vector.empty)
    val byCause = inventory.quarantined
      .map(_.disposition match
        case TrialDisposition.Quarantined(cause) => cause.productPrefix
        case other                               => other.label)
      .groupMapReduce(identity)(_ => 1)(_ + _)
    assertEquals(
      byCause,
      Map("DuplicateOrdinals" -> 4, "no fixations" -> 5, "Overlap" -> 6, "RejectedRecords" -> 2)
    )
    assertEquals(identities(inventory.absent), StudioFixture.absent)
    // Every record of a quarantined trial is rejected, and only those are.
    val quarantinedRecords = inventory.quarantined.map(_.records.size).sum
    assertEquals(ledger.rejected.size, quarantinedRecords)
    assertEquals(ledger.admitted.size, 11520 - quarantinedRecords)
    rejectedRecords.foreach(expected => assertEquals(quarantinedRecords, expected))
    assertEquals(input.trials.rows.size, 937)
  }

  test("inventory attributes are typed values of the admitted trial") {
    val query      = key(StudioFixture.focusQuery)
    val attributes = inventory.attributes(query).getOrElse(fail("no attributes"))
    assertEquals(attributes.get("response"), Some(AttributeValue.Text("Remembered")))
    assertEquals(
      attributes.get("display_kind"),
      Some(AttributeValue.Text("blank+fixation-cross"))
    )
    assertEquals(attributes.get("image_file"), Some(AttributeValue.Blank))
    assertEquals(admission.attributes(query), Some(attributes))
    val matched = inventory.attributes(key(StudioFixture.focusMatched))
    assertEquals(
      matched.flatMap(_.get("image_file")),
      Some(AttributeValue.Text("beach-042.png"))
    )
  }

  test("window: 543 records fall outside the image, in 409 trials, none off the screen") {
    val summary = get(work.preview).windowSummary
    assertEquals(summary.outsideWindow, 543)
    assertEquals(summary.trialsOutsideWindow, 409)
    assertEquals(summary.outsideScreen, 0)
    assertEquals(WindowSummary.of(work.windowTallies, ledger).sourceRecords, Some(11520))
    assertEquals(summary.untallied, 0)
    assertEquals(imported.outsideFrame, Vector.empty)
    def percent(t: WindowTally) = t.outsideWindowShare.map(s => math.round(s * 100))
    val query                   = tallies(key(StudioFixture.focusQuery))
    val matched                 = tallies(key(StudioFixture.focusMatched))
    assertEquals((query.outsideWindow, query.total, percent(query)), (1, 12, Some(4L)))
    assertEquals((matched.outsideWindow, matched.total, percent(matched)), (1, 13, Some(3L)))
    StudioFixture.offImage.foreach { id =>
      val t = tallies(key(id))
      assertEquals((t.outsideWindow, t.total), (11, 11), s"$id")
    }
  }

  test("queries: 480 = 454 contributing + 3 failed + 9 without a match + 14 not admitted") {
    val requested   = inventory.trials.count(_.identity.phase == "Retrieval")
    val focal       = work.focalIndices.map(i => input.trials.rows(i).key)
    val cardinality = get(work.matchedCardinality)
    assertEquals(cardinality.multiple, Vector.empty)
    assertEquals(cardinality.itemConflicts, Vector.empty)
    val noMatch = cardinality.unmatched
    val scale   = result.scales.head
    val rows    = get(scale.contrast).rows.filterNot(r => noMatch.contains(r.key))
    val failed  = rows.filter(_.difference.isLeft)
    assertEquals(requested, 480)
    assertEquals(requested - focal.size, 14)
    assertEquals(noMatch.size, 9)
    assertEquals(failed.size, 3)
    assertEquals(rows.count(_.difference.isRight), 454)
    assertEquals(trialIds(failed.map(_.key)), StudioFixture.offImage)
    assertEquals(
      failed.map(r => scale.estimation.collectFirst { case (r.key, Left(f)) => f }),
      failed.map(r => Some(StudyFailure.OffWindow(r.key, tallies(r.key))))
    )
    assertEquals(plan.preflight(Some(input)).blockers, Vector.empty)
  }

  test("controls: 19 per query, 18 where an encoding trial was not admitted; 8,969 pairs") {
    val preview      = get(work.preview)
    val matched      = pairs(preview.matched)
    val controls     = pairs(preview.controls)
    val matchedFocal = matched.map(_._1).toSet
    val perQuery     = controls
      .filter(p => matchedFocal.contains(p._1))
      .groupMapReduce(_._1)(_ => 1)(_ + _)
    assertEquals(perQuery.values.toSet, Set(18, 19))
    assertEquals(matched.size, 457)
    assertEquals(matched.size + perQuery.values.sum, 8969)
    val query = key(StudioFixture.focusQuery)
    assertEquals(query.item, "beach-042")
    assertEquals(
      matched.collect { case (`query`, r) => (r.trial, r.item) },
      Vector(("enc_03", "beach-042"))
    )
    assertEquals(perQuery(query), 19)
  }

/** The acceptance tests over the small deterministic stand-in, on both platforms. */
class StudioAcceptanceSuite extends StudioAcceptance(StudioFixture.source)
