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
  * 1920 x 1080 screen admitted through `FixationCsv`, a 1024 x 768 image
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

  private val screen = get(Frame.screen("studio-screen", 1920, 1080))
  private val image  =
    get(Subframe.centred(screen, FrameId("studio-image"), get(Extent.of[Px](1024, 768))))
  private val grid    = get(Grid.over(image.frame, 64, 48))
  private val columns = get(
    FixationColumns.of("ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count")
  )
  private val keys = get(
    FixationKeyReader.trial("participant", "phase", "trial", "item", Some("occurrence"))
  )

  private lazy val imported = get(
    FixationCsv.admit(source.fixations, columns, keys, screen, TimestampUnit.Milliseconds)
  )
  private lazy val ledger = get(
    FixationEvidence.ledger("fixations.csv", imported, AdmissionDecision.ReviewExclusions)
  )
  private lazy val input = StudyInput(imported.accepted)

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
  private lazy val tallies = work.windowTallies.toMap

  private def trialIds(ks: Iterable[TrialKey])    = ks.map(k => (k.participant, k.trial)).toSet
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
    assertEquals(source.inventory.size, 960)
    assertEquals(imported.sourceRows.size, 11520)
    val admitted = trialIds(input.trials.rows.map(_.key))
    // Trials with records but none admitted: quarantined, or every record rejected.
    val quarantined = trialIds(imported.rejected.flatMap(_.key)) -- admitted
    assertEquals(admitted.size, 937)
    assertEquals(quarantined.size, 17)
    val seen   = trialIds(imported.admitted.map(_.key) ++ imported.rejected.flatMap(_.key))
    val absent = source.inventory.toSet -- seen
    assertEquals(absent.size, 6)
    assertEquals(admitted.size + quarantined.size + absent.size, source.inventory.size)
    // The importer's own causes. A trial every record of which is rejected
    // ("no fixations" in the Studio inventory) has no trial-level cause until
    // the trial inventory (UI-H); its records are row-level rejections.
    val quarantineCauses = ledger.records
      .collect {
        case SourceRecord(
              _,
              Disposition.Rejected(_, Some(k), AdmissionReason.Quarantined(_, cause))
            ) =>
          (k.participant, k.trial) -> cause.productPrefix
      }
      .toMap
      .values
      .groupMapReduce(identity)(_ => 1)(_ + _)
    assertEquals(
      quarantineCauses,
      Map("RejectedRecords" -> 2, "Overlap" -> 6, "DuplicateOrdinals" -> 4)
    )
    assertEquals((quarantined -- trialIds(ledger.quarantined)).size, 5)
  }

  test("window: 543 records fall outside the image, in 409 trials, none off the screen") {
    val summary = get(work.preview).windowSummary
    assertEquals(summary.outsideWindow, 543)
    assertEquals(summary.trialsOutsideWindow, 409)
    assertEquals(summary.outsideScreen, 0)
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
    val requested   = source.inventory.count(_._2.startsWith("ret_"))
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
