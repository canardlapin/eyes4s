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

package eyes4s.studio.core.selection

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, InventoryScenario, StoryMoment}
import io.circe.syntax.*
import munit.CatsEffectSuite

/** The admission ledger's counts and the trials they open (ticket S5.6), on
  * the fake backend's admission and ledger of r3: every count the summary
  * holds is exactly the number of ledger entries its ref opens, the causes
  * partition the quarantined trials, and a whole ledger is read across
  * pages.
  */
class LedgerCountsSuite extends CatsEffectSuite:

  private val r3 = DatasetRevision(3)

  private def ok[E, A](fa: IO[Either[E, A]]): IO[A] =
    fa.flatMap(e => IO.fromEither(e.leftMap(err => new AssertionError(err.toString))))

  /** Every count ref of `summary`'s ledger. */
  private def refs(s: AdmissionSummary): Vector[StudioRef] =
    val kinds = Vector(
      InventoryKind.Inventory,
      InventoryKind.Admitted,
      InventoryKind.Quarantined,
      InventoryKind.NoFixations,
      InventoryKind.Absent
    ) ++ s.quarantined.map(q => InventoryKind.Cause(q.code))
    kinds.map(StudioRef.InventoryCount(s.dataset, _)) :+
      StudioRef.WindowTally(s.dataset, TallyRegion.OutsideScreen)

  private def read(
      moment: StoryMoment,
      scenario: InventoryScenario = InventoryScenario.Joined
  ): IO[(AdmissionSummary, Vector[LedgerEntry])] =
    for
      fake    <- FakeStudyBackend.create[IO](moment)
      _       <- fake.serveInventory(r3, scenario)
      summary <- ok(fake.admission(r3))
      entries <- ok(LedgerPages.all(fake.ledger(r3, _)))
    yield (summary, entries)

  test("fixture: every count of r3 opens exactly as many ledger trials as it counts") {
    List(StoryMoment.T1, StoryMoment.T2).traverse_ { moment =>
      read(moment).map { (s, entries) =>
        assertEquals(entries.size, 960)
        val counted = refs(s).map(r => r -> LedgerCounts.count(r, s))
        assertEquals(
          counted.map((r, n) => r -> n),
          refs(s).map(r => r -> LedgerCounts.trials(r, r3, entries).map(_.size))
        )
        def n(kind: InventoryKind) = LedgerCounts.count(StudioRef.InventoryCount(r3, kind), s)
        assertEquals(
          (
            n(InventoryKind.Inventory),
            n(InventoryKind.Admitted),
            n(InventoryKind.Quarantined),
            n(InventoryKind.Absent)
          ),
          (Some(960), Some(937), Some(17), Some(6))
        )
        assertEquals(
          s.quarantined.map(q => q.code -> q.trials).sortBy(_._1) :+
            ("no-fixations" -> s.noFixations),
          Vector(
            "quarantine.duplicate-ordinals" -> 4,
            "quarantine.overlap"            -> 6,
            "quarantine.rejected-records"   -> 2,
            "no-fixations"                  -> 5
          )
        )
        // The causes and no-fixations partition the quarantined trials.
        val parts = (s.quarantined.map(q => InventoryKind.Cause(q.code)) :+
          InventoryKind.NoFixations).flatMap(k =>
          LedgerCounts.trials(StudioRef.InventoryCount(r3, k), r3, entries).get.map(_.trial)
        )
        val quarantined = LedgerCounts
          .trials(StudioRef.InventoryCount(r3, InventoryKind.Quarantined), r3, entries)
          .get
          .map(_.trial)
        assertEquals(parts.sortBy(_.label), quarantined.sortBy(_.label))
        assertEquals(parts.distinct.size, parts.size)
      }
    }
  }

  test("a count of another dataset, or outside the window, counts and opens nothing here") {
    read(StoryMoment.T1).map { (s, entries) =>
      val other = StudioRef.InventoryCount(DatasetRevision(2), InventoryKind.Admitted)
      val frame = StudioRef.WindowTally(r3, TallyRegion.OutsideWindow)
      assertEquals(LedgerCounts.count(other, s), None)
      assertEquals(LedgerCounts.trials(other, r3, entries), None)
      assertEquals(LedgerCounts.count(frame, s), None)
      assertEquals(LedgerCounts.trials(frame, r3, entries), None)
      assertEquals(LedgerCounts.count(StudioRef.Trial(entries.head.trial), s), None)
      val unknown = StudioRef.InventoryCount(r3, InventoryKind.Cause("quarantine.wrong-clock"))
      assertEquals(LedgerCounts.count(unknown, s), None)
      assertEquals(LedgerCounts.trials(unknown, r3, entries).map(_.size), Some(0))
    }
  }

  test("without an inventory the ledger lists no absent trials and absent is not counted") {
    read(StoryMoment.T1, InventoryScenario.Undeclared).map { (s, entries) =>
      assertEquals(s.inventory, InventoryJoin.Undeclared)
      assertEquals(entries.size, 954)
      val absent = StudioRef.InventoryCount(r3, InventoryKind.Absent)
      assertEquals(LedgerCounts.count(absent, s), None)
      assertEquals(LedgerCounts.trials(absent, r3, entries).map(_.size), Some(0))
      assertEquals(
        LedgerCounts.count(StudioRef.InventoryCount(r3, InventoryKind.Inventory), s),
        None
      )
    }
  }

  test("a cause's trials sit under the quarantined count; the others are roots") {
    val cause = StudioRef.InventoryCount(r3, InventoryKind.Cause("quarantine.overlap"))
    val none  = StudioRef.InventoryCount(r3, InventoryKind.NoFixations)
    val held  = StudioRef.InventoryCount(r3, InventoryKind.Quarantined)
    assertEquals((cause.parent, none.parent, held.parent), (Some(held), Some(held), None))
    assert(cause.isAggregate)
    assertEquals(cause.asJson.as[StudioRef], Right(cause))
  }

  // --- Paging ------------------------------------------------------------------

  private def entry(i: Int): LedgerEntry =
    LedgerEntry(
      TrialKey(s"P$i", Phase.Encoding, "enc_01", 1),
      "item",
      None,
      TrialDisposition.Admitted,
      Vector.empty
    )

  /** A backend that serves at most `size` entries of `total` per page. */
  private def paged(total: Int, size: Int)(p: PageRequest) =
    val entries = (p.offset until (p.offset + size).min(total)).map(entry).toVector
    IO.pure(
      Right(LedgerPage(r3, PageInfo.of(p, total, entries.size), entries))
        .withLeft[BackendError]
    )

  test("a whole ledger is read across the pages the backend serves") {
    for
      all  <- LedgerPages.all(paged(250, 7))
      none <- LedgerPages.all(paged(0, 7))
    yield
      assertEquals(
        all.map(_.map(_.trial.participant)),
        Right((0 until 250).map(i => s"P$i").toVector)
      )
      assertEquals(none, Right(Vector.empty))
  }

  test("a refused page is the answer, never the entries read before it") {
    val refused = BackendError.UnknownDataset(r3, Vector.empty)
    for left <- LedgerPages.all(p =>
        if p.offset == 0 then paged(20, 5)(p) else IO.pure(Left(refused))
      )
    yield assertEquals(left, Left(LedgerReadError.Refused(refused)))
  }

  test("a next page that does not advance is an error naming the page, not a partial ledger") {
    var asked = 0
    // The first page advances to 5; the second names itself (or an earlier
    // page) as next.
    def stuck(back: Int)(p: PageRequest) =
      asked += 1
      val next = if p.offset == 0 then 5 else p.offset - back
      IO.pure(
        Right(LedgerPage(r3, PageInfo(p.offset, 10, Some(next)), Vector(entry(p.offset))))
          .withLeft[BackendError]
      )
    for
      same <- LedgerPages.all(stuck(0))
      sameAsked = asked
      back <- LedgerPages.all(stuck(3))
    yield
      assertEquals(same, Left(LedgerReadError.Stalled(r3, 5, 5)))
      assertEquals(sameAsked, 2)
      assertEquals(back, Left(LedgerReadError.Stalled(r3, 5, 2)))
      assertEquals(
        LedgerReadError.Stalled(r3, 5, 2).message,
        "The ledger of r3 does not advance: the page at offset 5 names the next page at " +
          "offset 2."
      )
  }
