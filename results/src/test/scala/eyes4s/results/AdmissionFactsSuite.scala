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

package eyes4s.results

import eyes4s.kernel.{GeometryError, Span}
import eyes4s.plan.*

/** The admission facts of a methods text, read from a hand-built ledger,
  * with and without its trial inventory, and from hand-built window tallies.
  */
class AdmissionFactsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private def id(trial: String) =
    get(TrialIdentity.of("P1", "Encoding", trial, TrialOccurrence.first))
  private val (a, b, c, d) = (id("a"), id("b"), id("c"), id("d"))
  private val ka           = get(a.withItem("beach"))
  private val kb           = get(b.withItem("dog"))
  private val kc           = get(c.withItem("tree"))
  private val zero         = AdmissionReason.Number("n", "0", "a positive integer")
  private val overlap      = QuarantineCause.Overlap(2, "100", "90")
  private val source       = SourceRef.of("f.csv", Vector("h"), Vector(Vector("1")))
  private val rt           = AttributeColumn("rt", AttributeKind.Integer)

  // Records 2 and 3 admit a; 4 is a row-level rejection of b; 5 quarantines c
  // for overlap. d has no records.
  private val ledger = get(
    AdmissionLedger.decide(
      source,
      Vector("h"),
      Vector(
        SourceRecord(2, Disposition.Admitted(ka, 1)),
        SourceRecord(3, Disposition.Admitted(ka, 2)),
        SourceRecord(4, Disposition.Rejected(Vector("x"), Some(kb), zero)),
        SourceRecord(
          5,
          Disposition.Rejected(
            Vector("y"),
            Some(kc),
            AdmissionReason.Quarantined(Vector(5), overlap)
          )
        )
      ),
      AdmissionDecision.ReviewExclusions,
      AdmissionPolicy.default[TrialKey],
      Vector.empty
    )
  )

  private def trial(
      identity: TrialIdentity,
      item: String,
      records: Vector[Int],
      disposition: TrialDisposition
  ) = get(
    InventoryTrial.of(
      identity,
      Vector(2),
      Some(item),
      get(Attributes.of(Vector("rt" -> AttributeValue.Integer(1)))),
      if records.isEmpty then Vector.empty else Vector(item),
      records,
      disposition
    )
  )

  private val inventoried = get(
    ledger.withInventory(
      get(
        InventoryLedger.of(
          source,
          Vector("h"),
          Vector(rt),
          Vector(
            trial(a, "beach", Vector(2, 3), TrialDisposition.Admitted),
            trial(b, "dog", Vector(4), TrialDisposition.NoFixations),
            trial(c, "tree", Vector(5), TrialDisposition.Quarantined(overlap)),
            trial(d, "cat", Vector.empty, TrialDisposition.Absent)
          ),
          Vector.empty,
          Vector.empty,
          Vector.empty,
          SampleCountRule.PositiveColumn("n")
        )
      ),
      TrialIdentity.of,
      _.item
    )
  )

  private def tally(screen: Int, window: Int, total: Int, s: Long, w: Long, t: Long) =
    get(WindowTally.of(screen, window, total, Span.micros(s), Span.micros(w), Span.micros(t)))
  private val tallies = Vector(
    ka -> Right(tally(1, 2, 10, 100, 300, 1000)),
    kb -> Right(tally(0, 0, 5, 0, 0, 500)),
    kc -> Left(GeometryError.DegenerateBounds(0, 0, 0, 0))
  )

  private def fact(slot: FactSlot, count: LedgerCount, value: FactValue) =
    get(Fact.of(slot, FactSource.Ledger(count), value))
  private def n(slot: FactSlot, count: LedgerCount, value: Long) =
    fact(slot, count, FactValue.Count(value))
  private val overlapCode = FactCode.of(overlap)

  test("a ledger with its inventory: every trial's disposition, tallies and the policy") {
    assertEquals(
      get(AdmissionFacts.of(inventoried, tallies)),
      Vector(
        n(FactSlot.FixationRecords, LedgerCount.FixationRecords, 4),
        n(FactSlot.InventoryTrials, LedgerCount.InventoryTrials, 4),
        n(FactSlot.Admitted, LedgerCount.Admitted, 1),
        n(FactSlot.Quarantined, LedgerCount.Quarantined, 1),
        n(FactSlot.QuarantineCause(overlapCode), LedgerCount.Cause(overlapCode), 1),
        n(FactSlot.NoFixations, LedgerCount.NoFixations, 1),
        n(FactSlot.Absent, LedgerCount.Absent, 1),
        // The refused tally of c counts for nothing.
        n(FactSlot.TalliedRecords, LedgerCount.TalliedRecords, 15),
        n(FactSlot.RecordsOutsideWindow, LedgerCount.RecordsOutsideWindow, 2),
        n(FactSlot.TrialsOutsideWindow, LedgerCount.TrialsOutsideWindow, 1),
        fact(
          FactSlot.OutsideWindowShare,
          LedgerCount.OutsideWindowShare,
          FactValue.Share(0.2, ShareBasis.FixationDuration)
        ),
        n(FactSlot.RecordsOutsideScreen, LedgerCount.RecordsOutsideScreen, 1),
        n(FactSlot.TrialsOutsideScreen, LedgerCount.TrialsOutsideScreen, 1),
        fact(
          FactSlot.OffScreenPolicy,
          LedgerCount.OffScreenPolicy,
          FactValue.OffScreen(OffScreenPolicy.ExcludeRecord)
        )
      )
    )
    assertEquals(overlapCode.value, "overlap")
    // The totals agree with their parts, so they make a set of methods facts.
    assert(MethodsFacts.of(get(AdmissionFacts.of(inventoried, tallies))).isRight)
  }

  test("without an inventory, the trials the records name; absent trials are not known") {
    val slots = get(AdmissionFacts.of(ledger, Vector.empty))
    assertEquals(
      slots,
      Vector(
        n(FactSlot.FixationRecords, LedgerCount.FixationRecords, 4),
        n(FactSlot.Admitted, LedgerCount.Admitted, 1),
        n(FactSlot.Quarantined, LedgerCount.Quarantined, 1),
        n(FactSlot.QuarantineCause(overlapCode), LedgerCount.Cause(overlapCode), 1),
        n(FactSlot.NoFixations, LedgerCount.NoFixations, 1),
        fact(
          FactSlot.OffScreenPolicy,
          LedgerCount.OffScreenPolicy,
          FactValue.OffScreen(OffScreenPolicy.ExcludeRecord)
        )
      )
    )
  }

  test("fixations without duration leave the share undefined, so it is not stated") {
    val slots = get(AdmissionFacts.of(ledger, Vector(ka -> Right(tally(0, 0, 3, 0, 0, 0)))))
      .map(_.slot)
    assert(slots.contains(FactSlot.TalliedRecords), slots)
    assert(!slots.contains(FactSlot.OutsideWindowShare), slots)
  }
