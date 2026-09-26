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

package eyes4s.plan

/** An inventory joined to an admission ledger is accepted only when every
  * trial's disposition agrees with its records; each refusal names the trial
  * and record it concerns.
  */
class InventoryLedgerSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private def id(trial: String) = get(
    TrialIdentity.of("P1", "Encoding", trial, TrialOccurrence.first)
  )
  private val a      = id("a")
  private val b      = id("b")
  private val ka     = get(a.withItem("beach"))
  private val kb     = get(b.withItem("dog"))
  private val zero   = AdmissionReason.Number("n", "0", "a positive integer")
  private val source = SourceRef.of("f.csv", Vector("h"), Vector(Vector("1")))

  /** Records 2 and 3 admit trial a; record 4 is a row-level rejection of b. */
  private def ledger(
      records: Vector[SourceRecord[TrialKey]] = Vector(
        SourceRecord(2, Disposition.Admitted(ka, 1)),
        SourceRecord(3, Disposition.Admitted(ka, 2)),
        SourceRecord(4, Disposition.Rejected(Vector("x"), Some(kb), zero))
      )
  ) =
    get(
      AdmissionLedger.decide(
        source,
        Vector("h"),
        records,
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
  ) = InventoryTrial(
    identity,
    Vector(2),
    Some(item),
    Attributes.empty,
    Vector.empty,
    records,
    disposition
  )

  private def inventory(
      trials: Vector[InventoryTrial],
      unlisted: Vector[UnlistedTrial] = Vector.empty,
      attributes: Vector[RecordAttributes] = Vector.empty
  ) = InventoryLedger.of(source, Vector("h"), trials, unlisted, attributes)

  private def join(l: AdmissionLedger[TrialKey], i: InventoryLedger) =
    l.withInventory(i, TrialIdentity.of, _.item)

  private val good = Vector(
    trial(a, "beach", Vector(2, 3), TrialDisposition.Admitted),
    trial(b, "dog", Vector(4), TrialDisposition.NoFixations)
  )

  test("a consistent inventory joins and makes the ledger version 3") {
    val joined = get(join(ledger(), get(inventory(good))))
    assertEquals(joined.version, 3)
    assertEquals(ledger().version, 2)
    assertEquals(joined.inventory.map(_.admitted.map(_.identity)), Some(Vector(a)))
  }

  test("a trial whose records are all rejected on their own is refused as RejectedRecords") {
    val demoted = good.updated(
      1,
      good(1).copy(disposition = TrialDisposition.Quarantined(QuarantineCause.RejectedRecords))
    )
    assertEquals(
      join(ledger(), get(inventory(demoted))),
      Left(
        AdmissionError.Inventory(
          InventoryError.DispositionMismatch(
            b.render,
            "quarantined (RejectedRecords)",
            4,
            "rejected (Number)"
          )
        )
      )
    )
  }

  test("absent exactly when a trial has no records") {
    assertEquals(
      inventory(good.updated(1, good(1).copy(disposition = TrialDisposition.Absent))),
      Left(InventoryError.AbsentMismatch(b.render, "absent", Vector(4)))
    )
    assertEquals(
      inventory(Vector(trial(a, "beach", Vector.empty, TrialDisposition.NoFixations))),
      Left(InventoryError.AbsentMismatch(a.render, "no fixations", Vector.empty))
    )
  }

  test("identities, record ownership and order are checked when the inventory is built") {
    assertEquals(
      inventory(Vector(good(0), good(0).copy(records = Vector(4)))),
      Left(InventoryError.DuplicateTrial("P1", "Encoding", "a", 1))
    )
    assertEquals(
      inventory(Vector(good(0), good(1).copy(records = Vector(3)))),
      Left(InventoryError.SharedRecord(3, Vector(a.render, b.render)))
    )
    assertEquals(
      inventory(Vector(good(0).copy(records = Vector(3, 2)))),
      Left(InventoryError.RecordOrder(a.render, Vector(3, 2)))
    )
    assertEquals(
      inventory(
        good,
        attributes =
          Vector(RecordAttributes(3, Attributes.empty), RecordAttributes(2, Attributes.empty))
      ),
      Left(InventoryError.AttributeRecord(2))
    )
  }

  test("joining refuses unknown, foreign and unclaimed records and mismatched items") {
    def refused(i: InventoryLedger) = join(ledger(), i).left.toOption
    assertEquals(
      refused(get(inventory(Vector(good(0).copy(records = Vector(2, 3, 9)), good(1))))),
      Some(AdmissionError.Inventory(InventoryError.UnknownRecord(a.render, 9)))
    )
    assertEquals(
      refused(
        get(
          inventory(
            Vector(good(0).copy(records = Vector(2)), good(1).copy(records = Vector(3, 4)))
          )
        )
      ),
      Some(AdmissionError.Inventory(InventoryError.ForeignRecord(b.render, 3, a.render)))
    )
    assertEquals(
      refused(get(inventory(Vector(good(0))))),
      Some(AdmissionError.Inventory(InventoryError.UnclaimedRecord(4, b.render)))
    )
    assertEquals(
      refused(get(inventory(good.updated(0, good(0).copy(inventoryItem = Some("lake")))))),
      Some(AdmissionError.Inventory(InventoryError.ItemMismatch(a.render, 2, "lake", "beach")))
    )
    assertEquals(
      refused(get(inventory(good, attributes = Vector(RecordAttributes(4, Attributes.empty))))),
      Some(AdmissionError.Inventory(InventoryError.AttributeRecord(4)))
    )
  }

  test("a trial outside the inventory may not admit, and quarantines only as not listed") {
    val admittedUnlisted = get(
      inventory(Vector(good(1)), Vector(UnlistedTrial(a, Vector.empty, Vector(2, 3))))
    )
    assertEquals(
      join(ledger(), admittedUnlisted),
      Left(
        AdmissionError.Inventory(
          InventoryError.DispositionMismatch(a.render, "not in the inventory", 2, "admitted")
        )
      )
    )
  }

  test("an item conflict decides the cause of a quarantined trial") {
    val conflicting = good(0).copy(recordItems = Vector("lake"))
    assertEquals(
      conflicting.itemConflict,
      Some(QuarantineCause.InventoryItemConflict("beach", Vector("lake")))
    )
    assertEquals(conflicting.item, None)
    assertEquals(
      good(0).copy(inventoryItem = None, recordItems = Vector("a", "b")).itemConflict,
      Some(QuarantineCause.ItemConflict(Vector("a", "b")))
    )
    assertEquals(good(0).copy(inventoryItem = None, recordItems = Vector("a")).item, Some("a"))
  }

  test("a layout without a trial label projects no identity") {
    assertEquals(TrialIdentity.projection(StudyKey.layout(DefinitionId.studyLayout)), None)
    val project = TrialIdentity.projection(TrialKey.layout(TrialKeyDefinitions.trialLayout))
    assertEquals(project.map(_(ka)), Some(a))
  }

  test("declared attribute kinds parse their cells; an empty cell is blank") {
    assertEquals(
      AttributeColumn("n", AttributeKind.Integer).parse("-12"),
      Right(AttributeValue.Integer(-12L))
    )
    assertEquals(
      AttributeColumn("n", AttributeKind.Integer).parse("1.5"),
      Left("a decimal integer")
    )
    assertEquals(
      AttributeColumn("x", AttributeKind.Number).parse("NaN"),
      Left("a finite number")
    )
    assertEquals(
      AttributeColumn("t", AttributeKind.Text).parse(""),
      Right(AttributeValue.Blank)
    )
    assertEquals(
      Attributes.of(Vector("a" -> AttributeValue.Blank, "a" -> AttributeValue.Blank)),
      Left(InventoryError.DuplicateAttribute(Vector("a")))
    )
  }

  test("attributes name their entries; causes know the ledger version that introduced them") {
    val values = get(Attributes.of(Vector("a" -> AttributeValue.Blank)))
    assertEquals(values.names, Vector("a"))
    assertEquals((values.isEmpty, Attributes.empty.isEmpty), (false, true))
    assertEquals(QuarantineCause.isVersion1(QuarantineCause.Overlap(1, "a", "b")), true)
    assertEquals(QuarantineCause.isVersion1(QuarantineCause.ItemConflict(Vector("a"))), false)
    assertEquals(
      QuarantineCause.version(QuarantineCause.NotInInventory("P1", "E", "t", 1)),
      3
    )
  }
