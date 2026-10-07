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
  * trial's disposition, items and attributes agree with its records and
  * declarations; each refusal names the trial and record it concerns.
  */
class InventoryLedgerSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private def id(trial: String) =
    get(TrialIdentity.of("P1", "Encoding", trial, TrialOccurrence.first))
  private val a      = id("a")
  private val b      = id("b")
  private val ka     = get(a.withItem("beach"))
  private val kb     = get(b.withItem("dog"))
  private val zero   = AdmissionReason.Number("n", "0", "a positive integer")
  private val source = SourceRef.of("f.csv", Vector("h"), Vector(Vector("1")))
  private val rt     = AttributeColumn("rt", AttributeKind.Integer)
  private val counts = SampleCountRule.PositiveColumn("n")

  private val records = Vector(
    SourceRecord(2, Disposition.Admitted(ka, 1)),
    SourceRecord(3, Disposition.Admitted(ka, 2)),
    SourceRecord(4, Disposition.Rejected(Vector("x"), Some(kb), zero))
  )

  /** Records 2 and 3 admit trial a; record 4 is a row-level rejection of b. */
  private def ledger(rs: Vector[SourceRecord[TrialKey]] = records) =
    get(
      AdmissionLedger.decide(
        source,
        Vector("h"),
        rs,
        AdmissionDecision.ReviewExclusions,
        AdmissionPolicy.default[TrialKey],
        Vector.empty
      )
    )

  private def rtIs(n: Long) = get(Attributes.of(Vector("rt" -> AttributeValue.Integer(n))))

  private final case class T(
      identity: TrialIdentity,
      item: String,
      records: Vector[Int],
      disposition: TrialDisposition,
      recordItems: Vector[String] = Vector.empty,
      attributes: Attributes = rtIs(1)
  ):
    def build = InventoryTrial.of(
      identity,
      Vector(2),
      Some(item),
      attributes,
      recordItems,
      records,
      disposition
    )

  private def inventory(
      trials: Vector[T],
      unlisted: Vector[UnlistedTrial] = Vector.empty,
      attributes: Vector[RecordAttributes] = Vector.empty,
      recordColumns: Vector[AttributeColumn] = Vector.empty
  ): Either[InventoryError, InventoryLedger] =
    trials
      .foldLeft[Either[InventoryError, Vector[InventoryTrial]]](Right(Vector.empty))((acc, t) =>
        acc.flatMap(done => t.build.map(done :+ _))
      )
      .flatMap(built =>
        InventoryLedger.of(
          source,
          Vector("h"),
          Vector(rt),
          built,
          unlisted,
          recordColumns,
          attributes,
          counts
        )
      )

  private def join(l: AdmissionLedger[TrialKey], i: InventoryLedger) =
    l.withInventory(i, TrialIdentity.of, _.item)

  private val good = Vector(
    T(a, "beach", Vector(2, 3), TrialDisposition.Admitted),
    T(b, "dog", Vector(4), TrialDisposition.NoFixations)
  )

  test("a consistent inventory joins and makes the ledger version 3") {
    val joined = get(join(ledger(), get(inventory(good))))
    assertEquals(joined.version, 3)
    assertEquals(ledger().version, 2)
    assertEquals(joined.inventory.map(_.admitted.map(_.identity)), Some(Vector(a)))
    assertEquals(joined.inventory.map(_.sampleCounts), Some(counts))
  }

  test("a trial whose records are all rejected on their own is refused as RejectedRecords") {
    val demoted =
      good.updated(
        1,
        good(1).copy(disposition =
          TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)
        )
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

  test("absent exactly when a trial has no records, and then it names no items") {
    assertEquals(
      good(1).copy(disposition = TrialDisposition.Absent).build,
      Left(InventoryError.AbsentMismatch(b.render, "absent", Vector(4)))
    )
    assertEquals(
      T(a, "beach", Vector.empty, TrialDisposition.NoFixations).build,
      Left(InventoryError.AbsentMismatch(a.render, "no fixations", Vector.empty))
    )
    assertEquals(
      T(a, "beach", Vector.empty, TrialDisposition.Absent, Vector("zzz")).build,
      Left(InventoryError.RecordItems(a.render, "absent", Vector("zzz")))
    )
  }

  test("record items are sorted, distinct, and agree with the disposition") {
    assertEquals(
      good(0).copy(recordItems = Vector("dog", "beach")).build.isLeft,
      true
    )
    // An item conflict the disposition does not name.
    assertEquals(
      good(0).copy(recordItems = Vector("lake")).build,
      Left(InventoryError.RecordItems(a.render, "admitted", Vector("lake")))
    )
    // A named item conflict the records do not show.
    assertEquals(
      good(0)
        .copy(disposition =
          TrialDisposition.Quarantined(QuarantineCause.ItemConflict(Vector("a", "b")))
        )
        .build
        .isLeft,
      true
    )
    assertEquals(
      good(0)
        .copy(disposition =
          TrialDisposition.Quarantined(QuarantineCause.NotInInventory("P1", "Encoding", "a", 1))
        )
        .build
        .isLeft,
      true
    )
  }

  test("attributes match the declared columns, by name, order and kind") {
    assertEquals(
      inventory(
        good.updated(
          1,
          good(1)
            .copy(attributes = get(Attributes.of(Vector("rt" -> AttributeValue.Text("fast")))))
        )
      ),
      Left(InventoryError.AttributeKindMismatch(b.render, "rt", "Integer", "Text"))
    )
    assertEquals(
      inventory(good.updated(1, good(1).copy(attributes = Attributes.empty))),
      Left(InventoryError.AttributeNames(b.render, Vector("rt"), Vector.empty))
    )
    assertEquals(
      inventory(
        good,
        attributes = Vector(get(RecordAttributes.of(2, rtIs(3)))),
        recordColumns = Vector(AttributeColumn("rt", AttributeKind.Text))
      ),
      Left(InventoryError.AttributeKindMismatch("record 2", "rt", "Text", "Integer"))
    )
  }

  test(
    "identities, labels, record ownership and order are checked when the inventory is built"
  ) {
    assertEquals(
      inventory(Vector(good(0), good(0).copy(records = Vector(4)))),
      Left(InventoryError.DuplicateTrial("P1", "Encoding", "a", 1))
    )
    val second = get(TrialIdentity.of("P1", "Encoding", "a", get(TrialOccurrence.of(2))))
    assertEquals(
      inventory(Vector(good(0), good(0).copy(identity = second, records = Vector(4)))),
      Left(InventoryError.DuplicateTrial("P1", "Encoding", "a", 1))
    )
    assertEquals(
      inventory(Vector(good(0), good(1).copy(records = Vector(3)))),
      Left(InventoryError.SharedRecord(3, Vector(a.render, b.render)))
    )
    assertEquals(
      good(0).copy(records = Vector(3, 2)).build,
      Left(InventoryError.RecordOrder(a.render, Vector(3, 2)))
    )
    assertEquals(
      inventory(
        good,
        attributes = Vector(
          get(RecordAttributes.of(3, Attributes.empty)),
          get(RecordAttributes.of(2, Attributes.empty))
        )
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
      refused(get(inventory(good.updated(0, good(0).copy(item = "lake"))))),
      Some(AdmissionError.Inventory(InventoryError.ItemMismatch(a.render, 2, "lake", "beach")))
    )
    // The rejected record of a trial with no fixations carries the item too.
    assertEquals(
      refused(get(inventory(good.updated(1, good(1).copy(item = "cat"))))),
      Some(AdmissionError.Inventory(InventoryError.ItemMismatch(b.render, 4, "cat", "dog")))
    )
    assertEquals(
      refused(
        get(inventory(good, attributes = Vector(get(RecordAttributes.of(4, Attributes.empty)))))
      ),
      Some(AdmissionError.Inventory(InventoryError.AttributeRecord(4)))
    )
  }

  test("a trial outside the inventory may not admit, and quarantines only as not listed") {
    val admittedUnlisted =
      get(
        inventory(Vector(good(1)), Vector(get(UnlistedTrial.of(a, Vector.empty, Vector(2, 3)))))
      )
    assertEquals(
      join(ledger(), admittedUnlisted),
      Left(
        AdmissionError.Inventory(
          InventoryError.DispositionMismatch(a.render, "not in the inventory", 2, "admitted")
        )
      )
    )
    assertEquals(
      UnlistedTrial.of(a, Vector.empty, Vector.empty),
      Left(InventoryError.RecordOrder(a.render, Vector.empty))
    )
  }

  test("only a ledger with an inventory may name the inventory's causes") {
    val unlisted = SourceRecord(
      5,
      Disposition.Rejected[TrialKey](
        Vector("x"),
        None,
        AdmissionReason.Quarantined(
          Vector(5),
          QuarantineCause.NotInInventory("P2", "E", "x", 1)
        )
      )
    )
    val rs = records :+ unlisted
    assertEquals(
      AdmissionLedger.of(source, Vector("h"), rs, AdmissionOutcome.ReviewedExclusions),
      Left(
        AdmissionError.UninventoriedCause(5, QuarantineCause.NotInInventory("P2", "E", "x", 1))
      )
    )
    val x      = get(TrialIdentity.of("P2", "E", "x", TrialOccurrence.first))
    val joined = AdmissionLedger.decide(
      source,
      Vector("h"),
      rs,
      AdmissionDecision.ReviewExclusions,
      AdmissionPolicy.default[TrialKey],
      Vector.empty,
      get(inventory(good, Vector(get(UnlistedTrial.of(x, Vector.empty, Vector(5)))))),
      TrialIdentity.of,
      _.item
    )
    assertEquals(joined.map(_.version), Right(3))
  }

  test("an item conflict decides the cause of a quarantined trial") {
    val conflicting = get(
      good(0)
        .copy(
          records = Vector(2),
          recordItems = Vector("lake"),
          disposition = TrialDisposition.Quarantined(
            QuarantineCause.InventoryItemConflict("beach", Vector("lake"))
          )
        )
        .build
    )
    assertEquals(
      conflicting.itemConflict,
      Some(QuarantineCause.InventoryItemConflict("beach", Vector("lake")))
    )
    assertEquals((conflicting.item, conflicting.keyItem), (None, Some("beach")))
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
      AttributeColumn("n", AttributeKind.Integer).parse("+7"),
      Right(AttributeValue.Integer(7L))
    )
    assertEquals(
      AttributeColumn("n", AttributeKind.Integer).parse("1.5"),
      Left("a decimal integer")
    )
    assertEquals(
      AttributeColumn("x", AttributeKind.Number).parse("NaN"),
      Left("a finite decimal number")
    )
    assertEquals(
      AttributeColumn("x", AttributeKind.Number).parse("1e400"),
      Left("a finite decimal number")
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
    assertEquals(QuarantineCause.version(QuarantineCause.NotInInventory("P1", "E", "t", 1)), 3)
    assertEquals(SampleCountRule.PositiveColumn("n").countColumn, Some("n"))
    assertEquals(
      SampleCountRule.DerivedFromDuration(get(eyes4s.kernel.Hz(500))).countColumn,
      None
    )
  }

  test("accounting partitions declared trials, including no-fixations in quarantine") {
    val value = get(
      inventory(
        Vector(
          T(a, "beach", Vector(2), TrialDisposition.Admitted),
          T(b, "dog", Vector(3), TrialDisposition.NoFixations),
          T(
            id("c"),
            "bird",
            Vector(4),
            TrialDisposition.Quarantined(
              QuarantineCause.InvalidExtent("invalid trial duration")
            )
          ),
          T(id("d"), "cat", Vector.empty, TrialDisposition.Absent)
        )
      )
    )
    assertEquals(value.admitted.size, 1)
    assertEquals(value.quarantined.size, 2)
    assertEquals(value.absent.size, 1)
    assert(value.accountingBalances)
    assert(get(inventory(Vector.empty)).accountingBalances)
  }
