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

import eyes4s.codec.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The review's probes of the inventory route (B1-B7 and the should-fix
  * items), kept as regression tests.
  */
class InventoryReviewSuite extends munit.FunSuite:
  import InventoryFixtures.*

  private val columns   = get(TrialInventoryColumns.of(trial, Some("item")))
  private val inventory = get(
    TrialInventory.read(
      "participant,phase,trial,occurrence,item\nP1,E,e1,1,beach\nP1,E,e2,1,dog\n",
      columns
    )
  )
  private val header =
    "participant,phase,trial,occurrence,item,ordinal,x,y,onset,duration,samples\n"
  private def run(rows: String, from: TrialInventory = inventory) =
    get(FixationCsv.admitInventory(header + rows, table(item = true), from, frame))
  private def ledger(result: InventoryImport[Px]) =
    get(FixationEvidence.ledger("f", "t", result, AdmissionDecision.ReviewExclusions))

  test(
    "B1: a blank item cell keeps the record with its trial, which takes the inventory's item"
  ) {
    val result = run("P1,E,e1,1,,1,5,5,0,40,20\nP1,E,e2,1,dog,1,5,5,0,40,20\n")
    assertEquals(result.trials.head.records, Vector(2))
    assertEquals(result.trials.head.disposition, TrialDisposition.Admitted)
    assertEquals(result.fixations.admitted.map(_.key.item), Vector("beach", "dog"))
  }

  test("B2: a blank item cell among good records is neither dropped nor detached") {
    val result = run("P1,E,e1,1,beach,1,5,5,0,40,20\nP1,E,e1,1,,2,5,5,100,40,20\n")
    assertEquals(result.trials.head.records, Vector(2, 3))
    assertEquals(result.fixations.admitted.map(_.rowNumber), Vector(2, 3))
  }

  test("B2': without an inventory item, a blank record item rejects the record with its key") {
    val bare = get(
      TrialInventory.read(
        "participant,phase,trial,occurrence\nP1,E,e1,1\n",
        get(TrialInventoryColumns.of(trial))
      )
    )
    val result = run("P1,E,e1,1,beach,1,5,5,0,40,20\nP1,E,e1,1,,2,5,5,100,40,20\n", bare)
    assertEquals(result.trials.head.records, Vector(2, 3))
    assertEquals(
      result.trials.head.disposition,
      TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)
    )
    val blank = result.fixations.rejected.find(_.rowNumber == 3).get
    assertEquals(blank.key.map(_.item), Some("beach"))
    assertEquals(blank.error, FixationRowError.Number("item", "", "a non-blank item cell"))
  }

  test("B3/item 3: a label's records with another occurrence are an occurrence conflict") {
    val rejected = TrialInventory.read(
      "participant,phase,trial,occurrence,item\nP1,E,e1,1,beach\nP1,E,e1,2,beach\n",
      columns
    )
    assert(rejected.isLeft)
    // The occurrence is part of a declared trial's identity, not of its label.
    def e1(occurrence: Int) =
      get(TrialIdentity.of("P1", "E", "e1", get(TrialOccurrence.of(occurrence))))
    assertEquals(inventory.trial(e1(1)).map(_.records), Some(Vector(2)))
    assertEquals(inventory.trial(e1(2)), None)
    val result = run("P1,E,e1,2,beach,1,5,5,0,40,20\n")
    assertEquals(
      result.trials.head.disposition,
      TrialDisposition.Quarantined(QuarantineCause.OccurrenceConflict(Vector(1, 2)))
    )
    assertEquals(result.unlisted, Vector.empty)
    assert(
      FixationEvidence.ledger("f", "t", result, AdmissionDecision.ReviewExclusions).isRight
    )
  }

  test("B4/item 7: repeated inventory rows are compared on parsed values") {
    val numeric = get(
      TrialInventoryColumns.of(
        trial,
        Some("item"),
        Vector(AttributeColumn("rt", AttributeKind.Number))
      )
    )
    val read = TrialInventory.read(
      "participant,phase,trial,occurrence,item,rt\nP1,E,e1,1,beach,1\nP1,E,e1,01,beach,1.0\n",
      numeric
    )
    assertEquals(read.map(_.trials.map(_.records)), Right(Vector(Vector(2, 3))))
  }

  test("B6/item 4: a version-3 ledger must carry its inventory") {
    val codec  = StudyInputCodecs.trial[Px].ledger
    val json   = get(codec.encode(ledger(run("P1,E,e1,1,beach,1,5,5,0,40,20\n"))))
    val nulled =
      json.hcursor.downField("value").downField("inventory").set(io.circe.Json.Null).top.get
    assert(codec.decode(nulled).isLeft)
  }

  test("item 4: a ledger without an inventory cannot name NotInInventory") {
    val built = AdmissionLedger.of(
      SourceRef.of("f", Vector("h"), Vector(Vector("1"))),
      Vector("h"),
      Vector(
        SourceRecord(
          2,
          Disposition.Rejected[TrialKey](
            Vector("x"),
            None,
            AdmissionReason.Quarantined(
              Vector(2),
              QuarantineCause.NotInInventory("P1", "E", "x", 1)
            )
          )
        )
      ),
      AdmissionOutcome.ReviewedExclusions
    )
    assert(built.isLeft, s"$built")
  }

  test("B7/item 5: the inventory is refused with every defect, each naming row and column") {
    val refused = TrialInventory.read(
      "participant,phase,trial,occurrence,item\nP1,E,e1,1,\nP1,E,,1,dog\nP1,E,e3,0,cat\n",
      columns
    )
    val message = refused.left.map(_.message).left.getOrElse("")
    Vector("record 2", "'item'", "record 3", "'trial'", "record 4", "'occurrence'").foreach(
      part => assert(message.contains(part), s"$part missing from: $message")
    )
  }

  test("item 8: numbers follow a strict decimal grammar") {
    val number = AttributeColumn("x", AttributeKind.Number)
    Vector(" 1", "1 ", "1d", "1f", "0x1p3", "NaN", "Infinity", "1e", "+", ".").foreach(raw =>
      assert(number.parse(raw).isLeft, s"'$raw' was accepted")
    )
    Vector("1", "-1.5", "+2", ".5", "1.", "1e3", "-2.5E-3").foreach(raw =>
      assert(number.parse(raw).isRight, s"'$raw' was refused")
    )
    val integer = AttributeColumn("n", AttributeKind.Integer)
    Vector(" 1", "1 ", "0x10", "1L").foreach(raw => assert(integer.parse(raw).isLeft, raw))
  }

  test(
    "B5/item 2: a forged inventory (absent trial with record items, mixed kinds) is refused"
  ) {
    val l  = ledger(run("P1,E,e1,1,beach,1,5,5,0,40,20\n"))
    val iv = l.inventory.get
    def rebuilt(t: InventoryTrial, items: Vector[String], values: Attributes) =
      InventoryTrial.of(
        t.identity,
        t.rows,
        t.inventoryItem,
        values,
        items,
        t.records,
        t.disposition
      )
    val absent = iv.trials.find(_.identity.trial == "e2").get
    assertEquals(absent.disposition, TrialDisposition.Absent)
    assertEquals(
      rebuilt(absent, Vector("zzz"), absent.attributes),
      Left(InventoryError.RecordItems(absent.identity.render, "absent", Vector("zzz")))
    )
    val mixed = iv.trials.map(t =>
      get(
        rebuilt(
          t,
          t.recordItems,
          get(
            Attributes.of(
              Vector(
                "rt" -> (if t.identity.trial == "e2" then AttributeValue.Text("fast")
                         else AttributeValue.Integer(3))
              )
            )
          )
        )
      )
    )
    assertEquals(
      InventoryLedger.of(
        iv.source,
        iv.header,
        Vector(AttributeColumn("rt", AttributeKind.Integer)),
        mixed,
        iv.unlisted,
        iv.recordAttributeColumns,
        iv.recordAttributes,
        iv.sampleCounts
      ),
      Left(
        InventoryError.AttributeKindMismatch(absent.identity.render, "rt", "Integer", "Text")
      )
    )
  }

  test("item 6: derived sample counts and their rate are recorded in the ledger") {
    val rate  = get(eyes4s.kernel.Hz(500))
    val table = get(
      FixationTable.of(
        trial,
        "ordinal",
        "x",
        "y",
        TimeColumns("onset", "duration", TimestampUnit.Milliseconds),
        SampleCountRule.DerivedFromDuration(rate),
        Some("item")
      )
    )
    val result = get(
      FixationCsv.admitInventory(
        "participant,phase,trial,occurrence,item,ordinal,x,y,onset,duration\n" +
          "P1,E,e1,1,beach,1,5,5,0,41\n",
        table,
        inventory,
        frame
      )
    )
    val saved = ledger(result)
    assertEquals(
      saved.inventory.map(_.sampleCounts),
      Some(SampleCountRule.DerivedFromDuration(rate))
    )
    val codec = StudyInputCodecs.trial[Px].ledger
    assertEquals(codec.decode(get(codec.encode(saved))), Right(saved))
  }
