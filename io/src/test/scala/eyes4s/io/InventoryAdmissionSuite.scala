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

import cats.data.NonEmptyVector
import eyes4s.codec.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.*
import eyes4s.plan.*

/** The importer's trial-inventory route: the published inventory laws over
  * generated studies, the version-3 ledger round trip of what it admits, and
  * the declarations it requires.
  */
object InventoryFixtures:
  def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  val frame: Frame[Px]    = get(Frame.screen("law-frame", 100, 100))
  val trial: TrialColumns =
    get(TrialColumns.of("participant", "phase", "trial", Some("occurrence")))

  def table(
      item: Boolean,
      samples: SampleCountRule = SampleCountRule.PositiveColumn("samples")
  ) =
    get(
      FixationTable.of(
        trial,
        "ordinal",
        "x",
        "y",
        TimeColumns("onset", "duration", TimestampUnit.Milliseconds),
        samples,
        Option.when(item)("item")
      )
    )
  val inventoryColumns: TrialInventoryColumns = get(
    TrialInventoryColumns.of(trial, Some("item"))
  )

  /** Admit a generated scenario under the columns it documents. */
  def admit(s: InventoryScenario): Either[String, AdmissionLedger[TrialKey]] =
    for
      inventory <- TrialInventory.read(s.inventoryTable, inventoryColumns).left.map(_.message)
      imported  <- FixationCsv
        .admitInventory(s.fixationTable, table(s.recordItems), inventory, frame)
        .left
        .map(_.message)
      ledger <- FixationEvidence
        .ledger("fixations.csv", "trials.csv", imported, AdmissionDecision.ReviewExclusions)
        .left
        .map(_.message)
    yield ledger

class InventoryAdmissionLawSuite extends munit.DisciplineSuite:
  import InventoryFixtures.*

  checkAll("importer", InventoryLaws(InventoryFixtures.admit).inventory)

  private val ledgers = StudyInputCodecs.trial[Px].ledger
  checkAll(
    "inventory ledger",
    CodecLaws.roundTrip(
      ledgers,
      InventoryScenario.gen.map(s => get(admit(s))),
      (a: AdmissionLedger[TrialKey], b: AdmissionLedger[TrialKey]) => a == b
    )
  )

class InventoryAdmissionSuite extends munit.FunSuite:
  import InventoryFixtures.*

  private val trialsHeader = "participant,phase,trial,occurrence,item,response,count\n"
  private val header = "participant,phase,trial,occurrence,ordinal,x,y,onset,duration,samples\n"

  private val attributes = Vector(
    AttributeColumn("response", AttributeKind.Text),
    AttributeColumn("count", AttributeKind.Integer)
  )
  private def inventory(rows: String*): Either[FixationImportError, TrialInventory] =
    TrialInventory.read(
      trialsHeader + rows.mkString("\n") + "\n",
      get(TrialInventoryColumns.of(trial, Some("item"), attributes))
    )

  test("a trials table is read under declared columns with typed attributes") {
    val read = get(
      inventory(
        "P1,Encoding,e1,1,beach,,9007199254740993",
        "P1,Retrieval,r1,1,beach,Remembered,"
      )
    )
    assertEquals(read.trials.map(_.identity.trial), Vector("e1", "r1"))
    assertEquals(
      read.trials.map(_.attributes.entries),
      Vector(
        Vector(
          "response" -> AttributeValue.Blank,
          "count"    -> AttributeValue.Integer(9007199254740993L)
        ),
        Vector("response" -> AttributeValue.Text("Remembered"), "count" -> AttributeValue.Blank)
      )
    )
  }

  test(
    "identical inventory rows collapse; conflicting ones refuse, naming records and columns"
  ) {
    val same = get(inventory("P1,Encoding,e1,1,beach,,1", "P1,Encoding,e1,1,beach,,1"))
    assertEquals(same.trials.map(_.records), Vector(Vector(2, 3)))
    assertEquals(
      inventory("P1,Encoding,e1,1,beach,,1", "P1,Encoding,e1,2,dog,,1"),
      Left(
        FixationImportError.Inventory(
          NonEmptyVector.one(
            InventoryError.Conflict(
              "P1",
              "Encoding",
              "e1",
              Vector(2, 3),
              Vector("occurrence", "item")
            )
          )
        )
      )
    )
    assertEquals(
      inventory("P1,Encoding,e1,1,beach,,x"),
      Left(
        FixationImportError.Inventory(
          NonEmptyVector.one(InventoryError.Field(2, "count", "x", "a decimal integer"))
        )
      )
    )
    assertEquals(
      inventory("P1,Encoding,e1,1,,,1"),
      Left(
        FixationImportError.Inventory(
          NonEmptyVector.one(InventoryError.Field(2, "item", "", "a non-blank item"))
        )
      )
    )
    assertEquals(
      inventory("P1,Encoding,e1,1,beach,,1,extra"),
      Left(FixationImportError.Inventory(NonEmptyVector.one(InventoryError.Width(2, 7, 8))))
    )
  }

  private val trials = get(inventory("P1,Encoding,e1,1,beach,,1", "P1,Retrieval,r1,1,beach,,2"))

  private def admitted(
      fixations: String,
      table: FixationTable = InventoryFixtures.table(item = false)
  ): InventoryImport[Px] =
    get(FixationCsv.admitInventory(header + fixations, table, trials, frame))

  test(
    "an inventory trial with no records is absent; one with only rejected records has none"
  ) {
    val result = admitted("P1,Encoding,e1,1,1,5,5,0,40,0\nP1,Encoding,e1,1,2,6,6,50,40,0\n")
    assertEquals(
      result.trials.map(t => t.identity.trial -> t.disposition),
      Vector("e1" -> TrialDisposition.NoFixations, "r1" -> TrialDisposition.Absent)
    )
    assertEquals(result.trials.head.records, Vector(2, 3))
    // Each record keeps its own reason; none is quarantined as RejectedRecords.
    assert(result.fixations.rejected.forall(_.error match
      case FixationRowError.Number("samples", "0", _) => true
      case _                                          => false))
  }

  test("records of a trial outside the inventory are quarantined and listed, not dropped") {
    val result = admitted("P1,Encoding,e1,1,1,5,5,0,40,20\nP2,Encoding,x9,1,1,5,5,0,40,20\n")
    assertEquals(result.trials.map(_.disposition).head, TrialDisposition.Admitted)
    assertEquals(
      result.unlisted.map(u => u.identity.trial -> u.records),
      Vector("x9" -> Vector(3))
    )
    assertEquals(
      result.fixations.rejected.map(r => r.rowNumber -> r.key -> r.error),
      Vector(
        3 -> None -> FixationRowError.Trial(
          Vector(3),
          QuarantineCause.NotInInventory("P2", "Encoding", "x9", 1)
        )
      )
    )
  }

  test("the item comes from the inventory; records that name another conflict with it") {
    val itemHeader = header.stripSuffix("\n") + ",item\n"
    val table      = InventoryFixtures.table(item = true)
    val agree      = get(
      FixationCsv.admitInventory(
        itemHeader + "P1,Encoding,e1,1,1,5,5,0,40,20,beach\n",
        table,
        trials,
        frame
      )
    )
    assertEquals(agree.fixations.accepted.rows.map(_.key.item), Vector("beach"))
    val differ = get(
      FixationCsv.admitInventory(
        itemHeader + "P1,Encoding,e1,1,1,5,5,0,40,20,dog\nP1,Retrieval,r1,1,1,5,5,0,40,20,a\n" +
          "P1,Retrieval,r1,1,2,5,5,50,40,20,b\n",
        table,
        trials,
        frame
      )
    )
    assertEquals(
      differ.trials.map(_.disposition),
      Vector(
        TrialDisposition.Quarantined(
          QuarantineCause.InventoryItemConflict("beach", Vector("dog"))
        ),
        TrialDisposition.Quarantined(QuarantineCause.ItemConflict(Vector("a", "b")))
      )
    )
    // Reported under the inventory's item.
    assertEquals(differ.fixations.rejected.flatMap(_.key).map(_.item).distinct, Vector("beach"))
  }

  test("without an item column on either side the route refuses") {
    val bare = get(
      TrialInventory.read(
        "participant,phase,trial,occurrence\nP1,E,e1,1\n",
        get(TrialInventoryColumns.of(trial))
      )
    )
    val result =
      FixationCsv.admitInventory(header, InventoryFixtures.table(item = false), bare, frame)
    assert(result match
      case Left(FixationImportError.NoItemColumn(_, _)) => true
      case _                                            => false)
  }

  test("the declared time unit, never the values, sets the spans") {
    def span(unit: TimestampUnit) =
      val table = get(
        FixationTable.of(
          trial,
          "ordinal",
          "x",
          "y",
          TimeColumns("onset", "duration", unit),
          SampleCountRule.PositiveColumn("samples")
        )
      )
      admitted("P1,Encoding,e1,1,1,5,5,2,3,20\n", table).fixations.accepted.rows.head.value
        .fixations(0)
        .span
    assertEquals(span(TimestampUnit.Milliseconds).offset.toMicros, 5000L)
    assertEquals(span(TimestampUnit.Seconds).offset.toMicros, 5000000L)
  }

  test("without a count column, support is the duration at a declared rate, rounded up") {
    val rate  = get(Hz(500))
    val table = InventoryFixtures.table(item = false, SampleCountRule.DerivedFromDuration(rate))
    val noCount =
      "participant,phase,trial,occurrence,ordinal,x,y,onset,duration\n" +
        "P1,Encoding,e1,1,1,5,5,0,41\nP1,Encoding,e1,1,2,5,5,100,1\n"
    val result = get(FixationCsv.admitInventory(noCount, table, trials, frame))
    assertEquals(
      result.fixations.accepted.rows.head.value.fixations.toVector.map(_.sampleCount),
      Vector(21, 1)
    )
  }

  test("declared record attributes are typed; a value of the wrong kind rejects its record") {
    val table = get(
      FixationTable.of(
        trial,
        "ordinal",
        "x",
        "y",
        TimeColumns("onset", "duration", TimestampUnit.Milliseconds),
        SampleCountRule.PositiveColumn("samples"),
        attributes = Vector(AttributeColumn("pupil", AttributeKind.Number))
      )
    )
    val withPupil = header.stripSuffix("\n") + ",pupil\n"
    val good      = get(
      FixationCsv.admitInventory(
        withPupil + "P1,Encoding,e1,1,1,5,5,0,40,20,3.5\nP1,Encoding,e1,1,2,5,5,50,40,20,\n",
        table,
        trials,
        frame
      )
    )
    assertEquals(
      good.recordAttributes,
      Vector(
        get(
          RecordAttributes
            .of(2, get(Attributes.of(Vector("pupil" -> AttributeValue.Number(3.5)))))
        ),
        get(RecordAttributes.of(3, get(Attributes.of(Vector("pupil" -> AttributeValue.Blank)))))
      )
    )
    val bad = get(
      FixationCsv.admitInventory(
        withPupil + "P1,Encoding,e1,1,1,5,5,0,40,20,wide\n",
        table,
        trials,
        frame
      )
    )
    assertEquals(
      bad.fixations.rejected.map(_.error),
      Vector(FixationRowError.Number("pupil", "wide", "a finite decimal number"))
    )
    assertEquals(bad.trials.head.disposition, TrialDisposition.NoFixations)
  }

  test("the inventory route admits the same study input as the item-joined key route") {
    val rows = Vector(
      "P1,Encoding,e1,1,1,5,5,0,40,20",
      "P1,Encoding,e1,1,2,6,5,50,40,20",
      "P1,Retrieval,r1,1,1,7,5,0,40,20"
    )
    val joined = get(
      FixationCsv.admit(
        header.stripSuffix("\n") + ",item\n" + rows.map(_ + ",beach").mkString("\n"),
        get(FixationColumns.of("ordinal", "x", "y", "onset", "duration", "samples")),
        get(
          FixationKeyReader.trial("participant", "phase", "trial", "item", Some("occurrence"))
        ),
        frame,
        TimestampUnit.Milliseconds
      )
    )
    val inventoried = admitted(rows.mkString("\n"))
    assertEquals(
      StudyInput[TrialKey, Px](inventoried.fixations.accepted).reference,
      StudyInput[TrialKey, Px](joined.accepted).reference
    )
    assertEquals(inventoried.fixations.admitted, joined.admitted)
  }

  test("the ledger records the inventory and writes version 3; decoding re-checks it") {
    val result = admitted("P1,Encoding,e1,1,1,5,5,0,40,20\nP2,Encoding,x9,1,1,5,5,0,40,20\n")
    val ledger = get(
      FixationEvidence.ledger("f.csv", "t.csv", result, AdmissionDecision.ReviewExclusions)
    )
    assertEquals(ledger.version, 3)
    val codec = StudyInputCodecs.trial[Px].ledger
    val json  = get(codec.encode(ledger))
    assertEquals(
      json.hcursor.downField("schema").get[Int]("version"),
      Right(3)
    )
    assertEquals(get(codec.decode(json)), ledger)
  }

  test("an inventory import refuses to complete while records are rejected") {
    val clean = admitted("P1,Encoding,e1,1,1,5,5,0,40,20\n")
    assertEquals(clean.requireComplete.map(_.trials.size), Right(1))
    val dirty = admitted("P1,Encoding,e1,1,1,5,5,0,40,0\n")
    assertEquals(dirty.requireComplete.isLeft, true)
    val columns = get(FixationColumns.of("ordinal", "x", "y", "onset", "duration", "samples"))
    assertEquals(columns.names, Vector("ordinal", "x", "y", "onset", "duration", "samples"))
  }
