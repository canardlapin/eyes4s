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

package eyes4s.studio.core.importing

import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.{AdmissionPolicy, InventoryError, QuarantineCause, TrialKey as CoreTrialKey}
import eyes4s.studio.core.document.{ColumnName, SourceRole}

/** The key builder's repeated keys are eyes4s's own findings (ticket S5.3):
  * on the `blocks` study, with Block as the occurrence, the keys the check
  * reports for the fixation records are exactly the trials eyes4s-io
  * quarantines as occurrence conflicts, record for record, and the keys it
  * reports for the inventory are exactly the conflicts eyes4s-io refuses the
  * inventory for. The studio check resolves nothing eyes4s does not.
  */
class TrialKeyAgreementJvmSuite extends munit.FunSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def name(s: String): ColumnName = get(ColumnName.of(s))

  private def check(source: SourceRole, text: String, key: KeyColumns, unit: TrialUnit) =
    val preview = get(CsvSniffer.sniff("f.csv", text))
    val table   = get(KeyTable.read("f.csv", text, preview.delimiter, preview.header))
    get(TrialKeyCheck.check("f.csv", source, table, key, unit))

  /** eyes4s's trial key reader needs the matched item: one per trial label. */
  private val fixations: String =
    KeyStudies
      .fixations(blocks = true)
      .linesIterator
      .zipWithIndex
      .map { (line, i) =>
        if i == 0 then s"$line,item"
        else
          val cells = line.split(",", -1)
          s"$line,item-${cells(0)}-${cells(2)}"
      }
      .mkString("", "\n", "\n")

  test("fixation records: repeated keys = eyes4s occurrence-conflict quarantines") {
    val screen  = get(Frame.screen("screen", 1920, 1080))
    val columns =
      get(FixationColumns.of("FixNum", "FixX", "FixY", "FixStart", "FixDur", "NSamples"))
    val keys =
      get(FixationKeyReader.trial("Subject", "Phase", "TrialID", "item", Some("Block")))
    val imported = get(
      FixationCsv.admit(
        fixations,
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy.default[CoreTrialKey]
      )
    )
    // eyes4s: each quarantined record of an occurrence conflict, by label.
    val eyes4s = imported.rejected
      .collect {
        case r @ RejectedFixationRow(
              _,
              _,
              Some(k),
              FixationRowError.Trial(_, QuarantineCause.OccurrenceConflict(_))
            ) =>
          KeyLabel(k.participant, k.phase, k.trial) -> (r.rowNumber - 1)
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sorted)
      .toMap
    val key = KeyColumns(
      Some(name("Subject")),
      Some(name("Phase")),
      Some(name("TrialID")),
      Some(name("Block"))
    )
    val report =
      check(SourceRole.Fixations, fixations, key, TrialUnit.Occurrence(name("Block")))
    assertEquals(eyes4s.keySet.size, 38)
    assertEquals(report.repeated.map(_.key).toSet, eyes4s.keySet)
    report.repeated.foreach(k =>
      assertEquals(k.trials.flatMap(_.records).sorted, eyes4s(k.key), k.key.render)
    )
    // Nothing else is quarantined, and eyes4s admits every other trial.
    assertEquals(imported.rejected.size, 38 * 2 * KeyStudies.recordsPerTrial)
    assertEquals(imported.accepted.rows.size, report.keys - 38)
  }

  test("the inventory: repeated keys = the conflicts eyes4s refuses the inventory for") {
    val text    = KeyStudies.inventory(blocks = true)
    val columns = get(
      TrialInventoryColumns.of(
        get(TrialColumns.of("participant", "phase", "trial", Some("Block"))),
        Some("item")
      )
    )
    val conflicts = TrialInventory.read(text, columns) match
      case Left(FixationImportError.Inventory(errors)) =>
        errors.toVector.collect { case InventoryError.Conflict(p, f, t, records, _) =>
          // eyes4s numbers the header as record 1.
          KeyLabel(p, f, t) -> records.map(_ - 1)
        }.toMap
      case other => fail(s"expected eyes4s to refuse the inventory, got $other")
    val key =
      KeyColumns(
        Some(name("participant")),
        Some(name("phase")),
        Some(name("trial")),
        Some(name("Block"))
      )
    val report = check(SourceRole.Trials, text, key, TrialUnit.Record)
    assertEquals(conflicts.size, 38)
    assertEquals(
      report.repeated.map(k => k.key -> k.trials.flatMap(_.records)).toMap,
      conflicts
    )
  }

  test("the unrepeated study: eyes4s finds no conflict and the check no repeat") {
    val text    = KeyStudies.inventory(blocks = false)
    val columns = get(
      TrialInventoryColumns.of(
        get(TrialColumns.of("participant", "phase", "trial", Some("Block")))
      )
    )
    val inventory = get(TrialInventory.read(text, columns))
    val key       =
      KeyColumns(
        Some(name("participant")),
        Some(name("phase")),
        Some(name("trial")),
        Some(name("Block"))
      )
    val report = check(SourceRole.Trials, text, key, TrialUnit.Record)
    assertEquals(inventory.trials.size, report.keys)
    assert(report.unique)
  }
