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

import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.*

/** The trial key check (ticket S5.3): keys composed of participant, phase,
  * trial and, optionally, occurrence, checked against every record under
  * eyes4s's TrialKey semantics. A repeated key is reported with all of its
  * trials and all of their records; it is never resolved by first match.
  */
class TrialKeySuite extends munit.FunSuite:

  private def name(s: String): ColumnName =
    ColumnName.of(s).fold(e => fail(e.message), identity)

  private def table(file: String, text: String): KeyTable =
    val preview = CsvSniffer.sniff(file, text).fold(e => fail(e.message), identity)
    KeyTable
      .read(file, text, preview.delimiter, preview.header)
      .fold(e => fail(e.message), identity)

  private def report(
      source: SourceRole,
      text: String,
      columns: KeyColumns,
      unit: TrialUnit,
      file: String = "f.csv"
  ): KeyReport =
    TrialKeyCheck
      .check(file, source, table(file, text), columns, unit)
      .fold(g => fail(g.message), identity)

  private val boardKey: KeyColumns =
    KeyColumns(
      Some(name("Subject")),
      Some(name("Phase")),
      Some(name("TrialID")),
      Some(name("Block"))
    )

  private val boardKeyWithoutOccurrence: KeyColumns = boardKey.copy(occurrence = None)

  private val inventoryKey: KeyColumns =
    KeyColumns(
      Some(name("participant")),
      Some(name("phase")),
      Some(name("trial")),
      Some(name("Block"))
    )

  private val block = TrialUnit.Occurrence(name("Block"))

  // --- the fixture's case: every key unique ---------------------------------------

  test("golden-shaped inventory: 960 unique keys, 0 duplicates, occurrence 1 for every trial") {
    val r = report(
      SourceRole.Trials,
      KeyStudies.inventory(blocks = false),
      inventoryKey,
      TrialUnit.Record
    )
    assertEquals(r.records, 960)
    assertEquals(r.keys, 960)
    assertEquals(r.repeated, Vector.empty)
    assertEquals(r.unresolved, Vector.empty)
    assertEquals(r.occurrences, Vector(1))
    assertEquals(r.laterPresentations, 0)
    assert(r.unique)
  }

  test("golden-shaped fixations: 960 keys over 2,880 records, none repeating") {
    val r = report(SourceRole.Fixations, KeyStudies.fixations(blocks = false), boardKey, block)
    assertEquals(r.records, 960 * KeyStudies.recordsPerTrial)
    assertEquals(r.keys, 960)
    assert(r.unique)
    // Leaving the occurrence out changes nothing when Block is 1 everywhere.
    val out =
      report(
        SourceRole.Fixations,
        KeyStudies.fixations(blocks = false),
        boardKeyWithoutOccurrence,
        block
      )
    assert(out.unique)
    assertEquals(out.occurrences, Vector.empty)
    assert(!out.repeatsWithoutOccurrence)
  }

  // --- the board's case: 38 keys repeat without Occurrence ---------------------------

  test("38 keys repeat without Occurrence, each reported with both trials and every record") {
    val text = KeyStudies.fixations(blocks = true)
    val r    = report(SourceRole.Fixations, text, boardKeyWithoutOccurrence, block)
    assertEquals(r.keys, 922)
    assertEquals(r.repeated.map(_.key), KeyStudies.repeatedKeys.sorted)
    assert(r.repeatsWithoutOccurrence)
    r.repeated.foreach { k =>
      // Never first-match resolved: both presentations are listed, in record order.
      assertEquals(k.trials.map(_.occurrence), Vector(Some("1"), Some("2")), k.key.render)
      k.trials.foreach(t =>
        assertEquals(t.records.size, KeyStudies.recordsPerTrial, k.key.render)
      )
      assert(k.trials(0).records.last < k.trials(1).records.head, k.key.render)
    }
    // Every record of a repeated key is listed exactly once: none is dropped.
    val listed = r.repeated.flatMap(_.trials.flatMap(_.records))
    assertEquals(listed.distinct.size, listed.size)
    assertEquals(listed.size, 38 * 2 * KeyStudies.recordsPerTrial)
    val lines = text.linesIterator.drop(1).toVector
    val keyed = lines.zipWithIndex.collect {
      case (line, i)
          if KeyStudies.repeatedKeys
            .exists(k => line.startsWith(s"${k.participant},${k.phase},${k.trial},")) =>
        i + 1
    }
    assertEquals(listed.sorted, keyed)
  }

  test("P01 · Encoding · enc_01: records 1–3 are occurrence 1, records 55–57 occurrence 2") {
    val r = report(
      SourceRole.Fixations,
      KeyStudies.fixations(blocks = true),
      boardKeyWithoutOccurrence,
      block
    )
    val k = r.repeated
      .find(_.key == KeyLabel("P01", "Encoding", "enc_01"))
      .getOrElse(fail("no P01 key"))
    assertEquals(
      k.trials,
      Vector(KeyTrial(Some("1"), Vector(1, 2, 3)), KeyTrial(Some("2"), Vector(55, 56, 57)))
    )
    // Without an occurrence column eyes4s keys both as occurrence 1: one key.
    assertEquals(
      k.trials.map(k.trialKey(_, keyHasOccurrence = false)).distinct,
      Vector(TrialKey("P01", Phase.Encoding, "enc_01", 1))
    )
  }

  test("with Occurrence the keys still repeat: eyes4s identifies a trial by its label") {
    val r = report(SourceRole.Fixations, KeyStudies.fixations(blocks = true), boardKey, block)
    // A label naming two occurrences is an eyes4s occurrence conflict, not two trials.
    assertEquals(r.keys, 922)
    assertEquals(r.repeated.size, 38)
    assert(!r.repeatsWithoutOccurrence)
    assertEquals(r.occurrences, Vector(1, 2))
    assertEquals(r.laterPresentations, 38)
    val k = r.repeated.head
    assertEquals(
      k.trials.map(k.trialKey(_, keyHasOccurrence = true)),
      Vector(
        TrialKey(k.key.participant, Phase(k.key.phase), k.key.trial, 1),
        TrialKey(k.key.participant, Phase(k.key.phase), k.key.trial, 2)
      )
    )
  }

  test("a key's records with nothing to split them are one trial: no repeat can be seen") {
    val r = report(
      SourceRole.Fixations,
      KeyStudies.fixations(blocks = true),
      boardKeyWithoutOccurrence,
      TrialUnit.Key
    )
    assertEquals(r.keys, 922)
    assertEquals(r.repeated, Vector.empty)
  }

  test("the inventory: each record is a trial, so a key on two records repeats with both") {
    val r = report(
      SourceRole.Trials,
      KeyStudies.inventory(blocks = true),
      inventoryKey,
      TrialUnit.Record
    )
    assertEquals(r.records, 960)
    assertEquals(r.keys, 922)
    assertEquals(r.repeated.map(_.key), KeyStudies.repeatedKeys.sorted)
    val p01 = r.repeated.head
    assertEquals(p01.key, KeyLabel("P01", "Encoding", "enc_01"))
    assertEquals(
      p01.trials,
      Vector(KeyTrial(Some("1"), Vector(1)), KeyTrial(Some("2"), Vector(19)))
    )
  }

  test("identical inventory records of one key are both reported") {
    val text =
      """|participant,phase,trial,item
         |P01,Encoding,enc_01,a
         |P01,Encoding,enc_02,b
         |P01,Encoding,enc_01,a
         |""".stripMargin
    val key = inventoryKey.copy(occurrence = None)
    val r   = report(SourceRole.Trials, text, key, TrialUnit.Record)
    assertEquals(r.keys, 2)
    assertEquals(
      r.repeated,
      Vector(
        RepeatedKey(
          KeyLabel("P01", "Encoding", "enc_01"),
          Vector(KeyTrial(None, Vector(1)), KeyTrial(None, Vector(3)))
        )
      )
    )
  }

  // --- records that resolve to no key ---------------------------------------------------

  test("blank key fields and bad occurrences resolve to no key, each record and column named") {
    val text =
      """|participant,phase,trial,occ
         |P01,Encoding,enc_01,1
         |,Encoding,enc_02,1
         |P01, ,enc_03,1
         |P01,Encoding,enc_04,0
         |P01,Encoding,enc_05,x
         |P01,Encoding,enc_06,+2
         |,Encoding,enc_07,
         |""".stripMargin
    val key = KeyColumns(
      Some(name("participant")),
      Some(name("phase")),
      Some(name("trial")),
      Some(name("occ"))
    )
    val fx = report(SourceRole.Fixations, text, key, TrialUnit.Occurrence(name("occ")))
    assertEquals(
      fx.unresolved.map(u => (u.record, u.column.value, u.value, u.requirement)),
      Vector(
        (2, "participant", "", TrialKeyCheck.nonBlank),
        (3, "phase", " ", TrialKeyCheck.nonBlank),
        (4, "occ", "0", TrialKeyCheck.positiveOccurrence),
        (5, "occ", "x", TrialKeyCheck.positiveOccurrence),
        (7, "participant", "", TrialKeyCheck.nonBlank),
        (7, "occ", "", TrialKeyCheck.positiveOccurrence)
      )
    )
    // eyes4s reads "+2" as occurrence 2 in fixation records...
    assertEquals(fx.keys, 2)
    assertEquals(fx.occurrences, Vector(1, 2))
    // ...and refuses it in a trial inventory, which takes decimal digits only.
    val inv = report(SourceRole.Trials, text, key, TrialUnit.Record)
    assert(
      inv.unresolved.exists(u => u.record == 6 && u.value == "+2"),
      inv.unresolved.toString
    )
    assertEquals(inv.keys, 1)
    assert(!inv.unique)
  }

  // --- gaps -------------------------------------------------------------------------------

  test("a key without a phase column cannot be checked; the gap names the part") {
    val key = boardKey.copy(phase = None)
    assertEquals(
      TrialKeyCheck.check(
        "f.csv",
        SourceRole.Fixations,
        table("f.csv", KeyStudies.fixations(false)),
        key,
        block
      ),
      Left(KeyGap.Missing("f.csv", Vector(KeyPart.Phase)))
    )
    assertEquals(key.missing, Vector(KeyPart.Phase))
    assert(KeyGap.Missing("f.csv", Vector(KeyPart.Phase)).message.contains("no phase column"))
  }

  test("a key column the file lacks is named") {
    val key = boardKey.copy(trial = Some(name("Nope")))
    assertEquals(
      TrialKeyCheck.check(
        "f.csv",
        SourceRole.Fixations,
        table("f.csv", KeyStudies.fixations(false)),
        key,
        block
      ),
      Left(KeyGap.Absent("f.csv", KeyPart.Trial, name("Nope")))
    )
  }

  // --- composition ------------------------------------------------------------------------

  test(
    "the key is the mapping's roles; a Block column without a role can hold the occurrence"
  ) {
    val header   = Vector("Subject", "Phase", "TrialID", "Block", "FixNum").map(name)
    val proposed = header.zip(RoleProposals.propose(header))
    assertEquals(KeyColumns.of(proposed), boardKey)
    assertEquals(KeyColumns.occurrenceCandidate(proposed), None)
    val left = proposed.map((c, ch) =>
      if c.value == "Block" then c -> ColumnChoice.Attribute else c -> ch
    )
    assertEquals(KeyColumns.of(left), boardKeyWithoutOccurrence)
    assertEquals(KeyColumns.occurrenceCandidate(left), Some(name("Block")))
    // A column whose name does not suggest an occurrence is never taken for one.
    val other =
      left.map((c, ch) => if c.value == "Block" then name("Session") -> ch else c -> ch)
    assertEquals(KeyColumns.occurrenceCandidate(other), None)
  }

  test(
    "a repeated key's Studio check names the file, the column, the count and the first key"
  ) {
    val r = report(
      SourceRole.Fixations,
      KeyStudies.fixations(true),
      boardKeyWithoutOccurrence,
      block,
      "fixations.csv"
    )
    val b = KeyBlock.RepeatWithoutOccurrence(r.file, name("Block"), r.repeated)
    assertEquals(
      b.message,
      "Studio check · fixations.csv: 38 keys repeat without Occurrence. Records with " +
        "different Block values share each key, and eyes4s would read them as one trial; " +
        "add Occurrence to the key, or give each presentation its own trial label. " +
        "First: P01 · Encoding · enc_01."
    )
  }

  // --- the table ------------------------------------------------------------------------------

  test("the key table keeps every record, numbered as the preview numbers them") {
    val text = "a,b\n\n1,x\n2\n\n3,z,extra\n"
    val t    = table("t.csv", text)
    assertEquals(t.records, 3)
    val b = t.column(name("b")).getOrElse(fail("no b"))
    // A short record's missing cell is blank; a long record's extra cell is not kept.
    assertEquals((0 until 3).map(b.value).toVector, Vector("x", "", "z"))
    assertEquals(t, table("t.csv", text))
    assertNotEquals(t, table("t.csv", "a,b\n1,x\n2,y\n3,z\n"))
  }
