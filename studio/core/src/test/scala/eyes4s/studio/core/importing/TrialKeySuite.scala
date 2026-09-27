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
  * eyes4s's TrialKey semantics. A repeated key is reported with its trials,
  * never resolved by first match; counts are exact and lists bounded.
  */
class TrialKeySuite extends munit.FunSuite:

  private def name(s: String): ColumnName =
    ColumnName.of(s).fold(e => fail(e.message), identity)

  private def preview(file: String, text: String): CsvPreview =
    CsvSniffer.sniff(file, text).fold(e => fail(e.message), identity)

  private def report(
      source: SourceRole,
      text: String,
      columns: KeyColumns,
      unit: TrialUnit,
      file: String = "f.csv"
  ): KeyReport =
    val p = preview(file, text)
    assertEquals(TrialKeyCheck.unencoded(p.keys, columns, unit), Vector.empty)
    val kept = TrialKeyCheck.check(file, source, p.keys, columns, unit)
    // The streaming pass over the text gives the same report.
    assertEquals(TrialKeyCheck.stream(file, source, text, p.delimiter, columns, unit), kept)
    kept.fold(g => fail(g.message), identity)

  private def key(
      p: String,
      f: String,
      t: String,
      o: Option[String],
      i: Option[String] = None
  ) =
    KeyColumns(Some(name(p)), Some(name(f)), Some(name(t)), o.map(name), i.map(name))

  private val boardKey     = key("Subject", "Phase", "TrialID", Some("Block"))
  private val boardKeyOut  = boardKey.copy(occurrence = None)
  private val inventoryKey = key("participant", "phase", "trial", Some("Block"))
  private val block        = TrialUnit.Presentation(Some(name("Block")))

  // --- the fixture's case: every key unique ---------------------------------------

  test("golden-shaped inventory: 960 unique keys, 0 duplicates, occurrence 1 for every trial") {
    val r =
      report(
        SourceRole.Trials,
        KeyStudies.inventory(blocks = false),
        inventoryKey,
        TrialUnit.Record
      )
    assertEquals((r.records, r.keys, r.repeatedCount, r.unresolvedCount), (960, 960, 0, 0))
    assertEquals(r.occurrences, Vector(1))
    assertEquals(r.laterPresentations, 0)
    assert(r.unique)
  }

  test("golden-shaped fixations: 960 keys over 2,880 records, none repeating") {
    val r = report(SourceRole.Fixations, KeyStudies.fixations(blocks = false), boardKey, block)
    assertEquals(r.records, 960 * KeyStudies.recordsPerTrial)
    assertEquals(r.keys, 960)
    assert(r.unique)
    val out = report(SourceRole.Fixations, KeyStudies.fixations(false), boardKeyOut, block)
    assert(out.unique)
    assertEquals(out.occurrences, Vector.empty)
    assert(!out.repeatsWithoutOccurrence)
  }

  // --- the board's case: 38 keys repeat without Occurrence ---------------------------

  test("38 keys repeat without Occurrence, each with both trials and their records") {
    val text = KeyStudies.fixations(blocks = true)
    val r    = report(SourceRole.Fixations, text, boardKeyOut, block)
    assertEquals(r.keys, 922)
    assertEquals((r.repeatedCount, r.occurrenceConflicts, r.itemConflicts), (38, 38, 0))
    assertEquals(r.repeated.map(_.key), KeyStudies.repeatedKeys.sorted)
    assert(r.repeatsWithoutOccurrence)
    assertEquals(r.occurrenceLeftOut, Some(name("Block")))
    r.repeated.foreach { k =>
      // Never first-match resolved: both presentations, in record order.
      assertEquals(k.conflict, KeyConflict.Occurrences(Vector("1", "2")), k.key.render)
      assertEquals(k.trialCount, 2)
      assertEquals(k.trials.map(_.occurrence), Vector(Some("1"), Some("2")), k.key.render)
      k.trials.foreach(t => assertEquals(t.records, KeyStudies.recordsPerTrial, k.key.render))
      assert(k.trials(0).last < k.trials(1).first, k.key.render)
      assertEquals(k.records, 2 * KeyStudies.recordsPerTrial)
    }
    // The spans are the file's: every record of a repeated key is inside one.
    val lines = text.linesIterator.drop(1).toVector
    val keyed = lines.zipWithIndex.collect {
      case (line, i)
          if KeyStudies.repeatedKeys
            .exists(k => line.startsWith(s"${k.participant},${k.phase},${k.trial},")) =>
        i + 1
    }
    val spans = r.repeated.flatMap(_.trials.flatMap(t => t.first to t.last))
    assertEquals(spans.sorted, keyed)
  }

  test("P01 · Encoding · enc_01: records 1–3 are occurrence 1, records 55–57 occurrence 2") {
    val r = report(SourceRole.Fixations, KeyStudies.fixations(true), boardKeyOut, block)
    val k =
      r.repeated.find(_.key == KeyLabel("P01", "Encoding", "enc_01")).getOrElse(fail("no key"))
    assertEquals(
      k.trials,
      Vector(KeyTrial(Some("1"), None, 1, 3, 3), KeyTrial(Some("2"), None, 55, 57, 3))
    )
    assertEquals((k.first, k.last, k.records), (1, 57, 6))
    // Without an occurrence column eyes4s keys both as occurrence 1: one key.
    assertEquals(
      k.trials.map(k.trialKey(_, keyHasOccurrence = false)).distinct,
      Vector(TrialKey("P01", Phase.Encoding, "enc_01", 1))
    )
  }

  test("with Occurrence the keys still repeat: eyes4s identifies a trial by its label") {
    val r = report(SourceRole.Fixations, KeyStudies.fixations(true), boardKey, block)
    assertEquals((r.keys, r.repeatedCount, r.occurrenceConflicts), (922, 38, 38))
    assert(!r.repeatsWithoutOccurrence)
    assertEquals(r.occurrences, Vector(1, 2))
    assertEquals(r.laterPresentations, 38)
    val k = r.repeated.head
    assertEquals(
      k.trials.map(k.trialKey(_, keyHasOccurrence = true)).map(_.occurrence),
      Vector(1, 2)
    )
  }

  test("a key's records with nothing to split them are one trial: no repeat can be seen") {
    val r = report(
      SourceRole.Fixations,
      KeyStudies.fixations(true),
      boardKeyOut,
      TrialUnit.Presentation(None)
    )
    assertEquals((r.keys, r.repeatedCount), (922, 0))
  }

  // --- eyes4s's parsing and precedence ------------------------------------------------

  test("occurrences split by eyes4s's parsed value: 1, 01 and +1 are one presentation") {
    val text =
      """|p,f,t,o
         |P01,Enc,t1,1
         |P01,Enc,t1,01
         |P01,Enc,t1,+1
         |P01,Enc,t2,1
         |P01,Enc,t2,2
         |""".stripMargin
    val out = report(
      SourceRole.Fixations,
      text,
      key("p", "f", "t", None),
      TrialUnit.Presentation(Some(name("o")))
    )
    assertEquals(out.repeated.map(_.key.trial), Vector("t2"))
    assertEquals(out.repeated.head.conflict, KeyConflict.Occurrences(Vector("1", "2")))
    val in = report(
      SourceRole.Fixations,
      text,
      key("p", "f", "t", Some("o")),
      TrialUnit.Presentation(Some(name("o")))
    )
    assertEquals(in.repeated.map(_.key.trial), Vector("t2"))
    assertEquals(in.repeated.head.trials.head, KeyTrial(Some("1"), None, 4, 4, 1))
  }

  test("different items under one key are an item conflict, ahead of occurrences") {
    val text =
      """|p,f,t,o,item
         |P01,Enc,t1,1,a
         |P01,Enc,t1,1,b
         |P01,Enc,t2,1,a
         |P01,Enc,t2,2,b
         |P01,Enc,t3,1,a
         |P01,Enc,t3,2,a
         |""".stripMargin
    val k = key("p", "f", "t", Some("o"), Some("item"))
    val r = report(SourceRole.Fixations, text, k, TrialUnit.Presentation(Some(name("o"))))
    assertEquals(
      r.repeated.map(x => x.key.trial -> x.conflict),
      Vector(
        "t1" -> KeyConflict.Items(Vector("a", "b")),
        "t2" -> KeyConflict.Items(Vector("a", "b")),
        "t3" -> KeyConflict.Occurrences(Vector("1", "2"))
      )
    )
    assertEquals((r.itemConflicts, r.occurrenceConflicts), (2, 1))
    assertEquals(r.repeated.head.trials.map(_.item), Vector(Some("a"), Some("b")))
    // Leaving the occurrence out, only one-item keys are silent merges.
    val out = report(
      SourceRole.Fixations,
      text,
      k.copy(occurrence = None),
      TrialUnit.Presentation(Some(name("o")))
    )
    assertEquals((out.itemConflicts, out.occurrenceConflicts), (2, 1))
    assert(out.repeatsWithoutOccurrence)
  }

  test("the inventory: each record is a trial, so a key on two records repeats with both") {
    val r =
      report(
        SourceRole.Trials,
        KeyStudies.inventory(blocks = true),
        inventoryKey,
        TrialUnit.Record
      )
    assertEquals((r.records, r.keys, r.repeatedCount), (960, 922, 38))
    assertEquals(r.repeated.map(_.key), KeyStudies.repeatedKeys.sorted)
    val p01 = r.repeated.head
    assertEquals(p01.conflict, KeyConflict.Entries)
    assertEquals(
      p01.trials,
      Vector(KeyTrial(Some("1"), None, 1, 1, 1), KeyTrial(Some("2"), None, 19, 19, 1))
    )
  }

  // --- bounded reports --------------------------------------------------------------------

  test("a report lists the first repeated keys and failures; its counts are exact") {
    val rows = (1 to 150).flatMap(i => Vector(s"P01,Enc,t$i,1", s"P01,Enc,t$i,2")) ++
      (1 to 150).map(i => s",Enc,u$i,1")
    val text = ("p,f,t,o" +: rows).mkString("", "\n", "\n")
    val r    = report(SourceRole.Trials, text, key("p", "f", "t", None), TrialUnit.Record)
    assertEquals(r.repeatedCount, 150)
    assertEquals(r.repeated.size, TrialKeyCheck.Kept)
    assertEquals(r.repeated.map(_.key), r.repeated.map(_.key).sorted)
    assertEquals(r.unresolvedCount, 150)
    assertEquals(r.unresolved.size, TrialKeyCheck.Kept)
    assertEquals(r.unresolved.head.record, 301)
    // One key on many records lists its first trials and counts them all.
    val many = ("p,f,t" +: Vector.fill(40)("P01,Enc,t1")).mkString("", "\n", "\n")
    val one  = report(SourceRole.Trials, many, key("p", "f", "t", None), TrialUnit.Record)
    assertEquals(one.repeated.head.trialCount, 40)
    assertEquals(one.repeated.head.trials.size, TrialKeyCheck.TrialsKept)
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
    val k  = key("participant", "phase", "trial", Some("occ"))
    val fx = report(SourceRole.Fixations, text, k, TrialUnit.Presentation(Some(name("occ"))))
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
    assertEquals(fx.unresolvedCount, 5)
    // eyes4s reads "+2" as occurrence 2 in fixation records...
    assertEquals(fx.keys, 2)
    assertEquals(fx.occurrences, Vector(1, 2))
    // ...and refuses it in a trial inventory, which takes decimal digits only.
    val inv = report(SourceRole.Trials, text, k, TrialUnit.Record)
    assert(
      inv.unresolved.exists(u => u.record == 6 && u.value == "+2"),
      inv.unresolved.toString
    )
    assertEquals(inv.keys, 1)
  }

  // --- gaps -------------------------------------------------------------------------------

  test("a key without a phase column cannot be checked; the gap names the part") {
    val k = boardKey.copy(phase = None)
    val p = preview("f.csv", KeyStudies.fixations(false))
    assertEquals(
      TrialKeyCheck.check("f.csv", SourceRole.Fixations, p.keys, k, block),
      Left(KeyGap.Missing("f.csv", Vector(KeyPart.Phase)))
    )
    assert(KeyGap.Missing("f.csv", Vector(KeyPart.Phase)).message.contains("no phase column"))
  }

  test("a key column the file lacks is named") {
    val k = boardKey.copy(trial = Some(name("Nope")))
    val p = preview("f.csv", KeyStudies.fixations(false))
    assertEquals(
      TrialKeyCheck.check("f.csv", SourceRole.Fixations, p.keys, k, block),
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
    val left =
      proposed.map((c, ch) =>
        if c.value == "Block" then c -> ColumnChoice.Attribute else c -> ch
      )
    assertEquals(KeyColumns.of(left), boardKeyOut)
    assertEquals(KeyColumns.occurrenceCandidate(left), Some(name("Block")))
    val other =
      left.map((c, ch) => if c.value == "Block" then name("Session") -> ch else c -> ch)
    assertEquals(KeyColumns.occurrenceCandidate(other), None)
  }

  test("the Studio check names relabelling as the remedy, and what Occurrence would do") {
    val b = KeyBlock.RepeatWithoutOccurrence(
      "fixations.csv",
      name("Block"),
      38,
      Some(KeyLabel("P01", "Encoding", "enc_01"))
    )
    assertEquals(
      b.message,
      "Studio check · fixations.csv: 38 keys repeat without Occurrence. Records with " +
        "different Block values share each key, and eyes4s would read them as one trial. " +
        "Relabel the trials so that each presentation has its own trial label. With " +
        "Occurrence in the key the import proceeds, but eyes4s quarantines these trials " +
        "(occurrence conflict). First: P01 · Encoding · enc_01."
    )
  }

  // --- the table ------------------------------------------------------------------------------

  test(
    "the sniffer keeps low-cardinality columns only, numbered as the preview numbers records"
  ) {
    val text = "a,b\n\n1,x\n2\n\n3,z,extra\n"
    val t    = preview("t.csv", text).keys
    assertEquals(t.records, 3)
    val b = t.column(name("b")).getOrElse(fail("no b"))
    assertEquals((0 until 3).map(b.value).toVector, Vector("x", "", "z"))
    // A column past the cap is dropped, and a key on it is checked by streaming.
    val wide = ("p,f,t" +: (1 to KeyTable.Cap + 1).map(i => s"P01,Enc,t$i"))
      .mkString("", "\n", "\n")
    val w = preview("w.csv", wide)
    assertEquals(w.keys.dropped, Vector(name("t")))
    assertEquals(w.keys.columns.map(_.name), Vector(name("p"), name("f")))
    val k = key("p", "f", "t", None)
    assertEquals(TrialKeyCheck.unencoded(w.keys, k, TrialUnit.Record), Vector(name("t")))
    val streamed = TrialKeyCheck
      .stream("w.csv", SourceRole.Trials, wide, w.delimiter, k, TrialUnit.Record)
      .fold(g => fail(g.message), identity)
    assertEquals((streamed.keys, streamed.repeatedCount), (KeyTable.Cap + 1, 0))
  }
