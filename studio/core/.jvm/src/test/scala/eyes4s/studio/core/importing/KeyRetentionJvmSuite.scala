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

import eyes4s.studio.core.document.{ColumnName, SourceRole}

/** Retention is bounded (ticket S5.3). What a [[KeyTable]] keeps is
  * computed, not measured on the heap: 2 bytes × records × kept columns for
  * the codes (each kept column holds one `Short` a record), plus the kept
  * columns' dictionaries, at most [[KeyTable.Cap]] values each
  * ([[KeyTable.footprint]] adds them up). A 500,000-record fixation table
  * keeps only its low-cardinality columns; the worst case keeps every column;
  * and the key report lists a bounded number of repeated keys however many
  * there are.
  */
class KeyRetentionJvmSuite extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def name(s: String): ColumnName = get(ColumnName.of(s))

  private val records = 500000

  /** 50 participants × 2 phases × 125 trials × 2 blocks, 20 records each; positions,
    * onsets and durations take many values; every trial is shown twice
    * (Block 1 and 2), so every key repeats without Occurrence.
    */
  private lazy val text: String =
    val b = new StringBuilder(records * 64)
    b ++= "Subject,Phase,TrialID,Block,FixNum,FixX,FixY,FixStart,FixDur\n"
    var i = 0
    while i < records do
      val trial = i / 20
      b ++= f"P${trial % 50}%02d,${if (trial / 50) % 2 == 0 then "Enc" else "Ret"},"
      b ++= s"t${(trial / 100) % 125},${1 + trial / 12500},${i % 20 + 1},"
      b ++= s"${100.0 + i * 0.37},${200.0 + i * 0.53},${i * 7},${150 + i % 9000}\n"
      i += 1
    b.result()

  test("500,000 records keep the key roles only, at two bytes a record") {
    val p    = get(CsvSniffer.sniff("fixations.csv", text))
    val keys = p.keys
    assertEquals(keys.records, records)
    assertEquals(
      keys.columns.map(_.name.value),
      Vector("Subject", "Phase", "TrialID", "Block", "FixNum")
    )
    assertEquals(
      keys.dropped.map(_.value),
      Vector("FixX", "FixY", "FixStart", "FixDur")
    )
    keys.columns.foreach { c =>
      assertEquals(c.size, records, c.toString)
      assert(c.values.size <= KeyTable.Cap, c.toString)
    }
    // 2 bytes × 500,000 records × 5 kept columns, plus their dictionaries.
    assertEquals(keys.footprint, 2L * records * 5 + dictionaries(keys))
    assert(dictionaries(keys) < 100_000, dictionaries(keys).toString)
    // The whole text, as UTF-16 chars, is over ten times larger.
    assert(keys.footprint * 10 < 2L * text.length, s"${keys.footprint} vs ${text.length}")
  }

  /** The dictionaries' share of [[KeyTable.footprint]]. */
  private def dictionaries(keys: KeyTable): Long =
    keys.columns.map(c => c.footprint - 2L * c.size).sum

  test("worst case: every column kept, each at the cap, still 2 bytes a record per column") {
    // Nine columns, each with exactly Cap distinct values, all kept.
    val columns = 9
    val b       = new StringBuilder(records * 60)
    b ++= (1 to columns).map(c => s"c$c").mkString("", ",", "\n")
    var i = 0
    while i < records do
      b ++= (1 to columns).map(c => s"v$c-${(i + c) % KeyTable.Cap}").mkString("", ",", "\n")
      i += 1
    val keys = get(CsvSniffer.sniff("wide.csv", b.result())).keys
    assertEquals(keys.dropped, Vector.empty)
    assertEquals(keys.columns.size, columns)
    keys.columns.foreach(c => assertEquals(c.values.size, KeyTable.Cap, c.toString))
    assertEquals(keys.footprint, 2L * records * columns + dictionaries(keys))
    // The dictionaries are bounded by the cap, whatever the record count.
    assert(dictionaries(keys) <= columns.toLong * KeyTable.Cap * (40 + 2 * 12))
    assert(keys.footprint <= 2L * records * columns + columns.toLong * KeyTable.Cap * 64)
  }

  test("the report is bounded: exact counts, the first keys listed") {
    val p   = get(CsvSniffer.sniff("fixations.csv", text))
    val key =
      KeyColumns(Some(name("Subject")), Some(name("Phase")), Some(name("TrialID")), None)
    val r = get(
      TrialKeyCheck.check(
        "fixations.csv",
        SourceRole.Fixations,
        p.keys,
        key,
        TrialUnit.Presentation(Some(name("Block")))
      )
    )
    assertEquals(r.records, records)
    assert(r.repeatedCount > TrialKeyCheck.Kept, r.repeatedCount.toString)
    assertEquals(r.repeated.size, TrialKeyCheck.Kept)
    assert(r.repeatsWithoutOccurrence)
  }
