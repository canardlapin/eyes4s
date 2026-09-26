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

package eyes4s.laws

import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for the record, line and fixation identities.
  *
  * `conversion` states that a data record and a CSV record are the same
  * record counted two ways: the CSV record of data record `n` is `n + 1`,
  * every CSV record is the header (record 1) or exactly one data record, and
  * the two conversions invert each other. `fixations` states the same of
  * scanpath positions (from 0) and fixation numbers (from 1). `lines` states
  * that a record layout is exact and total: the header starts on line 1, the
  * records' spans follow each other without gap or overlap and end on the
  * layout's last line, each record spans the number of lines it was given,
  * every line of a span belongs to that record, and nothing lies beyond.
  *
  * All of these are exact integer identities; no tolerance applies.
  *
  * ==Generators hit the ends of the ranges==
  *
  * [[recordValues]] and [[positionValues]] include 1 and 2, where the header
  * and the first data record meet, and the largest values, where `+ 1` would
  * overflow an `Int`. [[lineCounts]] mixes one-line records with records of
  * several lines, including the header and the last record, so a layout
  * that ignores multi-line records, or shifts them by one record, fails.
  */
object RecordIdentityLaws extends Laws:
  val recordValues: Gen[Int] =
    Gen.oneOf(Gen.oneOf(1, 2, 3, DataRecord.maximum - 1, DataRecord.maximum), Gen.posNum[Int])

  val csvValues: Gen[Int] = Gen.oneOf(Gen.oneOf(1, 2, 3, Int.MaxValue), Gen.posNum[Int])

  val positionValues: Gen[Int] =
    Gen.oneOf(
      Gen.oneOf(0, 1, ScanpathPosition.maximum),
      Gen.choose(0, ScanpathPosition.maximum)
    )

  /** The lines each CSV record of a layout occupies, the header first. */
  val lineCounts: Gen[Vector[Int]] =
    for
      header  <- Gen.frequency(4 -> Gen.const(1), 1 -> Gen.choose(2, 3))
      records <- Gen.choose(0, 40)
      counts  <- Gen.listOfN(records, Gen.frequency(5 -> Gen.const(1), 1 -> Gen.choose(2, 4)))
    yield header +: counts.toVector

  private def get[A](e: Either[RecordIdentityError, A]): A =
    e.fold(error => throw new IllegalArgumentException(error.message), identity)

  def conversion: RuleSet =
    new SimpleRuleSet(
      "recordIdentity",
      "the CSV record of data record n is n + 1, and it is that data record" -> forAll(
        recordValues
      ) { n =>
        val record = get(DataRecord.of(n))
        Prop(record.value == n) &&
        Prop(record.csv.value == n + 1) :| s"csv ${record.csv.value} of data record $n" &&
        Prop(record.csv.role == RecordRole.Data(record)) :| s"${record.csv.role}" &&
        Prop(record.csv.dataRecord == Right(record))
      },
      "every CSV record is the header or exactly one data record" -> forAll(csvValues) { c =>
        val csv = get(CsvRecord.of(c))
        csv.role match
          case RecordRole.Header =>
            Prop(c == 1) :| s"CSV record $c is the header" &&
            Prop(csv == CsvRecord.header) &&
            Prop(csv.dataRecord == Left(RecordIdentityError.HeaderRecord(csv)))
          case RecordRole.Data(record) =>
            Prop(c >= 2) :| s"CSV record $c is data record ${record.value}" &&
            Prop(record.value == c - 1) &&
            Prop(record.csv == csv) :| "the conversions invert each other"
      },
      "the conversion is one to one" -> forAll(recordValues, recordValues) { (a, b) =>
        Prop((get(DataRecord.of(a)).csv == get(DataRecord.of(b)).csv) == (a == b))
      },
      "values outside the ranges are refused, naming the value" -> forAll(
        Gen.oneOf(Gen.oneOf(0, -1, Int.MinValue, Int.MaxValue), Gen.negNum[Int])
      ) { n =>
        Prop(
          DataRecord.of(n) == Left(
            RecordIdentityError.DataRecordOutOfRange(n, DataRecord.maximum)
          )
        ) &&
        Prop(
          n >= 1 || CsvRecord.of(n) == Left(RecordIdentityError.CsvRecordNotPositive(n))
        ) &&
        Prop(n >= 1 || SourceLine.of(n.toLong).isLeft)
      }
    )

  def fixations: RuleSet =
    new SimpleRuleSet(
      "fixationIdentity",
      "the fixation number of position p is p + 1, and back" -> forAll(positionValues) { p =>
        val position = get(ScanpathPosition.of(p))
        Prop(position.value == p) &&
        Prop(position.number.value == p + 1) :| s"number ${position.number.value} of $p" &&
        Prop(position.number.position == position) &&
        Prop(get(FixationNumber.of(p + 1)) == position.number)
      },
      "positions below 0 and numbers below 1 are refused" -> forAll(
        Gen.oneOf(Gen.const(-1), Gen.const(Int.MinValue), Gen.negNum[Int])
      ) { n =>
        Prop(
          ScanpathPosition.of(n) ==
            Left(RecordIdentityError.ScanpathPositionOutOfRange(n, ScanpathPosition.maximum))
        ) &&
        Prop(
          FixationNumber.of(n + 1) == Left(RecordIdentityError.FixationNumberNotPositive(n + 1))
        )
      }
    )

  /** Record layouts: `cases` gives the lines each record occupies (the header
    * first) and the layout built from them.
    */
  def lines(cases: Gen[(Vector[Int], RecordLines)]): RuleSet =
    new SimpleRuleSet(
      "recordLines",
      "the spans follow each other from line 1 to the last line, without gap" -> forAll(cases) {
        case (counts, layout) =>
          val spans = layout.header +: (1 to layout.records)
            .map(n => get(layout.span(get(DataRecord.of(n)))))
            .toVector
          val starts = spans.map(_.first.value)
          val ends   = spans.map(_.last.value)
          Prop(layout.records == counts.size - 1) :| s"${layout.records} records of $counts" &&
          Prop(starts.head == 1L) :| s"header starts on ${starts.head}" &&
          Prop(starts.tail == ends.init.map(_ + 1)) :| s"starts $starts after ends $ends" &&
          Prop(
            ends.last == layout.lines
          ) :| s"last span ends on ${ends.last} of ${layout.lines}" &&
          Prop(spans.map(_.count) == counts.map(_.toLong)) :| s"spans ${spans.map(_.count)}"
      },
      "every line of a span belongs to its record, and nothing lies beyond" -> forAll(cases) {
        case (_, layout) =>
          val header = (layout.header.first.value to layout.header.last.value).forall(l =>
            layout.owner(get(SourceLine.of(l))) == Right(RecordRole.Header)
          )
          val data = (1 to layout.records).forall { n =>
            val record = get(DataRecord.of(n))
            val span   = get(layout.span(record))
            (span.first.value to span.last.value).forall { l =>
              val line = get(SourceLine.of(l))
              span.contains(line) && layout.owner(line) == Right(RecordRole.Data(record))
            }
          }
          val beyond     = get(SourceLine.of(layout.lines + 1))
          val lastRecord = get(DataRecord.of(layout.records + 1))
          Prop(header) :| "a header line has another owner" &&
          Prop(data) :| "a record's line has another owner" &&
          Prop(
            layout.owner(beyond) == Left(RecordIdentityError.LineBeyond(beyond, layout.lines))
          ) &&
          Prop(
            layout.span(lastRecord) ==
              Left(RecordIdentityError.RecordBeyond(lastRecord, layout.records))
          )
      },
      "a layout of one-line records puts data record n on line n + 1" -> forAll(cases) {
        case (counts, layout) =>
          Prop(layout.isUniform == counts.forall(_ == 1)) &&
          Prop(
            !layout.isUniform || (1 to layout.records).forall(n =>
              get(layout.span(get(DataRecord.of(n)))).first.value == n + 1L
            )
          ) &&
          Prop(
            !layout.isUniform || RecordLines.uniform(layout.records) == Right(layout)
          ) :| "the uniform layout of the same records is this one"
      }
    )

  /** Layouts built by [[RecordLines.of]] from generated line counts. */
  val layouts: Gen[(Vector[Int], RecordLines)] =
    lineCounts.map(counts => counts -> get(RecordLines.of(counts)))
