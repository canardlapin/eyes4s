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

import eyes4s.codec.StudyInputFixtures
import eyes4s.kernel.*
import eyes4s.plan.*

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** A CSV text's record layout is the decoder's: the same records, in the same
  * places, on exactly the physical lines they occupy, with quoted line breaks
  * (LF and CRLF) spanning lines and a lone carriage return not ending one.
  */
class CsvLayoutSuite extends munit.ScalaCheckSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def data(n: Int): DataRecord      = get(DataRecord.of(n))
  private def line(n: Long): SourceLine     = get(SourceLine.of(n))
  private def span(first: Long, last: Long) = get(LineSpan.of(line(first), line(last)))

  /** Physical line `n` of a text, without its terminator's line feed. */
  private def physical(text: String): Vector[String] = text.split("\n", -1).toVector

  /** The text of lines `span`, joined by the line feeds that end them, as the
    * record's verbatim text (a CRLF's carriage return is its terminator's).
    */
  private def linesOf(text: String, span: LineSpan): String =
    physical(text)
      .slice((span.first.value - 1).toInt, span.last.value.toInt)
      .mkString("\n")
      .stripSuffix("\r")

  // A pinned layout. Record 2 holds an LF, record 3 a lone CR (not a line
  // break), record 4 a CRLF and an LF; record 5 has no terminator.
  private val pinned =
    "id,x,note\r\n" +
      "1,10,plain\r\n" +
      "2,20,\"two\nlines\"\r\n" +
      "3,30,\"cr\ronly\"\n" +
      "4,40,\"a\r\nb\nc\"\n" +
      "5,50,last"

  test("a pinned text with quoted line breaks: every record's lines, and every line's record") {
    val layout = get(CsvLayout.scan(pinned))
    assertEquals(layout.records, 5)
    assertEquals(layout.header, "id,x,note")
    assertEquals(layout.lines.header, span(1, 1))
    assertEquals(layout.lines.lines, 9L)
    assert(!layout.lines.isUniform)
    val spans = Vector(span(2, 2), span(3, 4), span(5, 5), span(6, 8), span(9, 9))
    assertEquals((1 to 5).map(n => get(layout.lines.span(data(n)))).toVector, spans)
    val owners = (1L to 9L).map(l => get(layout.lines.owner(line(l)))).toVector
    assertEquals(
      owners,
      Vector(RecordRole.Header) ++ Vector(1, 2, 2, 3, 4, 4, 4, 5).map(n =>
        RecordRole.Data(data(n))
      )
    )
    assertEquals(
      layout.lines.owner(line(10)),
      Left(RecordIdentityError.LineBeyond(line(10), 9L))
    )
    assertEquals(get(layout.verbatim(data(2))), "2,20,\"two\nlines\"")
    assertEquals(get(layout.verbatim(data(4))), "4,40,\"a\r\nb\nc\"")
    assertEquals(get(layout.verbatim(data(5))), "5,50,last")
    assertEquals(
      layout.verbatim(data(6)),
      Left(RecordIdentityError.RecordBeyond(data(6), 5))
    )
    // Round trips both ways, and the decoder reads the same records.
    (1 to 5).foreach { n =>
      val s = get(layout.lines.span(data(n)))
      (s.first.value to s.last.value).foreach(l =>
        assertEquals(layout.lines.owner(line(l)), Right(RecordRole.Data(data(n))))
      )
      assertEquals(get(layout.verbatim(data(n))), linesOf(pinned, s))
      assertEquals(
        get(Rfc4180.decode(get(layout.verbatim(data(n))))),
        Vector(get(Rfc4180.decode(pinned))(n))
      )
    }
    // The physical lines drift from the data records after record 2.
    assertEquals(physical(pinned)(4), "3,30,\"cr\ronly\"")
  }

  test("a fixation import's ledger records are the layout's records, drift and all") {
    val header = StudyInputFixtures.header :+ "note"
    val rows   = StudyInputFixtures.records.zipWithIndex.map { case (fields, i) =>
      fields :+ (if i % 5 == 0 then "reviewed,\nsee notebook" else "")
    }
    val text    = Rfc4180.encode(header +: rows)
    val frame   = get(Frame.screen("matched-control-display", 2, 2))
    val columns = get(
      FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
    )
    val keys     = get(FixationKeyReader.study("participant", "image", "phase"))
    val imported = get(FixationCsv.read(text, columns, keys, frame, TimestampUnit.Microseconds))
    val ledger   = get(
      FixationEvidence.ledger(
        "matched-control.csv",
        imported,
        AdmissionDecision.RequireComplete
      )
    )
    val input   = get(imported.requireComplete)
    val sources = get(StudySources.of(input, ledger))
    val layout  = get(CsvLayout.scan(text))
    val decoded = get(Rfc4180.decode(text))
    assertEquals(layout.records, ledger.records.size)
    assertEquals(layout.header, Rfc4180.encode(Vector(header)).stripSuffix("\r\n"))
    ledger.records.foreach { entry =>
      val record = get(entry.dataRecord)
      assertEquals(get(entry.csvRecord).value, entry.record)
      assertEquals(sources.entryAt(record), Some(entry))
      assertEquals(
        get(Rfc4180.decode(get(layout.verbatim(record)))),
        Vector(decoded(record.value))
      )
      assertEquals(get(layout.verbatim(record)), linesOf(text, get(layout.lines.span(record))))
    }
    // Every admitted fixation reaches its record's text and lines.
    input.trials.rows.foreach { row =>
      (0 until row.value.n).foreach { i =>
        val position = get(ScanpathPosition.of(i))
        val source   = get(sources.fixationAt(row.key, position))
        val record   = get(source.dataRecord)
        assertEquals(get(source.position), position)
        assertEquals(sources.trialAt(record), Some(row.key))
        val fields = get(Rfc4180.decode(get(layout.verbatim(record)))).head
        assertEquals(
          fields.take(4),
          Vector(
            row.key.participant,
            row.key.stimulus,
            row.key.phase,
            source.ordinal.toString
          )
        )
      }
    }
    // The notes make lines and records disagree: the last record's line is
    // not its data record plus one.
    val last = data(layout.records)
    assert(get(layout.lines.span(last)).first.value > last.value + 1L)
  }

  // ------------------------------------------------------------ generated

  private val fieldText: Gen[String] =
    Gen
      .listOfN(
        4,
        Gen.frequency(
          6 -> Gen.alphaNumChar.map(_.toString),
          1 -> Gen.const(","),
          1 -> Gen.const("\""),
          1 -> Gen.const("\n"),
          1 -> Gen.const("\r\n"),
          1 -> Gen.const("\r")
        )
      )
      .flatMap(parts => Gen.choose(0, parts.size).map(n => parts.take(n).mkString))

  private def quote(value: String): String =
    if value.exists(c => c == ',' || c == '"' || c == '\r' || c == '\n') then
      "\"" + value.replace("\"", "\"\"") + "\""
    else value

  /** A well-formed text with its records, LF or CRLF terminators and an
    * optional final terminator.
    */
  private val wellFormed: Gen[(String, Vector[Vector[String]])] =
    for
      width      <- Gen.choose(1, 3)
      n          <- Gen.choose(1, 12)
      rows       <- Gen.listOfN(n, Gen.listOfN(width, fieldText).map(_.toVector))
      ends       <- Gen.listOfN(n, Gen.oneOf("\n", "\r\n"))
      terminated <- Gen.oneOf(true, false)
    yield
      val lines = rows.toVector.map(_.map(quote).mkString(","))
      val body  = lines.zip(ends).map(_ + _).mkString
      (if terminated then body else body.stripSuffix(ends.last)) -> rows.toVector

  property("generated texts: the decoder's records, on the lines their quoted breaks give") {
    forAll(wellFormed) { case (text, _) =>
      val decoded = get(Rfc4180.decode(text))
      CsvLayout.scan(text) match
        case Left(CsvLayoutError.Layout(RecordIdentityError.NoHeaderRecord)) =>
          assertEquals(decoded, Vector.empty)
        case Left(error)   => fail(error.message)
        case Right(layout) =>
          assertEquals(layout.records, decoded.size - 1)
          val header = decoded.head
          assertEquals(
            get(Rfc4180.decode(layout.header)).headOption.getOrElse(Vector("")),
            header
          )
          val counts = (0 to layout.records).map { n =>
            val verbatim =
              if n == 0 then layout.header else get(layout.verbatim(data(n)))
            // A blank record decodes to no row; it is the one empty field.
            val row = get(Rfc4180.decode(verbatim)).headOption.getOrElse(Vector(""))
            assertEquals(row, decoded(n))
            if n > 0 then assertEquals(verbatim, linesOf(text, get(layout.lines.span(data(n)))))
            verbatim.count(_ == '\n') + 1
          }.toVector
          assertEquals(layout.lines, get(RecordLines.of(counts)))
    }
  }

  private val anyText: Gen[String] =
    Gen
      .listOfN(
        24,
        Gen.oneOf("a", "b", ",", "\"", "\"\"", "\n", "\r", "\r\n")
      )
      .flatMap(parts => Gen.choose(0, parts.size).map(n => parts.take(n).mkString))
      .flatMap(text => Gen.oneOf("", "\n\"\"", "\r\n\"\"", "\"\"").map(text + _))

  property("any text: the layout refuses exactly what the decoder refuses, with its error") {
    forAll(anyText) { text =>
      (Rfc4180.decode(text), CsvLayout.scan(text)) match
        case (Left(error), Left(CsvLayoutError.Csv(same))) => assertEquals(same, error)
        case (Right(rows), Right(layout))                  =>
          assertEquals(layout.records, rows.size - 1)
        case (Right(rows), Left(CsvLayoutError.Layout(RecordIdentityError.NoHeaderRecord))) =>
          assertEquals(rows, Vector.empty)
        case (decoded, scanned) => fail(s"decoder $decoded, layout $scanned")
    }
  }

  test(
    "the decoder's edge records: a trailing empty quoted field is no record, a blank line is"
  ) {
    val edges = Vector(
      "h\n\"\""      -> 0, // a lone "" after the last terminator holds no character
      "h\n\"\",\"\"" -> 1, // two empty fields are a record
      "h\n\n"        -> 1, // a blank line is a record of one empty field
      "h\n\r"        -> 1, // a lone CR is a character, not a terminator
      "h\r\n"        -> 0
    )
    edges.foreach { (text, records) =>
      assertEquals(get(CsvLayout.scan(text)).records, records, text)
      assertEquals(get(Rfc4180.decode(text)).size - 1, records, text)
    }
  }

  test("refusals name the decoder's error or the missing header") {
    val malformed = CsvLayout.scan("a,b\"c\n")
    assertEquals(malformed, Left(CsvLayoutError.Csv(TidyCsvError.MalformedCsv(3, '"'))))
    assertEquals(
      CsvLayout.scan("a,\"b\n"),
      Left(CsvLayoutError.Csv(TidyCsvError.UnterminatedQuotedField(5)))
    )
    val empty = CsvLayout.scan("")
    assertEquals(empty, Left(CsvLayoutError.Layout(RecordIdentityError.NoHeaderRecord)))
    assert(malformed.left.exists(_.message.contains("'\"'")), malformed.toString)
    assert(empty.left.exists(_.message.contains("no records")), empty.toString)
    assertEquals(
      get(CsvLayout.scan("h")).toString,
      "CsvLayout(RecordLines(0 data records, 1 lines, 0 spanning more than one))"
    )
  }
