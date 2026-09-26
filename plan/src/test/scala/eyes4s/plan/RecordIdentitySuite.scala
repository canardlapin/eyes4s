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

import scala.compiletime.testing.typeCheckErrors

/** The record, line and fixation identities: the counting conventions are
  * types, so one cannot be passed for another, and every refusal names the
  * value it refused.
  */
class RecordIdentitySuite extends munit.FunSuite:
  private def get[A](e: Either[RecordIdentityError, A]): A =
    e.fold(error => fail(error.message), identity)

  test("a data record cannot be passed where a CSV record is expected, nor the reverse") {
    val dataForCsv = typeCheckErrors("""
      import eyes4s.plan.*
      def roleOf(record: CsvRecord): RecordRole = record.role
      val r: DataRecord = DataRecord.of(7214).toOption.get
      roleOf(r)
    """)
    assert(dataForCsv.nonEmpty, "a DataRecord was accepted as a CsvRecord")
    assert(dataForCsv.exists(_.message.contains("CsvRecord")), dataForCsv.map(_.message))
    val csvForData = typeCheckErrors("""
      import eyes4s.plan.*
      def show(record: DataRecord): Int = record.value
      show(CsvRecord.header)
    """)
    assert(csvForData.nonEmpty, "a CsvRecord was accepted as a DataRecord")
    val checked = typeCheckErrors("""
      import eyes4s.plan.*
      def roleOf(record: CsvRecord): RecordRole = record.role
      roleOf(DataRecord.of(7214).toOption.get.csv)
    """)
    assertEquals(checked.map(_.message), Nil)
  }

  test("an Int, a position or a record is not another identity") {
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val r: DataRecord = 7214""").nonEmpty,
      """import eyes4s.plan.*
         val r: DataRecord = 7214"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val n: FixationNumber = ScanpathPosition.of(5).toOption.get""").nonEmpty,
      """import eyes4s.plan.*
         val n: FixationNumber = ScanpathPosition.of(5).toOption.get"""
    )
    assert(
      typeCheckErrors("""import eyes4s.plan.*
         val l: SourceLine = DataRecord.of(1).toOption.get""").nonEmpty,
      """import eyes4s.plan.*
         val l: SourceLine = DataRecord.of(1).toOption.get"""
    )
  }

  test("fixture record 7,214 is CSV record 7,215; fixation 6 is scanpath position 5") {
    val record = get(DataRecord.of(7214))
    assertEquals(record.csv, get(CsvRecord.of(7215)))
    assertEquals(get(CsvRecord.of(7215)).role, RecordRole.Data(record))
    assertEquals(CsvRecord.header.role, RecordRole.Header)
    assertEquals(get(FixationNumber.of(6)).position, get(ScanpathPosition.of(5)))
    assertEquals(get(ScanpathPosition.of(5)).number.value, 6)
    assertEquals(
      Vector(7, 1, 7214).map(n => get(DataRecord.of(n))).sorted.map(_.value),
      Vector(1, 7, 7214)
    )
  }

  test("every refusal names the value it refused") {
    val header = CsvRecord.header
    val first  = get(SourceLine.of(4))
    val last   = get(SourceLine.of(3))
    val cases: Vector[(Either[RecordIdentityError, Any], RecordIdentityError, String)] = Vector(
      (
        DataRecord.of(0),
        RecordIdentityError.DataRecordOutOfRange(0, DataRecord.maximum),
        "Data record 0"
      ),
      (CsvRecord.of(0), RecordIdentityError.CsvRecordNotPositive(0), "CSV record 0"),
      (
        header.dataRecord,
        RecordIdentityError.HeaderRecord(header),
        "CSV record 1 is the header"
      ),
      (SourceLine.of(0L), RecordIdentityError.SourceLineNotPositive(0L), "Source line 0"),
      (
        LineSpan.of(first, last),
        RecordIdentityError.LineSpanOrder(first, last),
        "line 3, before its first line 4"
      ),
      (
        ScanpathPosition.of(-1),
        RecordIdentityError.ScanpathPositionOutOfRange(-1, ScanpathPosition.maximum),
        "Scanpath position -1"
      ),
      (FixationNumber.of(0), RecordIdentityError.FixationNumberNotPositive(0), "number 0"),
      (
        RecordLines.uniform(-3),
        RecordIdentityError.RecordCountOutOfRange(-3, DataRecord.maximum),
        "not -3"
      ),
      (RecordLines.of(Vector.empty), RecordIdentityError.NoHeaderRecord, "no records"),
      (
        RecordLines.of(Vector(1, 1, 0)),
        RecordIdentityError.RecordLineCount(get(CsvRecord.of(3)), 0),
        "CSV record 3 is said to occupy 0 lines"
      ),
      (
        get(RecordLines.uniform(2)).span(get(DataRecord.of(3))),
        RecordIdentityError.RecordBeyond(get(DataRecord.of(3)), 2),
        "Data record 3 is beyond the layout's 2"
      ),
      (
        get(RecordLines.uniform(2)).owner(get(SourceLine.of(4))),
        RecordIdentityError.LineBeyond(get(SourceLine.of(4)), 3L),
        "Line 4 is beyond the layout's 3 lines"
      )
    )
    cases.foreach { (result, error, text) =>
      assertEquals(result, Left(error))
      assert(error.message.contains(text), s"${error.message} does not name '$text'")
    }
  }

  test("a line span counts and contains its lines, both ends included") {
    val s = get(LineSpan.of(get(SourceLine.of(3)), get(SourceLine.of(5))))
    assertEquals(s.count, 3L)
    assert(s.contains(get(SourceLine.of(3))) && s.contains(get(SourceLine.of(5))))
    assert(!s.contains(get(SourceLine.of(2))) && !s.contains(get(SourceLine.of(6))))
  }

  test("layouts compare by their records' lines") {
    val a = get(RecordLines.of(Vector(1, 2, 1)))
    val b = get(RecordLines.of(Vector(1, 2, 1)))
    val c = get(RecordLines.of(Vector(1, 1, 2)))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
    assertNotEquals(a, c)
    assertNotEquals[Any, Any](a, "RecordLines")
    assertEquals(Set(a, b, c).size, 2)
    assertEquals(get(RecordLines.uniform(2)), get(RecordLines.of(Vector(1, 1, 1))))
    assertEquals(a.toString, "RecordLines(2 data records, 4 lines, 1 spanning more than one)")
  }

  test("ledger and source-index record numbers read as typed identities") {
    val key      = "trial"
    val admitted = SourceRecord[String](3, Disposition.Admitted(key, 0))
    assertEquals(admitted.csvRecord, Right(get(CsvRecord.of(3))))
    assertEquals(admitted.dataRecord, Right(get(DataRecord.of(2))))
    val atHeader = SourceRecord[String](1, Disposition.Admitted(key, 0))
    assertEquals(atHeader.dataRecord, Left(RecordIdentityError.HeaderRecord(CsvRecord.header)))
    assertEquals(
      SourceRecord[String](0, Disposition.Admitted(key, 0)).dataRecord,
      Left(RecordIdentityError.CsvRecordNotPositive(0))
    )
    val fixation = FixationSource(key, 5, 7215, 6)
    assertEquals(fixation.dataRecord, Right(get(DataRecord.of(7214))))
    assertEquals(fixation.position.map(_.number.value), Right(6))
    assertEquals(
      FixationSource(key, -1, 1, 0).position,
      Left(RecordIdentityError.ScanpathPositionOutOfRange(-1, ScanpathPosition.maximum))
    )
    assertEquals(
      FixationSource(key, 0, 1, 0).dataRecord,
      Left(RecordIdentityError.HeaderRecord(CsvRecord.header))
    )
  }
