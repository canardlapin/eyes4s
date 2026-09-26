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

import eyes4s.design.SampleQuantum

class CsvTokenCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*

  private def drain(
      text: String,
      budget: Int,
      envelope: LedgerExecutionLimits = limits()
  ): Either[CsvTokenError, Vector[Vector[String]]] =
    val quantum = SampleQuantum.of(budget).toOption.get
    CsvTokenCursor
      .start(text, LedgerResourceSource.Primary, envelope)
      .left
      .map(CsvTokenError.Resource.apply)
      .flatMap { initial =>
        @annotation.tailrec
        def loop(
            cursor: CsvTokenCursor,
            rows: Vector[Vector[String]],
            work: Long
        ): Either[CsvTokenError, Vector[Vector[String]]] =
          cursor.advance(quantum) match
            case Left(error) => Left(error)
            case Right(page) =>
              assert(page.workUnits > 0 && page.workUnits <= budget)
              page.next match
                case Some(next) =>
                  assertEquals(next.index - cursor.index, page.workUnits)
                  loop(next, rows ++ page.row, work + page.workUnits)
                case None =>
                  assertEquals(work + page.workUnits, text.length.toLong + 1)
                  Right(rows ++ page.row)
        loop(initial, Vector.empty, 0L)
      }

  test("all short quote, CR, LF and Unicode combinations match the established decoder") {
    val alphabet = Vector('a', ',', '"', '\r', '\n', 'λ')
    var cases    = Vector("")
    (0 to 4).foreach { _ =>
      cases.foreach { text =>
        val expected = Rfc4180.decode(text).left.map(CsvTokenError.Csv.apply)
        Vector(1, 2, 7).foreach { budget =>
          assertEquals(drain(text, budget), expected, s"${text.toVector} / $budget")
        }
      }
      cases = cases.flatMap(prefix => alphabet.map(prefix + _))
    }
  }

  test("a large quoted field yields inside the field and immutable snapshots replay exactly") {
    val value  = "λ,\n\"界" * 4000
    val text   = Rfc4180.encode(Vector(Vector("header"), Vector(value)))
    val cursor = CsvTokenCursor.start(text, LedgerResourceSource.Primary, limits()).toOption.get
    val quantum = SampleQuantum.of(3).toOption.get
    val first   = cursor.advance(quantum)
    assertEquals(cursor.advance(quantum), first)
    val inside = first.toOption.get.next.get
    assertEquals(inside.index, 3)
    assertEquals(inside.advance(quantum), inside.advance(quantum))
    assertEquals(cursor.index, 0)
    val afterHeader = cursor.advance(SampleQuantum.of(100).toOption.get).toOption.get.next.get
    val quoted      = afterHeader.advance(quantum).toOption.get
    assertEquals(quoted.row, None)
    assertEquals(quoted.next.get.mode, CsvTokenMode.Quoted)
    assertEquals(quoted.next.get.index - afterHeader.index, 3)
    assertEquals(afterHeader.advance(quantum), Right(quoted))
    assertEquals(drain(text, 7), Right(Vector(Vector("header"), Vector(value))))
  }

  test("source code-unit limits use UTF-16, with exact below, at and above behavior") {
    val source = "😀"
    assertEquals(source.length, 2)
    assert(drain(source, 1, limits(LedgerResource.SourceCodeUnits -> 1L)).isLeft)
    assertEquals(
      drain(source, 1, limits(LedgerResource.SourceCodeUnits -> 2L)),
      Right(Vector(Vector(source)))
    )
    assertEquals(
      drain(source, 1, limits(LedgerResource.SourceCodeUnits -> 3L)),
      Right(Vector(Vector(source)))
    )
  }

  test(
    "each scanner resource is refused at its first excess, without returning a partial row"
  ) {
    val examples = Vector(
      ("a", LedgerResource.SourceCodeUnits, 1L),
      ("a", LedgerResource.LogicalRecords, 1L),
      ("a\r\n", LedgerResource.EncodedRecordCodeUnits, 3L),
      ("\"a\"\"b\"", LedgerResource.FieldCodeUnits, 3L),
      ("a,b", LedgerResource.Columns, 2L),
      ("a", LedgerResource.RetainedEvidenceUnits, 3L)
    )
    examples.foreach { (text, dimension, extent) =>
      val expected = Rfc4180.decode(text).left.map(CsvTokenError.Csv.apply)
      Vector(1, 2, 99).foreach { budget =>
        assertEquals(drain(text, budget, limits(dimension -> extent)), expected)
        assertEquals(drain(text, budget, limits(dimension -> (extent + 1))), expected)
        drain(text, budget, limits(dimension -> (extent - 1))) match
          case Left(
                CsvTokenError.Resource(
                  LedgerResourceError.Exceeded(actual, bound, observed, at)
                )
              ) =>
            assertEquals(actual, dimension)
            assertEquals(bound, extent - 1)
            assertEquals(observed, BigInt(extent))
            assertEquals(at.source, LedgerResourceSource.Primary)
          case other => fail(s"expected resource refusal: $dimension -> $other")
      }
    }
  }

  test(
    "wide headers and huge fields stop before scanning or rendering their remaining content"
  ) {
    val wide   = "a," + "x," * 10000
    val cursor = CsvTokenCursor
      .start(wide, LedgerResourceSource.Inventory, limits(LedgerResource.Columns -> 1L))
      .toOption
      .get
    val error = cursor.advance(SampleQuantum.of(100000).toOption.get).left.toOption.get
    assertEquals(
      error,
      CsvTokenError.Resource(
        LedgerResourceError.Exceeded(
          LedgerResource.Columns,
          1,
          2,
          LedgerResourceLocation(LedgerResourceSource.Inventory, Some(1), Some(2))
        )
      )
    )
    val huge    = "\"" + ("x" * 100000)
    val tooLong = drain(huge, 1, limits(LedgerResource.FieldCodeUnits -> 4L))
    tooLong match
      case Left(CsvTokenError.Resource(error)) =>
        assert(error.message.length < 220)
        assert(!error.message.contains("xxxxx"))
      case other => fail(s"expected resource refusal, got $other")
  }
