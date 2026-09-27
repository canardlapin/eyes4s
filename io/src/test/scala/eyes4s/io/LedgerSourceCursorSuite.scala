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
import eyes4s.plan.SourceRef

class LedgerSourceCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def drain(
      contents: String,
      budget: Int,
      envelope: LedgerExecutionLimits = limits(),
      role: LedgerResourceSource = LedgerResourceSource.Primary
  ): Either[CsvTokenError, (LedgerDecodedSource, Long, Long)] =
    LedgerSourceCursor
      .start(contents, role, envelope)
      .left
      .map(CsvTokenError.Resource.apply)
      .flatMap { initial =>
        val q = get(SampleQuantum.of(budget))
        @annotation.tailrec
        def loop(
            cursor: LedgerSourceCursor,
            decoded: Long,
            hashed: Long
        ): Either[CsvTokenError, (LedgerDecodedSource, Long, Long)] = cursor.advance(q) match
          case Left(error)                                      => Left(error)
          case Right(LedgerSourceStep.More(stage, units, next)) =>
            assert(units > 0 && units <= budget)
            assertEquals(stage, cursor.stage)
            if stage == LedgerSourceStage.Decode then loop(next, decoded + units, hashed)
            else loop(next, decoded, hashed + units)
          case Right(LedgerSourceStep.Done(units, result)) =>
            assert(units > 0 && units <= budget)
            assertEquals(cursor.stage, LedgerSourceStage.Digest)
            Right((result, decoded, hashed + units))
        loop(initial, 0, 0)
      }

  test(
    "complete decoding and semantic digest agree with synchronous references at every quantum"
  ) {
    val cases = Vector(
      "",
      "h\n",
      "h\na\n",
      "a,b\n\"x\nq\",\"a\"\"b\"\r\n",
      "\ud83d\ude42,\u6f22\na,\ud800\n",
      "\"\"",
      "\"\"\n",
      ",\n,\n"
    )
    cases.foreach { text =>
      val decoded  = get(Rfc4180.decode(text))
      val header   = decoded.headOption.getOrElse(Vector.empty)
      val rows     = decoded.drop(1)
      val all      = header +: rows
      val retained = decoded.map(row => 1L + row.map(value => value.length.toLong + 1).sum).sum
      val digestUnits =
        2L * all.size + all.map(_.map(value => value.length.toLong + 2).sum).sum + 1L
      val expected = LedgerDecodedSource(header, rows, SourceRef.digest(header, rows), retained)
      Vector(1, 3, 64).foreach { budget =>
        assertEquals(
          drain(text, budget),
          Right((expected, text.length.toLong + 1, digestUnits))
        )
      }
    }
  }

  test(
    "row-count digest placement is pinned independently and ignores equivalent CSV spelling"
  ) {
    // Independent byte-wise FNV reference over record count/field digests,
    // then header, data records and the trailing DATA-record count.
    Vector("h\na\n", "\"h\"\r\n\"a\"\r\n").foreach { text =>
      assertEquals(get(drain(text, 1))._1.recordsDigest.render, "6bbe82c8ff4b17ed")
    }
  }

  test("a huge quoted field yields inside decoding and hashing, without a bulk finalizer") {
    val field = "q" * 40000
    val text  = "h\n\"" + field + "\"\n"
    val one   = get(drain(text, 1))
    assertEquals(one._1.rows, Vector(Vector(field)))
    assertEquals(one._2, text.length.toLong + 1)
    assertEquals(one._3, 40010L)
    Vector(7, 1024).foreach(b => assertEquals(drain(text, b), Right(one)))
  }

  test("late malformed CSV prevents any complete decoded-source result") {
    val text     = "wrong-header\nok\n\"bad\"x"
    val expected = Rfc4180.decode(text).left.toOption.get
    Vector(1, 3, 1024).foreach(b =>
      assertEquals(drain(text, b), Left(CsvTokenError.Csv(expected)))
    )
  }

  test("retained evidence survives scanner completion and caps are source-located") {
    val text   = "h\na\n"
    val result = get(drain(text, 1))._1
    assertEquals(result.retainedUnits, 6L)
    assert(drain(text, 1, limits(LedgerResource.RetainedEvidenceUnits -> 6L)).isRight)
    assert(drain(text, 7, limits(LedgerResource.RetainedEvidenceUnits -> 5L)).isLeft)
    assertEquals(
      drain(
        "h\na\nb\n",
        1,
        limits(LedgerResource.LogicalRecords -> 2L),
        LedgerResourceSource.Inventory
      ),
      Left(
        CsvTokenError.Resource(
          LedgerResourceError.Exceeded(
            LedgerResource.LogicalRecords,
            2,
            BigInt(3),
            LedgerResourceLocation(LedgerResourceSource.Inventory, Some(3), None)
          )
        )
      )
    )
    val initial = get(LedgerSourceCursor.start(text, LedgerResourceSource.Primary, limits()))
    val q       = get(SampleQuantum.of(1))
    assertEquals(initial.advance(q), initial.advance(q))
    assertEquals(initial.records, Vector.empty)
    assertEquals(initial.retainedUnits, 0L)
  }
