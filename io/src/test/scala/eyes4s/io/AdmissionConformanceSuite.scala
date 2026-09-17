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

import eyes4s.kernel.*

/** Public admission versus measured eyesim clipping; exact integer accounting. */
class AdmissionConformanceSuite extends munit.FunSuite:
  import AdmissionReference.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("admission", 100, 50))
  private val columns                           = get(
    FixationColumns.of("ordinal", "x", "y", "onset", "duration", "count")
  )
  private val keys   = get(FixationKeyReader.study("participant", "trial", "phase"))
  private val header = Vector(
    "id",
    "participant",
    "trial",
    "phase",
    "ordinal",
    "x",
    "y",
    "onset",
    "duration",
    "count"
  )
  private def read(input: Vector[Row], duplicateOrdinal: Boolean = false) =
    val records = input.map { r =>
      // Stable source ordinal, not current file order. The import's count is declared
      // fixation-summary support, not a fabricated recording or raw sample series.
      val ordinal = if duplicateOrdinal then 0 else rows.indexWhere(_.id == r.id)
      Vector(
        r.id,
        r.participant,
        r.trial,
        "view",
        ordinal.toString,
        r.x.toString,
        r.y.toString,
        r.onset.toString,
        r.duration.toString,
        "1"
      )
    }
    get(
      FixationCsv.read(
        Rfc4180.encode(header +: records),
        columns,
        keys,
        frame,
        TimestampUnit.Milliseconds
      )
    )

  test(
    "reference silently clips one row and retains zero duration; eyes4s accounts for all four"
  ) {
    assertEquals(referenceRetained, Vector("valid-a", "zero-duration", "valid-b"))
    val report = read(rows)
    assertEquals(report.sourceRows.map(_.head), rows.map(_.id))
    assertEquals(report.accepted.rows.map(_.key.stimulus), Vector("b"))
    assertEquals(
      report.rejected.map(_.raw.head),
      Vector("valid-a", "outside-left", "zero-duration")
    )
    assertEquals(report.rejected.map(_.rowNumber), Vector(2, 3, 4))
    assertEquals(report.accepted.rows.map(_.value.n).sum + report.rejected.size, rows.size)
    assert(report.requireComplete.isLeft)
    assert(report.rejected.head.error.isInstanceOf[FixationRowError.Trial])
    assert(report.rejected(1).error.isInstanceOf[FixationRowError.Position])
    assert(report.rejected(2).error.isInstanceOf[FixationRowError.Time])
  }

  test("valid grouped coordinates and times agree without implicit coordinate conversion") {
    val input  = Vector(rows.head, rows.last)
    val report = read(input)
    assert(report.requireComplete.isRight)
    assertEquals(report.rejected, Vector.empty)
    report.accepted.rows.zip(input).foreach { case (trial, row) =>
      val fixation = trial.value.fixations(0)
      assertEquals(fixation.centre, Pt[Unit2D.Px](row.x, row.y))
      assertEquals(fixation.span.onset, Instant.micros(row.onset * 1000))
      assertEquals(fixation.span.duration, Span.micros(row.duration * 1000))
      assertEquals(trial.value.source, None)
    }
  }

  test("reordering input does not change accepted trials or rejected identities") {
    val direct   = read(rows)
    val reversed = read(rows.reverse)
    assertEquals(reversed.accepted.rows.map(_.key), direct.accepted.rows.map(_.key))
    assertEquals(reversed.rejected.map(_.raw.head).toSet, direct.rejected.map(_.raw.head).toSet)
    assertEquals(reversed.rejected.map(_.rowNumber), Vector(3, 4, 5))
  }

  test("duplicate ordinal quarantine differs from eyesim accepting duplicate fixations") {
    val report = read(Vector(rows.head, rows.last, rows.head), duplicateOrdinal = true)
    assertEquals(report.accepted.rows.map(_.key.stimulus), Vector("b"))
    assertEquals(report.rejected.map(_.rowNumber), Vector(2, 4))
    assert(report.rejected.forall(_.error.isInstanceOf[FixationRowError.Trial]))
  }

  test("half-open upper boundary is rejected rather than silently retained") {
    val report = read(Vector(rows.head.copy(x = 100, y = 50), rows.last))
    assertEquals(report.rejected.map(_.raw.head), Vector("valid-a"))
    assert(report.rejected.head.error.isInstanceOf[FixationRowError.Position])
  }

  test("negative duration and nonfinite coordinates remain identifiable rows") {
    Vector(rows.head.copy(duration = -1), rows.head.copy(x = Double.NaN)).foreach { invalid =>
      val report = read(Vector(invalid, rows.last))
      assertEquals(report.rejected.map(_.raw.head), Vector("valid-a"))
      assertEquals(report.accepted.rows.map(_.key.stimulus), Vector("b"))
      assertEquals(report.sourceRows.size, 2)
    }
  }

  test(
    "empty table is explicit; no malformed empty fixation group or unchecked reclassification"
  ) {
    assert(referenceEmptyGroupError.nonEmpty)
    val report = read(Vector.empty)
    assertEquals(report.sourceRows, Vector.empty)
    assertEquals(report.accepted.rows, Vector.empty)
    assertEquals(report.rejected, Vector.empty)
    assert(
      FixationCsv
        .read("unrelated\n42\n", columns, keys, frame, TimestampUnit.Milliseconds)
        .isLeft
    )
  }
