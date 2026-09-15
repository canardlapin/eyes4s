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

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.examples.TemporalStudyGuide
import eyes4s.kernel.*
import eyes4s.plan.*

class TemporalContrastSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val OracleTolerance                   = 1e-12
  private val frame                             = get(Frame.screen("temporal", 2, 2))
  private val grid                              = get(Grid.over(frame, 2, 2))
  private val columns                           = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  )
  private val keys  = get(FixationKeyReader.study("participant", "image", "phase"))
  private val input = get(
    get(
      FixationCsv.read(TemporalFixtures.csv, columns, keys, frame, TimestampUnit.Microseconds)
    ).requireComplete
  )
  private val epochs = input.trials.rows.map { trial =>
    val k     = trial.key
    val spans = TemporalFixtures.coverage(s"${k.participant}/${k.stimulus}/${k.phase}").map {
      case (a, b) =>
        get(Interval.of(trial.value.clock, Instant.micros(a), Instant.micros(b)))
    }
    k -> TrialEpoch(Instant.micros(0), get(ObservedCoverage.of(trial.value.clock, spans)))
  }
  test(
    "compiled guide admits the actual CSV and exports every window/scale contrast and trial ledger"
  ) {
    val output = get(TemporalStudyGuide.run(input, epochs, grid))
    val table  = output.tables.contrasts
    val rows   =
      table.rows.map(row => table.header.zip(row).toMap).filter(_("scope") == "contrast")
    assertEquals(rows.size, 96)
    TemporalFixtures.targets.foreach { target =>
      val selected = rows.filter(row =>
        row("repetition") == target.repetition && row("window") == target.window &&
          s"${row("participant")}/${row("stimulus")}/${row("phase")}" == target.key && row(
            "sigma"
          ).toDoubleOption == target.sigma
      )
      assertEquals(selected.size, 1)
      val row = selected.head
      target.difference match
        case Some(value) =>
          assertEqualsDouble(row("difference").toDouble, value, OracleTolerance)
        case None => assertEquals(row("difference"), ""); assertEquals(row("status"), "failed")
      assertEquals(row("matched_selected"), "1")
      assertEquals(row("control_selected"), "2")
      assertEquals(
        get(io.circe.parser.parse(row("plan_json"))),
        get(io.circe.parser.parse(output.savedPlan))
      )
    }
    val coverage = output.tables.coverage
    assertEquals(coverage.rows.size, 144)
    assertEquals(get(Rfc4180.decode(coverage.encode)).size, 145)
    assert(coverage.rows.forall(_.size == coverage.header.size))
    assert(table.rows.forall(_.size == table.header.size))
    val outside =
      coverage.rows.map(row => coverage.header.zip(row).toMap).filter(_("window") == "outside")
    assert(outside.forall(_("missing_us") == "150000"))
    assert(outside.forall(_("retained_us") == "0"))
  }
