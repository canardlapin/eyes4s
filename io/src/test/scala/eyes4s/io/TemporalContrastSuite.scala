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

  test("anisotropic temporal plans round-trip and export named widths in every window") {
    val choice = StudyEstimate.Anisotropic(
      get(Sigma.px(0.5)),
      get(Sigma.px(1)),
      eyes4s.surface.EdgePolicy.Renormalise
    )
    val temporal = get(TemporalStudyInput.of(input, epochs))
    val base     = get(
      StudyPlan.cosine(
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(choice),
        eyes4s.design.FailurePolicy.RequireAll
      )
    )
    val windows = Vector(
      get(StudyWindow.of("whole", get(Window.of(Span.micros(0), Span.micros(950000))))),
      get(StudyWindow.of("outside", get(Window.of(Span.micros(950000), Span.micros(1100000)))))
    )
    val repetition =
      get(RepetitionContrast.withinParticipant("recall-encode", "recall", "encode"))
    val plan = get(
      TemporalStudyPlan.of(
        base,
        temporal.reference,
        windows,
        Vector(repetition),
        FixationBoundary.ClipDuration
      )
    )
    val codec = new TemporalStudyCodec(
      get(DefinitionId.of("anisotropic-temporal", 1)),
      StudyCodecs.cosine[Unit2D.Px]
    )
    val restored = get(codec.codec.decode(get(codec.codec.encode(plan))))
    assertEquals(plan.diff(restored), Vector.empty)
    assertEquals(get(codec.codec.encode(restored)), get(codec.codec.encode(plan)))
    val result = get(restored.run(temporal))
    val table  = get(
      TemporalContrastCsv.document(restored, result, codec, ScoreColumns.similarity)
    ).contrasts
    val rows =
      table.rows.map(row => table.header.zip(row).toMap).filter(_("scope") == "contrast")
    assertEquals(rows.size, 12)
    assert(
      rows.forall(row => row("sigma_x") == "0.5" && row("sigma_y") == "1" && row("sigma") == "")
    )
    assert(rows.filter(_("window") == "whole").forall(_("status") == "ok"))
    assert(rows.filter(_("window") == "outside").forall(_("status") == "failed"))
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

  test("typed temporal coverage exports exclusion counts and indices for 0, 1 and 2 exclusions") {
    // Onsets per trial are fixed by TemporalFixtures. With a [0, 620 ms) window a
    // fixation starting at or after 620 ms retains nothing: s1/a/encode excludes
    // none, s2/a/encode excludes index 3, and s1/b/encode excludes indices 2 and 3.
    val temporal = get(TemporalStudyInput.of(input, epochs))
    val base     = get(
      StudyPlan.cosine(
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        eyes4s.design.FailurePolicy.RequireAll
      )
    )
    val window = get(StudyWindow.of("early", get(Window.of(Span.micros(0), Span.micros(620000)))))
    val plan   = get(
      TemporalStudyPlan.of(
        base,
        temporal.reference,
        Vector(window),
        Vector(get(RepetitionContrast.withinParticipant("recall-encode", "recall", "encode"))),
        FixationBoundary.ClipDuration
      )
    )
    val result = get(plan.run(temporal))
    val codec  = new TemporalStudyCodec(
      get(DefinitionId.of("exclusion-temporal", 1)),
      StudyCodecs.cosine[Unit2D.Px]
    )
    val tables   = get(BaselineExports.temporal(plan, result, codec, ScoreColumns.similarity))
    val coverage = tables.find(_.family == ResultFamily.TemporalCoverage).get
    def column(name: String) = coverage.columns.indexWhere(_.name == name)
    assertEquals(coverage.columns(column("excluded_fixation_count")).kind, ResultColumnType.Int64)
    assertEquals(
      coverage.columns(column("excluded_fixations_json")).kind,
      ResultColumnType.JsonUtf8
    )
    val expected = result.cells.flatMap { cell =>
      cell.occupancy.collect { case (key, Right(occupancy)) =>
        s"${key.participant}/${key.stimulus}/${key.phase}" -> occupancy.excludedFixations
      }
    }.toMap
    val observed = coverage.rows.collect {
      case row if row(column("status")) == ResultCell.Text("ok") =>
        val key = get(
          codec.study.keys.decode(get(io.circe.parser.parse(row(column("key_json")).text)))
        )
        val label = s"${key.participant}/${key.stimulus}/${key.phase}"
        val indices = get(
          get(io.circe.parser.parse(row(column("excluded_fixations_json")).text)).as[Vector[Int]]
        )
        assertEquals(
          row(column("excluded_fixation_count")),
          ResultCell.Integer(indices.size.toLong)
        )
        label -> indices
    }.toMap
    assertEquals(observed, expected)
    assertEquals(observed("s1/a/encode"), Vector.empty[Int])
    assertEquals(observed("s2/a/encode"), Vector(3))
    assertEquals(observed("s1/b/encode"), Vector(2, 3))
    tables.foreach { table =>
      val context = table.context.hcursor
      assertEquals(
        context.get[String]("source_schema").toOption,
        Some(TemporalContrastCsv.schemaVersion)
      )
      assert(context.downField("key_schema").focus.isDefined, clue = table.family)
    }
  }
