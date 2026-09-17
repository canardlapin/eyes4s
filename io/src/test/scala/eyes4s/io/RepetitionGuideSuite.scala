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

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.examples.{MatchedControlFixtures, RepetitionGuide}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class RepetitionGuideSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val CosineTolerance               = 1e-12
  private val frame                         = get(Frame.screen("repetition-guide", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val header                        = Vector(
    "participant",
    "image",
    "phase",
    "fixation",
    "x_px",
    "y_px",
    "onset_us",
    "duration_us",
    "sample_count"
  )
  private val rows = MatchedControlFixtures.fixations.map(r =>
    Vector(
      r.participant,
      r.image,
      r.phase,
      r.ordinal.toString,
      r.x.toString,
      r.y.toString,
      r.onsetMicros.toString,
      r.durationMicros.toString,
      r.sampleCount.toString
    )
  )
  private val csv = Rfc4180.encode(header +: rows)

  test("the exact guide admits, saves, restores, executes and exports the rational fixture") {
    val imported = get(RepetitionGuide.admit(csv, frame))
    assertEquals(imported.sourceRows.size, 48)
    assertEquals(imported.rejected, Vector.empty)
    val output   = get(RepetitionGuide.run(imported, grid, "recall", "encode"))
    val contrast = get(output.result.scales.head.contrast)
    contrast.rows.foreach { row =>
      val id       = s"${row.key.participant}/${row.key.stimulus}/${row.key.phase}"
      val expected = MatchedControlFixtures.reductions.find(_.id == id).get
      assertEqualsDouble(get(row.difference).value, expected.difference, CosineTolerance)
      assertEquals(row.matched.get.contributing, 1)
      assertEquals(row.control.get.contributing, 2)
    }
    assert(output.savedPlan.contains("recall"))
    assert(output.table.rows.nonEmpty)
  }

  test("saved two-phase results agree with direct all-occasion repetition at each focal key") {
    val output =
      get(RepetitionGuide.run(get(RepetitionGuide.admit(csv, frame)), grid, "recall", "encode"))
    val layout = StudyKey.layout(DefinitionId.studyLayout)
    val maps   = Trials(output.result.scales.head.estimation.map { (key, value) =>
      Trial(key, (), get(value))
    })
    val spec = get(
      EvaluationSpec.of(
        "binned-cosine",
        "1",
        Vector.empty,
        Vector("value"),
        EvaluationGeometry.onGrid(grid),
        EvaluationTime.OrderFree
      )
    )
    val direct = RepetitionDesign
      .withinParticipant(layout.participant, layout.stimulus, layout.phase)
      .evaluate(maps, output.input.hash, Distribution.cosine[Px], spec)
    val reduced = get(direct.contrast[SignedDifference](FailurePolicy.RequireAll))
    get(output.result.scales.head.contrast).rows.foreach { saved =>
      val explicit = reduced.rows.find(_.key == saved.key).get
      assertEqualsDouble(
        get(saved.difference).value,
        get(explicit.difference).value,
        CosineTolerance
      )
      assertEquals(saved.matched.get.contributing, explicit.matched.get.contributing)
      assertEquals(saved.control.get.contributing, explicit.control.get.contributing)
    }
  }

  test("bad input stays inspectable and blocks a saved analysis without dropping a trial") {
    val broken   = rows.updated(0, rows.head.updated(7, "-1"))
    val imported = get(RepetitionGuide.admit(Rfc4180.encode(header +: broken), frame))
    assert(imported.rejected.nonEmpty)
    assertEquals(imported.accepted.size, 11)
    assert(RepetitionGuide.run(imported, grid, "recall", "encode").left.exists {
      case _: FixationImportError.Incomplete => true
      case _                                 => false
    })
  }
