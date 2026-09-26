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

package eyes4s.templateconsumer

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class LearnedTemplateSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val LearnedTolerance                  = 1e-12
  private val frame                             = get(Frame.screen("template", 2, 1))
  private val grid                              = get(Grid.over(frame, 2, 1))
  private def mass(x: Double): Mass[Px]         =
    val values = IArray(x, 1.0 - x)
    get(Surface.mass(grid, values, Provenance.raw(ContentHash.of(values))))
  private def row(key: String, group: String, matched: String, x: Double, y: Double) =
    get(MapTemplateObservation.of(key, group, matched, mass(x), y))
  private val train =
    Vector(row("a", "p1", "a", 1, 2), row("b", "p2", "b", 1, 2), row("c", "p3", "c", 0, 1))
  private def split(x: Double = 0.5, y: Double = 7.0) = get(
    MapTemplateSplit.of(
      train ++ Vector(
        row("duplicate-match", "p1", "held", 0, 900),
        row("held", "p4", "held", x, y)
      ),
      Set("p4"),
      "participant",
      "score"
    )
  )
  private def near(actual: Double, expected: Double): Unit =
    assertEqualsDouble(actual, expected, LearnedTolerance)

  test("equal-trial learned mean and slope agree with independent analytic expectations") {
    val s = split(); val model = get(LearnedTemplate.fit(s.training))
    near(model.mean.values(0), 2.0 / 3); near(model.mean.values(1), 1.0 / 3)
    near(model.slope, math.sqrt(5.0))
    model.trainingFeatures
      .map(_._2)
      .zip(Vector(2.0, 2.0, 1.0))
      .foreach((a, b) => near(a, b / math.sqrt(5.0)))
    val result = get(model.evaluate(s.heldOut))
    near(get(result.rows.head.result)._1, 3.0 / math.sqrt(2.0))
    near(get(result.rows.head.result)._2, 7.0 - 3.0 / math.sqrt(2.0))
    assertEquals(model.training.splitUnit, "participant")
    assertEquals(model.training.responseUnit, "score")
  }

  test(
    "changing held-out map and response preserves learned state but changes predictions and errors"
  ) {
    val a  = split(); val b                               = split(0, 700)
    val ma = get(LearnedTemplate.fit(a.training)); val mb = get(LearnedTemplate.fit(b.training))
    assertEquals(a.training.hash, b.training.hash)
    assertEquals(ma.mean.values.toVector, mb.mean.values.toVector)
    assertEquals(ma.mean.provenance.digest, mb.mean.provenance.digest)
    assertEquals(ma.slope, mb.slope)
    assertEquals(ma.trainingFeatures, mb.trainingFeatures)
    assertNotEquals(a.heldOut.hash, b.heldOut.hash)
    val ra = get(get(ma.evaluate(a.heldOut)).rows.head.result)
    val rb = get(get(mb.evaluate(b.heldOut)).rows.head.result)
    near(ra._1, 3.0 / math.sqrt(2.0)); near(rb._1, 1.0); near(rb._2, 699.0)
  }

  test("matching-key exclusions retain full rows and cannot enter training") {
    val s = split()
    assertEquals(s.training.rows.map(_.key), Vector("a", "b", "c"))
    assertEquals(s.heldOut.rows.map(_.key), Vector("held"))
    assertEquals(s.excluded.map(_.row.key), Vector("duplicate-match"))
    assertEquals(s.excluded.head.row.response, 900.0)
    assertEquals(s.excluded.head.reason, TemplateExclusionReason.HeldOutMatchGroup)
    assertEquals(s.training.rows.size + s.heldOut.rows.size + s.excluded.size, s.rows.size)
    val onlyOverlap = Vector(row("a", "p1", "same", 1, 2), row("b", "p2", "same", 0, 1))
    assert(MapTemplateSplit.of(onlyOverlap, Set("p2"), "participant", "score").isLeft)
  }

  test("invalid split, duplicate identity, metadata and foreign grids are refused") {
    val rows = split().rows
    assert(MapTemplateSplit.of(rows, Set("missing"), "participant", "score").isLeft)
    assert(MapTemplateSplit.of(rows, Set.empty, "participant", "score").isLeft)
    assert(
      MapTemplateSplit.of(rows, rows.map(_.splitGroup).toSet, "participant", "score").isLeft
    )
    assert(MapTemplateSplit.of(rows :+ rows.head, Set("p4"), "participant", "score").isLeft)
    assert(MapTemplateSplit.of(rows, Set("p4"), "", "score").isLeft)
    assert(MapTemplateObservation.of("a", "p", "m", mass(1), Double.NaN).isLeft)
    assert(MapTemplateObservation.of("a", "", "m", mass(1), 1).isLeft)
    val otherGrid = get(Grid.over(get(Frame.screen("other", 2, 1)), 2, 1))
    val otherMap  =
      get(Surface.mass(otherGrid, IArray(0.5, 0.5), Provenance.raw(ContentHash.empty)))
    val foreign = get(MapTemplateObservation.of("foreign", "p4", "other", otherMap, 7))
    assert(MapTemplateSplit.of(train :+ foreign, Set("p4"), "participant", "score").isLeft)
  }

  test("evaluation refuses a different training payload and retains row order") {
    val a = split()
    val b = get(
      MapTemplateSplit.of(
        a.rows.updated(0, row("a", "p1", "a", 0.5, 2)),
        Set("p4"),
        "participant",
        "score"
      )
    )
    assert(get(LearnedTemplate.fit(a.training)).evaluate(b.heldOut).isLeft)
    val two = get(
      MapTemplateSplit.of(
        a.rows :+ row("held-two", "p4", "second", 1, 2),
        Set("p4"),
        "participant",
        "score"
      )
    )
    val evaluation = get(get(LearnedTemplate.fit(two.training)).evaluate(two.heldOut))
    assertEquals(evaluation.rows.map(_.key), Vector("held", "held-two"))
  }

  test(
    "external callers cannot pass held-out maps into learned construction or forge capabilities"
  ) {
    assert(typeCheckErrors("""
      def leak(h: eyes4s.design.MapTemplateHeldOut[String, eyes4s.kernel.Unit2D.Px]) = eyes4s.design.LearnedTemplate.fit(h)
    """).nonEmpty)
    assert(typeCheckErrors("""
      new eyes4s.design.MapTemplateTraining[String, eyes4s.kernel.Unit2D.Px](Vector.empty, "participant", "score", eyes4s.kernel.ContentHash.empty)
    """).nonEmpty)
    assert(typeCheckErrors("""
      new eyes4s.design.LearnedTemplate[String, eyes4s.kernel.Unit2D.Px](null, null, 1.0, Vector.empty)
    """).nonEmpty)
  }
