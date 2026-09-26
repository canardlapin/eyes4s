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
    get(TemplateObservation.of(key, group, mass(x), y, Some(matched)))
  private val design = get(TemplateDesign.meanMap[Px]("participant", "score"))
  private val train  =
    Vector(row("a", "p1", "a", 1, 2), row("b", "p2", "b", 1, 2), row("c", "p3", "c", 0, 1))
  private def split(x: Double = 0.5, y: Double = 7.0) = get(
    TemplateSplit.of(
      design,
      train ++ Vector(
        row("duplicate-match", "p1", "held", 0, 900),
        row("held", "p4", "held", x, y)
      ),
      Set("p4")
    )
  )
  private def near(actual: Double, expected: Double): Unit =
    assertEqualsDouble(actual, expected, LearnedTolerance)

  test("the training identity names the spatial unit, not only the numbers") {
    // Identical ids, bounds, axis and values in pixels and in degrees are different inputs.
    val degrees =
      Frame.of[Unit2D.Deg](frame.id, get(Bounds.of[Unit2D.Deg](0, 0, 2, 1)), frame.yAxis)
    val angularGrid = get(Grid.over(degrees, 2, 1))
    def angular(key: String, group: String, matched: String, x: Double, y: Double) =
      val values = IArray(x, 1.0 - x)
      get(
        TemplateObservation.of(
          key,
          group,
          get(Surface.mass(angularGrid, values, Provenance.raw(ContentHash.of(values)))),
          y,
          Some(matched)
        )
      )
    val inDegrees = get(
      TemplateSplit.of(
        get(TemplateDesign.meanMap[Unit2D.Deg]("participant", "score")),
        Vector(
          angular("a", "p1", "a", 1, 2),
          angular("b", "p2", "b", 1, 2),
          angular("c", "p3", "c", 0, 1),
          angular("duplicate-match", "p1", "held", 0, 900),
          angular("held", "p4", "held", 0.5, 7.0)
        ),
        Set("p4")
      )
    )
    assertNotEquals(inDegrees.training.hash, split().training.hash)
    assertNotEquals(inDegrees.heldOut.hash, split().heldOut.hash)
  }

  test("equal-trial learned mean and slope agree with independent analytic expectations") {
    val s    = split(); val model = get(Template.fit(s.training))
    val mean = get(model.template.toRight("no learned template"))
    near(mean.values(0), 2.0 / 3); near(mean.values(1), 1.0 / 3)
    assertEquals(model.coefficients.size, 1)
    near(model.coefficients.head, math.sqrt(5.0))
    assertEquals(model.method, TemplateDesign.meanMapMethod)
    model.trainingFeatures
      .map(_._2.head)
      .zip(Vector(2.0, 2.0, 1.0))
      .foreach((a, b) => near(a, b / math.sqrt(5.0)))
    val result = get(model.evaluate(s.heldOut))
    near(get(result.rows.head.result)._1, 3.0 / math.sqrt(2.0))
    near(get(result.rows.head.result)._2, 7.0 - 3.0 / math.sqrt(2.0))
    assertEquals(design.splitUnit, "participant")
    assertEquals(model.training.design.responseUnit, "score")
    assertEquals(model.training.design.featureNames, Vector("training-mean cosine"))
    val validated = get(Template.crossValidate(s))
    assertEquals(validated.fitted.coefficients, model.coefficients)
    assertEquals(validated.evaluation.rows, result.rows)
  }

  test(
    "changing held-out map and response preserves learned state but changes predictions and errors"
  ) {
    val a  = split(); val b                        = split(0, 700)
    val ma = get(Template.fit(a.training)); val mb = get(Template.fit(b.training))
    assertEquals(a.training.hash, b.training.hash)
    assertEquals(ma.template.map(_.values.toVector), mb.template.map(_.values.toVector))
    assertEquals(
      ma.template.map(_.provenance.digest),
      mb.template.map(_.provenance.digest)
    )
    assertEquals(ma.coefficients, mb.coefficients)
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
    assert(TemplateSplit.of(design, onlyOverlap, Set("p2")).isLeft)
    val ungrouped = get(TemplateObservation.of("u", "p5", mass(1), 1.0))
    assertEquals(
      TemplateSplit.of(design, train :+ ungrouped, Set("p5")).left.toOption,
      Some(TemplateError.Observation("u", "p5", None, 1.0))
    )
  }

  test("invalid split, duplicate identity, metadata and foreign grids are refused") {
    val rows = split().rows
    assert(TemplateSplit.of(design, rows, Set("missing")).isLeft)
    assert(TemplateSplit.of(design, rows, Set.empty).isLeft)
    assert(TemplateSplit.of(design, rows, rows.map(_.splitGroup).toSet).isLeft)
    assert(TemplateSplit.of(design, rows :+ rows.head, Set("p4")).isLeft)
    assertEquals(
      TemplateDesign.meanMap[Px]("", "score").left.toOption,
      Some(TemplateError.Definition("", "score"))
    )
    assert(TemplateObservation.of("a", "p", mass(1), Double.NaN, Some("m")).isLeft)
    assert(TemplateObservation.of("a", "", mass(1), 1, Some("m")).isLeft)
    val otherGrid = get(Grid.over(get(Frame.screen("other", 2, 1)), 2, 1))
    val otherMap  =
      get(Surface.mass(otherGrid, IArray(0.5, 0.5), Provenance.raw(ContentHash.empty)))
    val foreign = get(TemplateObservation.of("foreign", "p4", otherMap, 7, Some("other")))
    assert(TemplateSplit.of(design, train :+ foreign, Set("p4")).isLeft)
  }

  test("evaluation refuses a different training payload and retains row order") {
    val a = split()
    val b =
      get(TemplateSplit.of(design, a.rows.updated(0, row("a", "p1", "a", 0.5, 2)), Set("p4")))
    assertEquals(
      get(Template.fit(a.training)).evaluate(b.heldOut).left.toOption,
      Some(TemplateError.Identity(a.training.hash, b.training.hash))
    )
    val two =
      get(TemplateSplit.of(design, a.rows :+ row("held-two", "p4", "second", 1, 2), Set("p4")))
    val evaluation = get(get(Template.fit(two.training)).evaluate(two.heldOut))
    assertEquals(evaluation.rows.map(_.key), Vector("held", "held-two"))
  }

  test(
    "external callers cannot pass held-out maps into learned construction or forge capabilities"
  ) {
    assert(typeCheckErrors("""
      def leak(h: eyes4s.design.TemplateHeldOut[String, eyes4s.kernel.Mass[eyes4s.kernel.Unit2D.Px]]) = eyes4s.design.Template.fit(h)
    """).nonEmpty)
    assert(typeCheckErrors("""
      new eyes4s.design.TemplateTraining[String, eyes4s.kernel.Mass[eyes4s.kernel.Unit2D.Px]](null, Vector.empty, eyes4s.kernel.ContentHash.empty)
    """).nonEmpty)
    assert(typeCheckErrors("""
      new eyes4s.design.FittedTemplate[String, eyes4s.kernel.Mass[eyes4s.kernel.Unit2D.Px]](null, Vector(1.0), None, Vector.empty, "forged", "forged")
    """).nonEmpty)
  }
