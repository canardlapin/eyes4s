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

package eyes4s.surface

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class SmootherCardSuite extends munit.FunSuite:

  private val screen = Frame.screen("display", 100, 100).toOption.get
  private val grid   = Grid.square(screen, 50).toOption.get
  private val sigma  = Sigma.px(4.0).toOption.get

  private val measure = PointMeasure
    .of(screen, IArray(Pt[Px](50.0, 50.0), Pt[Px](20.0, 70.0)), IArray(1.0, 2.5))
    .toOption
    .get

  /** Every public way of obtaining a smoother, so that a new constructor
    * without a card fails here rather than in a methods section.
    */
  private val shipped: Vector[(String, Smoother[Px])] =
    EdgePolicy.values.toVector.map(policy =>
      s"gaussian/$policy" -> Smoother.gaussian(sigma, policy)
    )

  test("every shipped smoother has a unique, complete card") {
    val cards = SmootherCards.all
    assertEquals(cards.length, 1)
    assertEquals(cards.map(_.id).distinct.length, cards.length)
    cards.foreach { card =>
      assert(card.id.nonEmpty)
      assert(card.name.nonEmpty)
      assert(card.summary.nonEmpty)
      assert(card.bandwidthRule.nonEmpty)
      assert(card.parameters.nonEmpty)
      assert(card.parameters.forall(_.meaning.nonEmpty))
      assertEquals(card.parameterNames.distinct, card.parameterNames)
    }
    shipped.foreach { case (label, s) =>
      assert(cards.contains(s.card), clue(label))
    }
  }

  test("a smoother's configuration names exactly the card's parameters, in order") {
    shipped.foreach { case (label, s) =>
      assertEquals(s.configuration.map(_._1), s.card.parameterNames, clue(label))
      s.configuration.zip(s.card.parameters).foreach { case ((name, value), described) =>
        assertEquals(name, described.name, clue(label))
        (value, described.units) match
          case (Provenance.Param.Num(v), SmootherParameterUnits.FrameUnits) =>
            assert(v.isFinite, clue((label, name, v)))
          case (Provenance.Param.Text(v), SmootherParameterUnits.Alternatives(vs)) =>
            assert(vs.contains(v), clue((label, name, v, vs)))
          case other =>
            fail(s"$label: parameter $name has a value/units mismatch: $other")
      }
    }
  }

  test("the Gaussian card states its bandwidth convention and every edge policy") {
    val card = SmootherCards.gaussian
    assertEquals(card.id, "eyes4s.surface.gaussian")
    assertEquals(card.parameterNames, Vector("sigma", "edges"))
    assert(card.bandwidthRule.contains("standard deviation"))
    assert(card.bandwidthRule.contains("Bandwidth.silverman"))
    assertEquals(
      card.parameters(1).units,
      SmootherParameterUnits.Alternatives(Vector("Truncate", "Renormalise"))
    )
    // No primary source is cited in this repository's documentation for the
    // Gaussian kernel estimate, and the card says so rather than inventing one.
    assertEquals(card.citation, None)
    assertEquals(
      Smoother.gaussian(sigma, EdgePolicy.Truncate).configuration.head._2,
      Provenance.Param.Num(4.0)
    )
  }

  test("the provenance of an estimate records the configuration verbatim") {
    shipped.foreach { case (label, s) =>
      val out  = s.smooth(measure, grid).toOption.get
      val step = out.provenance.steps.last
      assertEquals(step.operation, "smooth", clue(label))
      assertEquals(step.params.head, "kernel" -> Provenance.Param.Text("gaussian"), clue(label))
      assertEquals(step.params.tail, s.configuration, clue(label))
      assertEquals(step.params.tail.map(_._1), s.card.parameterNames, clue(label))
    }
  }

  test(
    "the Gaussian provenance step digest is pinned, so a reordered configuration fails loudly"
  ) {
    // Literals computed once from the step as shipped at cb203e8: the same
    // Vector, written out below, so the pin is the old construction, not a
    // reading of the new one. Identical on JVM and Scala.js by DET-2.
    val pinned = Map(
      EdgePolicy.Truncate    -> "b2cd40693f4dcfeb",
      EdgePolicy.Renormalise -> "9ce79506f26d0fcb"
    )
    EdgePolicy.values.foreach { policy =>
      val out  = Smoother.gaussian(sigma, policy).smooth(measure, grid).toOption.get
      val step = out.provenance.steps.last
      assertEquals(step.digest.render, pinned(policy), clue(policy))
      val original = Provenance.Step(
        "smooth",
        Vector(
          "kernel" -> Provenance.Param.Text("gaussian"),
          "sigma"  -> Provenance.Param.Num(4.0),
          "edges"  -> Provenance.Param.Text(policy.toString)
        )
      )
      assertEquals(original.digest.render, pinned(policy), clue(policy))
    }
    val reordered = Provenance.Step(
      "smooth",
      Vector(
        "sigma"  -> Provenance.Param.Num(4.0),
        "kernel" -> Provenance.Param.Text("gaussian"),
        "edges"  -> Provenance.Param.Text("Truncate")
      )
    )
    assertEquals(reordered.digest.render, "668b07a67a1e4433")
    assertNotEquals(reordered.digest.render, pinned(EdgePolicy.Truncate))
  }

  test("a card cannot be declared with empty text or clashing parameter names") {
    val p = SmootherParameter("sigma", "spread", SmootherParameterUnits.FrameUnits)
    assert(SmootherCard.of("", "n", "s", Vector(p), "rule", Vector.empty, None).isLeft)
    assert(SmootherCard.of("id", " ", "s", Vector(p), "rule", Vector.empty, None).isLeft)
    assert(SmootherCard.of("id", "n", "s", Vector.empty, "rule", Vector.empty, None).isLeft)
    assert(SmootherCard.of("id", "n", "s", Vector(p, p), "rule", Vector.empty, None).isLeft)
    assert(SmootherCard.of("id", "n", "s", Vector(p), "", Vector.empty, None).isLeft)
    assert(SmootherCard.of("id", "n", "s", Vector(p), "rule", Vector.empty, Some(" ")).isLeft)
    assert(SmootherCard.of("id", "n", "s", Vector(p), "rule", Vector.empty, None).isRight)
  }
