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

package consumer

import eyes4s.surface.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class PublicSurfaceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  test("pyramid and smoother descriptions retain scales conventions and parameter units") {
    val frame   = get(Frame.screen("surface-consumer", 3, 2));
    val grid    = get(Grid.over(frame, 3, 2))
    val measure = get(PointMeasure.uniform(frame, IArray(Pt[Px](1, 1))))
    val sigma   = get(Sigma.px(0.5))
    val pyramid = get(Pyramid.of(measure, grid, Vector(sigma), EdgePolicy.Truncate))
    assert(pyramid.render.contains("0.5")); assert(pyramid.render.contains("surface-consumer"))
    val smoother = Smoother.gaussian(sigma, EdgePolicy.Truncate)
    assertEquals(smoother.edges, EdgePolicy.Truncate)
    val card = smoother.card
    assert(card.render.contains(card.name)); assert(card.render.contains("frame units"))
    assertEquals(SmootherParameterUnits.Alternatives(Vector("a", "b")).render, "one of {a, b}")
  }
  test("density and bandwidth errors preserve scientific operands and underlying failures") {
    val underlying = SurfaceError.NonFiniteValue(17, Double.PositiveInfinity)
    assertEquals(DensityLookupError.Surface(underlying).message, underlying.message)
    val examples = Vector(
      DensityLookupError
        .Arithmetic(DensityNormalization.Sum, "sum-X", Double.NaN)
        .message -> Vector("Sum", "sum-X", "NaN"),
      DensityPointFailure.OutsideGrid(GridId("grid-X"), 17.25, 19.25).message -> Vector(
        "grid-X",
        "17.25",
        "19.25"
      ),
      DensityPointFailure.NonFinitePoint(Double.NaN, 19.25).message -> Vector("NaN", "19.25"),
      IqrBandwidthError.TooFewPoints(FrameId("frame-X"), 1).message -> Vector(
        "frame-X",
        "count=1"
      ),
      SmootherCardError.EmptyText("summary-X").message -> Vector("summary-X"),
      SmootherCardError
        .InvalidParameters(Vector("duplicate-X", "duplicate-X"))
        .message -> Vector("duplicate-X")
    )
    examples.foreach { (message, operands) =>
      operands.foreach(o => assert(message.contains(o), message))
    }
  }
