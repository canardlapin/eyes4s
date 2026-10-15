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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class DomainInvariantSuite extends munit.FunSuite:
  test("validated geometry retains equality, hashing and read-only extractors") {
    val a = Extent.of[Px](2, 3).toOption.get
    val b = Extent.of[Px](2, 3).toOption.get
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
    val Extent(width, height) = a
    assertEquals((width, height), (2.0, 3.0))
    assertEquals(a.productArity, 2)
    assertEquals(a.productPrefix, "Extent")
    assertEquals(a.toString, s"Extent(${a.width},${a.height})")
    val bounds = Bounds.of[Px](0, 0, 2, 3).toOption.get
    assertEquals(bounds, Bounds.of[Px](0, 0, 2, 3).toOption.get)
    val frame = Frame.of(FrameId("invariants"), bounds, YAxis.Down)
    assertEquals(frame.withId(FrameId("other")).bounds, bounds)
    assertEquals(frame.spec, Frame.of(frame.id, bounds, frame.yAxis).spec)
    assertEquals(frame.spec.productArity, 5)
    assert(Extent.of[Px](-1, 3).isLeft)
    assert(Bounds.of[Px](0, 0, -1, 3).isLeft)
  }

  test("positive consumer compilation proves the rejection probes name real public types") {
    assertEquals(
      typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val extent: Either[GeometryError, Extent[Px]] = Extent.of[Px](2, 3)
      val bounds = Bounds.of[Px](0, 0, 2, 3).toOption.get
      val frame: Frame[Px] = Frame.of(FrameId("valid"), bounds, YAxis.Down)
      val spec: FrameSpec = frame.spec
    """),
      Nil
    )
  }

  test(
    "consumer cannot summon a constructing product mirror for Extent[eyes4s.kernel.Unit2D.Px]"
  ) {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      summon[scala.deriving.Mirror.ProductOf[Extent[eyes4s.kernel.Unit2D.Px]]]
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "consumer cannot summon a constructing product mirror for Bounds[eyes4s.kernel.Unit2D.Px]"
  ) {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      summon[scala.deriving.Mirror.ProductOf[Bounds[eyes4s.kernel.Unit2D.Px]]]
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "consumer cannot summon a constructing product mirror for Frame[eyes4s.kernel.Unit2D.Px]"
  ) {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      summon[scala.deriving.Mirror.ProductOf[Frame[eyes4s.kernel.Unit2D.Px]]]
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("consumer cannot summon a constructing product mirror for FrameSpec") {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      summon[scala.deriving.Mirror.ProductOf[FrameSpec]]
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("consumer cannot bypass checks through copy or direct construction") {
    assert(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      Extent.of[Px](2, 3).toOption.get.copy(width = -1)
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      new Extent[Px](-1, 3) {}
    """).nonEmpty)
  }

  test("external consumer cannot use package-trusted region constructors or factories") {
    assert(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      Region.Rect[Px](Pt[Px](0, 0), Pt[Px](-1, -1))
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      new Region.Rect[Px](Pt[Px](0, 0), Pt[Px](-1, -1)) {}
    """).nonEmpty)
  }
