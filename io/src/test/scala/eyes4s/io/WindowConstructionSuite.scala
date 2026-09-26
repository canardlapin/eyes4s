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

import scala.compiletime.testing.typeCheckErrors

/** Windows, angular scales, corrections, geometries and tallies are reachable
  * only through their checked constructors, from outside the modules that
  * define them.
  */
class WindowConstructionSuite extends munit.FunSuite:
  private def refused(errors: List[scala.compiletime.testing.Error]): Unit =
    assert(errors.nonEmpty, "an unchecked construction compiled")

  test("a window is built only by Subframe.of, centred or whole") {
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      val f = Frame.screen("s", 4, 4).toOption.get
      new Subframe[Unit2D.Px](f, f, f.bounds)
    """))
  }

  test("a units-per-degree scale is built only by LinearAngularScale.of") {
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      val f = Frame.screen("s", 4, 4).toOption.get
      new LinearAngularScale[Unit2D.Px](f, -1.0)
    """))
  }

  test("a translation is built only by Correction.translate") {
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      Correction.Translate(Double.NaN, 0.0)
    """))
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      new Correction.Translate(Double.NaN, 0.0)
    """))
  }

  test("a windowed geometry is built only by StudyGeometry.windowed") {
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.plan.*
      val f = Frame.screen("s", 4, 4).toOption.get
      val w = Subframe.whole(f, FrameId("w")).toOption.get
      StudyGeometry.Windowed(w, Grid.over(f, 2, 2).toOption.get, OffWindowPolicy.Exclude)
    """))
  }

  test("a window tally is counted or checked, never assembled") {
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.plan.*
      WindowTally(5, 5, 1, Span.zero, Span.zero, Span.zero)
    """))
    refused(typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.plan.*
      new WindowTally(5, 5, 1, Span.zero, Span.zero, Span.zero)
    """))
  }

  test("windows, scales, corrections and tallies render, compare and summarise") {
    import eyes4s.codec.StudyInputCodecs
    import eyes4s.kernel.*
    import eyes4s.plan.*
    def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
    val screen                        = get(Frame.screen("s", 20, 10))
    val window = get(Subframe.of(screen, FrameId("w"), get(Bounds.of[Unit2D.Px](5, 2, 15, 8))))
    assertEquals(window.origin, Pt[Unit2D.Px](5, 2))
    assertEquals(
      window,
      get(Subframe.centred(screen, FrameId("w"), get(Extent.of[Unit2D.Px](10, 6))))
    )
    assertEquals(
      window.hashCode,
      get(Subframe.of(screen, FrameId("w"), window.region)).hashCode
    )
    assert(window.render.contains("w"))
    val scale = get(LinearAngularScale.of(screen, 35))
    assertEquals(scale, get(LinearAngularScale.of(screen, 35)))
    assertEquals(scale.hashCode, get(LinearAngularScale.of(screen, 35)).hashCode)
    assertNotEquals(scale, get(LinearAngularScale.of(screen, 36)))
    assertEquals(
      Vector(Correction.FlipX, Correction.FlipY, get(Correction.translate(1.5, -2.5)))
        .map(_.render),
      Vector("flipX", "flipY", "translate(1.5, -2.5)")
    )
    val tally =
      get(WindowTally.of(1, 2, 8, Span.micros(100), Span.micros(300), Span.micros(1000)))
    assertEquals(tally.outsideScreenShare, Some(0.1))
    assert(tally.render.contains("outside window 2 of 8"))
    assertEquals(StudyInputCodecs.trial[Unit2D.Px].keys.schema, TrialKeyDefinitions.trialKey)
  }
