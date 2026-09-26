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
