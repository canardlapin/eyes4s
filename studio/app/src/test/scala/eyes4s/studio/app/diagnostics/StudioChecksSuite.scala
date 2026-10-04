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

package eyes4s.studio.app.diagnostics

import eyes4s.studio.core.backend.{DiagnosticLevel, DiagnosticOrigin, Phase, TrialKey}

/** The studio's own checks (ticket S3.5): host-origin diagnostics in the
  * `studio-check` family, presented apart from eyes4s's findings; the two
  * rules the plan named are retired in favour of eyes4s's own findings and
  * cannot be made again.
  */
class StudioChecksSuite extends munit.FunSuite:

  private val p05 = TrialKey("P05", Phase.Retrieval, "ret_02", 1)

  test("a studio check is a Host diagnostic in the studio-check family, presented apart") {
    val check = StudioChecks
      .finding("stimulus-size", DiagnosticLevel.Warning, Vector(p05), "an example")
      .fold(e => fail(e.message), identity)
    assertEquals(check.code, "studio-check.stimulus-size")
    assertEquals(check.origin, DiagnosticOrigin.Host)
    assertEquals(check.affected, Vector(p05))
    val vm = DiagnosticsPresenter.present(Vector(check))
    assertEquals(vm.eyes4s, Vector.empty)
    assertEquals(vm.studio.map(_.code), Vector("studio-check.stimulus-size"))
    assertEquals(vm.studio.head.remedy.map(_.trials), Some(Vector(p05)))
  }

  test("the plan's two rules are retired: eyes4s reports them, and they cannot be made again") {
    assertEquals(
      StudioChecks
        .finding("matched-cardinality", DiagnosticLevel.Error, Vector(p05), "x")
        .left
        .map(_.message),
      Left(
        "Studio check studio-check.matched-cardinality is retired; eyes4s reports study-finding.matched-cardinality."
      )
    )
    assert(
      StudioChecks
        .finding("empty-map-in-window", DiagnosticLevel.Warning, Vector(p05), "x")
        .isLeft
    )
    // The presenter has words for each replacement.
    StudioChecks.retired.values.foreach { code =>
      val f = DiagnosticsPresenter.finding(
        eyes4s.studio.core.backend.StudioDiagnostic(
          code,
          DiagnosticLevel.Warning,
          DiagnosticOrigin.EyesCore,
          Vector.empty,
          "m",
          Vector(p05)
        )
      )
      assertNotEquals(f.title, code)
    }
  }

  test("a name outside eyes4s's code grammar is refused, naming it") {
    val refused = StudioChecks.finding("Not Kebab", DiagnosticLevel.Warning, Vector(p05), "x")
    assert(refused.left.exists(_.message.contains("'Not Kebab'")), refused)
  }
