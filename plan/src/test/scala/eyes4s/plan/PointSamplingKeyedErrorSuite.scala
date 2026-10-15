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

import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

class PointSamplingKeyedErrorSuite extends munit.FunSuite:
  test("point failure contexts retain nominal key types across the public API") {
    val valid = typeCheckErrors(
      "final case class TrialId(value: String); val error: eyes4s.plan.PointSamplingError[TrialId] = eyes4s.plan.PointSamplingError.AtTrial(TrialId(\"one\"), None, eyes4s.plan.PointSamplingError.MissingTemplate(\"p\", \"s\"))"
    )
    assertEquals(valid, Nil)
    val invalid = typeCheckErrors(
      "final case class TrialId(value: String); final case class OtherId(value: String); val error: eyes4s.plan.PointSamplingError[TrialId] = eyes4s.plan.PointSamplingError.AtTrial(TrialId(\"one\"), None, eyes4s.plan.PointSamplingError.MissingTemplate(\"p\", \"s\")); val wrong: eyes4s.plan.PointSamplingError[OtherId] = error"
    )
    assert(invalid.nonEmpty)
    assert(invalid.exists(_.message.contains("OtherId")))
  }

  test("a global refusal remains key-free and can be attached to a trial explicitly") {
    val global: PointSamplingError[Nothing] =
      PointSamplingError.MissingTemplate("participant", "stimulus")
    val context: PointSamplingError[String] =
      PointSamplingError.AtTrial("source", Some("template"), global)
    assertEquals(context.reason, global)
    val diagnostic: Diagnostic[String] = Diagnostic.of(context)
    assertEquals(diagnostic.code.render, "point-sampling.at-trial")
    assertEquals(diagnostic.affectedTrials, Vector("source", "template"))
    assertEquals(diagnostic.operands.map(_._1), Vector("source", "template", "underlying"))
    assertEquals(Diagnostic.of(global).affectedTrials, Vector.empty)
  }
