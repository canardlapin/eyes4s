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

class EyeLinkSupportSuite extends munit.FunSuite:

  test("the draft support contract has unique stable ids and evidence for every row") {
    val rows = EyeLinkSupport.capabilities

    assert(rows.nonEmpty)
    assertEquals(rows.map(_.id).distinct.length, rows.length)
    assert(rows.forall(_.id.nonEmpty))
    assert(rows.forall(_.dimension.nonEmpty))
    assert(rows.forall(_.value.nonEmpty))
    assert(rows.forall(_.rationale.nonEmpty))
    assert(rows.forall(_.evidenceRefs.nonEmpty))
    assert(rows.forall(_.evidenceRefs.forall(_.nonEmpty)))
  }

  test("no planned capability is advertised as validated support") {
    assert(EyeLinkSupport.required.nonEmpty)
    assert(EyeLinkSupport.required.forall(_.evidence == EyeLinkEvidence.Planned))
    assert(EyeLinkSupport.required.forall(!_.isAdvertisable))
    assert(!EyeLinkSupport.releaseReady)
  }

  test("binary EDF is explicitly rejected at the portable boundary") {
    val edf = EyeLinkSupport.find("source-edf")

    assertEquals(edf.map(_.disposition), Some(EyeLinkDisposition.RejectExplicitly))
    assertEquals(edf.map(_.evidence), Some(EyeLinkEvidence.NotRequired))
  }

  test("GAZE is a release obligation while HREF and RAW remain preserved native data") {
    assertEquals(
      EyeLinkSupport.find("coordinates-gaze").map(_.disposition),
      Some(EyeLinkDisposition.RequiredForRelease)
    )
    assertEquals(
      EyeLinkSupport.find("coordinates-href").map(_.disposition),
      Some(EyeLinkDisposition.PreserveWithoutInterpretation)
    )
    assertEquals(
      EyeLinkSupport.find("coordinates-raw").map(_.disposition),
      Some(EyeLinkDisposition.PreserveWithoutInterpretation)
    )
  }

  test("the contract requires every eye layout, major rate, and current hardware family") {
    val requiredIds = EyeLinkSupport.required.map(_.id).toSet

    assert(Set("eye-left", "eye-right", "eye-binocular").subsetOf(requiredIds))
    assert(Set("rate-250", "rate-500", "rate-1000", "rate-2000").subsetOf(requiredIds))
    assert(
      Set("hardware-1000-family", "hardware-portable-duo", "hardware-eyelink-3")
        .subsetOf(requiredIds)
    )
  }

  test("third parties cannot construct unsupported support claims") {
    val errors = typeCheckErrors("""
      import eyes4s.io.*
      new EyeLinkCapability(
        "claim",
        "format",
        "anything",
        EyeLinkDisposition.RequiredForRelease,
        EyeLinkEvidence.Validated,
        Vector.empty,
        ""
      )
    """)

    assert(errors.nonEmpty)
  }

end EyeLinkSupportSuite
