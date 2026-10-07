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

package eyes4s.studio.core.real

import eyes4s.studio.core.backend.*
import eyes4s.codec.StudyInputCodecs
import eyes4s.kernel.Unit2D
import io.circe.syntax.*

class RealTemporalExtentSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  test("real CSV trial end comes from explicit native declaration, not the final fixation") {
    val read = TrialDurationNativeFixture.served()
    assertEquals(read.fixations.map(f => f.onsetMs + f.durationMs), Vector(400.0))
    val extent = read.extent.declared.getOrElse(fail("no declared extent"))
    assertEquals(extent.window.from.toMicros, 0L)
    assertEquals(extent.window.until.toMicros, 5000000L)
    assertEquals(extent.records, Vector(2))
    assertEquals(extent.declaration.column.value, "elapsed")
    assertEquals(extent.source.value, "inputs/trials.csv")
    assertEquals(read.asJson.as[TrialFixations], Right(read))
  }

  test(
    "undeclared legacy inventories and blank declared cells preserve distinct missing provenance"
  ) {
    val undeclared = TrialDurationNativeFixture.served(None)
    assertEquals(
      undeclared.extent,
      TrialTemporalExtent.Undeclared(undeclared.trial, undeclared.dataset)
    )
    val blank = TrialDurationNativeFixture.served(Some(""))
    assert(blank.extent.isInstanceOf[TrialTemporalExtent.Blank])
    assertEquals(blank.extent.declared, None)
    assert(blank.extent.missingMessage.exists(_.contains("records 2")))
  }

  test("captured duration survives native ledger codec and archived dataset restoration") {
    val prepared        = TrialDurationNativeFixture.prepared(Some("5"))
    val codec           = StudyInputCodecs.trial[Unit2D.Px]
    val encodedInput    = ok(codec.input.encode(prepared.admitted.input))
    val encodedEvidence = ok(codec.ledger.encode(prepared.admitted.evidence))
    val input           = ok(codec.input.decode(encodedInput))
    val evidence        = ok(codec.ledger.decode(encodedEvidence))
    val archived        = ok(RealAdmission.archived(prepared.admitted.spec, input, evidence))
    val restored        = ok(
      RealPrepared.archived(
        prepared.revision,
        prepared.dataset,
        prepared.recipe,
        prepared.plan,
        prepared.method,
        archived
      )
    )
    val served = ok(ok(RealTrialViews.of(restored)).fixations(TrialDurationNativeFixture.trial))
    assertEquals(served.extent, TrialDurationNativeFixture.served().extent)
  }
