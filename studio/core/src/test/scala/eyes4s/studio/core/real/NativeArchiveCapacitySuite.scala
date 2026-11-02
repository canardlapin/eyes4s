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

import eyes4s.studio.core.artifacts.NativeArtifactBudget
import eyes4s.studio.core.backend.{AnalysisRevision, BackendError}

class NativeArchiveCapacitySuite extends munit.FunSuite:
  private val revision = AnalysisRevision(1)
  private val budget   = NativeArtifactBudget.of(maxDensityCells = 12L).toOption.get

  test("the reservation accepts its exact limit and refuses one additional map") {
    assertEquals(NativeArchiveCapacity.refusal(revision, 3, 4, budget), None)
    assertEquals(
      NativeArchiveCapacity.refusal(revision, 4, 4, budget),
      Some(BackendError.ArchiveDensityCapacity(revision, 4, 4, 12))
    )
  }

  test("capacity arithmetic cannot wrap into an apparently small allocation") {
    val refusal = NativeArchiveCapacity.refusal(revision, Long.MaxValue, Int.MaxValue, budget)
    assert(refusal.nonEmpty)
    assert(refusal.get.message.contains((BigInt(Long.MaxValue) * Int.MaxValue).toString))
    assert(refusal.get.message.contains("conservative"))
  }
