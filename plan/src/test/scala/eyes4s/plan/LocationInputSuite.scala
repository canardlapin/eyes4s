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

package eyes4s.plan

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class LocationInputSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
  private val frame   = get(Frame.screen("location-input", 4, 3))
  private val trialId = get(LocationTrialId.of("trial"))
  private val source  = get(LocationSourceId.of("locations.tsv"))
  private def observation(ordinal: Long, x: Double, y: Double) =
    LocationObservation(source, ordinal, Pt[Px](x, y))
  private def trial(id: LocationTrialId, rows: LocationObservation*) =
    get(LocationTrial.of(id, rows.toVector))

  test("identifiers and trials are checked before input admission") {
    assertEquals(LocationTrialId.of(""), Left(LocationInputError.EmptyTrialId("")))
    assertEquals(LocationSourceId.of(""), Left(LocationInputError.EmptySourceId("")))
    assertEquals(
      LocationTrial.of(trialId, Vector.empty),
      Left(LocationInputError.EmptyTrial(trialId))
    )
    assertEquals(
      LocationTrial.of(trialId, Vector(observation(0, 1, 1))),
      Left(LocationInputError.NonPositiveSourceOrdinal(trialId, source, 0))
    )
  }

  test("input refuses empty, duplicate, non-finite, off-frame, and duplicate-provenance rows") {
    assertEquals(LocationInput.of(frame, Vector.empty), Left(LocationInputError.EmptyInput))

    val duplicateId = trial(trialId, observation(1, 1, 1))
    assertEquals(
      LocationInput.of(frame, Vector(duplicateId, duplicateId)),
      Left(LocationInputError.DuplicateTrial(trialId))
    )

    val nonFinite = trial(trialId, observation(1, Double.NaN, 1))
    LocationInput.of(frame, Vector(nonFinite)) match
      case Left(LocationInputError.NonFinitePosition(`trialId`, `source`, 1, x, 1)) =>
        assert(x.isNaN)
      case other => fail(s"Expected non-finite-position refusal, got $other")

    val offFrame = trial(trialId, observation(1, 4, 1))
    assertEquals(
      LocationInput.of(frame, Vector(offFrame)),
      Left(LocationInputError.PositionOutsideFrame(trialId, source, 1, 4, 1, frame.id))
    )

    val other = get(LocationTrialId.of("other"))
    assertEquals(
      LocationInput.of(
        frame,
        Vector(trial(trialId, observation(1, 1, 1)), trial(other, observation(1, 2, 1)))
      ),
      Left(LocationInputError.DuplicateProvenance(source, 1))
    )
  }
