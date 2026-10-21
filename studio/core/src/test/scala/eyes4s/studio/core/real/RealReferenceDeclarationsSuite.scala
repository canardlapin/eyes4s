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

import eyes4s.plan.{
  DeclaredReference,
  TrialDisposition as CoreDisposition,
  TrialIdentity,
  TrialOccurrence,
  UnmatchedKind
}
import eyes4s.studio.core.backend.*

/** Projections of actual native unmatched reasons; no ledger matching in Studio. */
class RealReferenceDeclarationsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val first  = get(TrialIdentity.of("P03", "Encoding", "enc_11", TrialOccurrence.first))
  private val second = get(
    TrialIdentity.of("P03", "Encoding", "enc_12", get(TrialOccurrence.of(2)))
  )

  test("not-admitted declarations preserve every identity, disposition and source order") {
    val reason = UnmatchedKind.ReferenceNotAdmitted(
      Vector(
        DeclaredReference(first, CoreDisposition.Absent),
        DeclaredReference(second, CoreDisposition.NoFixations)
      )
    )
    assertEquals(
      RealResults.declaredReferences(reason),
      Vector(
        DeclaredReferenceAdmission(
          TrialKey("P03", Phase.Encoding, "enc_11", 1),
          TrialDisposition.Absent
        ),
        DeclaredReferenceAdmission(
          TrialKey("P03", Phase.Encoding, "enc_12", 2),
          TrialDisposition.NoFixations
        )
      )
    )
  }

  test("not-pairable declarations remain admitted; by-design and undetermined invent none") {
    val unpairable =
      RealResults.declaredReferences(UnmatchedKind.ReferenceNotPairable(Vector(first)))
    assertEquals(unpairable.map(_.disposition), Vector(TrialDisposition.Admitted))
    assertEquals(
      RealResults.declaredReferences(UnmatchedKind.NoReferenceInDesign),
      Vector.empty
    )
    assertEquals(RealResults.declaredReferences(UnmatchedKind.Undetermined), Vector.empty)
  }
