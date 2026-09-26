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

package eyes4s.codec

import eyes4s.compare.*
import eyes4s.core.Weight
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** The pinned third version of the study plan. The value is built here;
  * src/test/resources/eyes4s/study-v3.json is its pretty-printed encoding,
  * and [[StudyV3Mirrors]] mirrors it compactly for portable JVM/JS tests.
  */
object StudyV3Fixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new AssertionError(s"$x"), identity)

  /** Fixations within 1.5 degrees of the cross at the screen centre are
    * dropped until the first one farther away, on the windowed version-2
    * fixture's geometry and angular scale.
    */
  val policy: InitialFixationPolicy[Px] =
    get(InitialFixationPolicy.dropLeadingInClosedDisc(Pt[Px](8.0, 6.0), 1.5))

  def plan: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference] =
    get(
      StudyPlan.configure(
        get(ArtifactRef.parse[StudyInput[StudyKey, Px]]("0123456789abcdef")),
        StudyKey.layout(DefinitionId.studyLayout),
        StudyV2Fixtures.geometry,
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyScale.Angular(StudyEstimate.Gaussian(get(Sigma.deg(0.5)), EdgePolicy.Truncate)),
          StudyScale.Native(StudyEstimate.Binned())
        ),
        Some(StudyV2Fixtures.angular),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        StudyPairing.default,
        policy
      )
    )
