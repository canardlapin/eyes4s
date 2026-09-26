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

/** The pinned second versions of the study plan and admission ledger. The
  * values are built here; src/test/resources/eyes4s/study-v2.json and
  * admission-ledger-v2.json are their pretty-printed encodings, and the
  * compact strings below mirror them for portable JVM/JS tests.
  */
object StudyV2Fixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new AssertionError(s"$x"), identity)

  val screen: Frame[Px]    = get(Frame.screen("v2-screen", 16, 12))
  val window: Subframe[Px] =
    get(Subframe.of(screen, FrameId("v2-image"), get(Bounds.of[Px](4, 3, 12, 9))))
  val grid: Grid[Px]              = get(Grid.of(GridId("v2-image@4x3"), window.frame, 4, 3))
  val geometry: StudyGeometry[Px] =
    get(StudyGeometry.windowed(window, grid, OffWindowPolicy.Exclude))
  val angular: LinearAngularScale[Px] = get(LinearAngularScale.of(screen, 2.0))

  /** A windowed study with one scale in degrees and one in pixels. */
  def plan: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference] =
    get(
      StudyPlan.configure(
        get(ArtifactRef.parse[StudyInput[StudyKey, Px]]("0123456789abcdef")),
        StudyKey.layout(DefinitionId.studyLayout),
        geometry,
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyScale.Angular(StudyEstimate.Gaussian(get(Sigma.deg(0.5)), EdgePolicy.Truncate)),
          StudyScale.Native(StudyEstimate.Binned())
        ),
        Some(angular),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )

  private val a1 = StudyKey("p1", "a", "encode")
  private val a2 = StudyKey("p2", "a", "encode")

  /** An admission under the default policy with two correction rules: every
    * trial of participant p2 is flipped vertically, and one trial of p1 is
    * translated. Record 3 lies outside the screen and is admitted as such.
    */
  def ledger: AdmissionLedger[StudyKey] =
    get(
      AdmissionLedger.decide(
        SourceRef.of("v2.csv", header, rows),
        header,
        Vector(
          SourceRecord(2, Disposition.Admitted(a1, 0)),
          SourceRecord(3, Disposition.Admitted(a1, 1)),
          SourceRecord(4, Disposition.Admitted(a2, 0))
        ),
        AdmissionDecision.RequireComplete,
        AdmissionPolicy(
          OffScreenPolicy.ExcludeRecord,
          Vector(
            AppliedCorrection(CorrectionScope.Participant("p2"), Correction.FlipY),
            AppliedCorrection(CorrectionScope.Trial(a1), get(Correction.translate(1.0, -0.5)))
          )
        ),
        Vector(OutsideFrame(3, 17.0, 2.5, screen.id))
      )
    )

  val header: Vector[String] =
    Vector("participant", "image", "phase", "fixation", "x", "y", "onset", "duration", "n")
  val rows: Vector[Vector[String]] = Vector(
    Vector("p1", "a", "encode", "0", "3", "4", "0", "100", "10"),
    Vector("p1", "a", "encode", "1", "16", "3", "110", "100", "10"),
    Vector("p2", "a", "encode", "0", "5", "5", "0", "100", "10")
  )
