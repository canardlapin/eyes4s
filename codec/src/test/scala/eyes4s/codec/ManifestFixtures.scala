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

import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** A complete saved graph over the pinned fixtures: the study-v1 plan, the
  * study-input-v1 input and its study-result-v1 archive, a complete admission
  * ledger for that input, the temporal fixture input with its base study by
  * reference, and the recording input fixture with its recording stored as a
  * packed recording and four payloads.
  */
object ManifestFixtures:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalStateException(s"$x"), identity)

  val studies   = StudyCodecs.cosine[Px]
  val inputs    = StudyInputCodecs.study[Px]
  val results   = StudyResultCodecs.cosine[Px]
  val temporals = TemporalInputCodecs.study[Px](StudyEmbedding.ByReference)
  val decoders: ArtifactDecoders[StudyKey, Px] = get(ArtifactDecoders.study[Px])

  lazy val plan: StudyPlan[
    StudyKey,
    Px,
    Unit,
    eyes4s.compare.Similarity,
    eyes4s.design.SignedDifference
  ] =
    get(studies.codec.parse(SavedStudyFixtures.versionOne))
  lazy val input: StudyInput[StudyKey, Px] =
    get(inputs.input.parse(StudyInputFixtures.inputVersionOne))
  lazy val result = get(results.codec.parse(StudyResultFixtures.resultVersionOne))

  /** The refused v1 import: it does not admit the pinned input. */
  lazy val refused: AdmissionLedger[StudyKey] =
    get(inputs.ledger.parse(StudyInputFixtures.ledgerVersionOne))

  /** Every matched-control record admitted: the ledger of the pinned input. */
  lazy val ledger: AdmissionLedger[StudyKey] = get(
    AdmissionLedger.of(
      SourceRef
        .of("matched-control.csv", StudyInputFixtures.header, StudyInputFixtures.records),
      StudyInputFixtures.header,
      MatchedControlFixtures.fixations.zipWithIndex.map { case (row, index) =>
        SourceRecord(
          index + 2,
          Disposition.Admitted(StudyKey(row.participant, row.image, row.phase), row.ordinal)
        )
      },
      AdmissionOutcome.Complete
    )
  )

  lazy val temporal       = InputPayloadFixtures.temporal
  lazy val recordingInput = InputPayloadFixtures.input

  def name(value: String): ArtifactName = get(ArtifactName.of(value))

  lazy val planArtifact     = get(StoredArtifact.plan("plan", studies, plan))
  lazy val inputArtifact    = get(StoredArtifact.input("input", inputs, input))
  lazy val ledgerArtifact   = get(StoredArtifact.ledger("ledger", inputs, ledger))
  lazy val resultArtifact   = get(StoredArtifact.result("result", results, result))
  lazy val baseArtifact     = get(StoredArtifact.input("base", inputs, temporal.study))
  lazy val temporalArtifact = get(StoredArtifact.temporalInput("temporal", temporals, temporal))
  lazy val recordingInputArtifact =
    get(StoredArtifact.recordingInput("recording-input", recordingInput))
  lazy val packed = get(
    StoredArtifact.packedRecording("recording", InputPayloadFixtures.monocular)
  )

  lazy val artifacts: Vector[StoredArtifact] = Vector(
    planArtifact,
    inputArtifact,
    ledgerArtifact,
    resultArtifact,
    baseArtifact,
    temporalArtifact,
    recordingInputArtifact,
    packed.recording
  ) ++ packed.payloads

  lazy val relations: Vector[ManifestRelation] = Vector(
    ManifestRelation.PlanInput(name("plan"), name("input")),
    ManifestRelation.ResultOf(name("result"), name("plan"), name("input")),
    ManifestRelation.LedgerOf(name("ledger"), name("input")),
    ManifestRelation.TemporalBase(name("temporal"), name("base")),
    ManifestRelation.RecordingOf(name("recording-input"), name("recording"))
  ) ++ packed.relations

  lazy val saved: SavedManifest = get(SavedManifest.of(artifacts, relations))

  /** Entry bytes by name, for sources a test perturbs. */
  lazy val blobs: Map[ArtifactName, IArray[Byte]] =
    saved.artifacts.map(a => a.name -> a.bytes).toMap
