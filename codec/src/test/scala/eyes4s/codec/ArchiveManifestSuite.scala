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

import eyes4s.core.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

/** Recording and temporal plans and their result archives as manifest roles:
  * relations checked against decoded identities, registration through
  * `ArtifactDecoders.withRecordings` (with the pixel unit witness) and
  * `withTemporal`, and every refusal as a typed value.
  */
class ArchiveManifestSuite extends munit.FunSuite:
  import ManifestFixtures.name

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)

  private val recordings      = ArchiveFixtures.recordingCodec
  private val recordingResult = ArchiveFixtures.recordingResults
  private val temporals       = ArchiveFixtures.temporalCodec
  private val temporalResult  = ArchiveFixtures.temporalResults

  private val decoders: ArtifactDecoders[StudyKey, Px] =
    get(
      for
        recordingPlans   <- RecordingRegistry.empty.register(recordings.registration)
        recordingResults <- RecordingResultRegistry.empty.register(recordingResult.registration)
        temporalPlans   <- TemporalRegistry.empty[StudyKey, Px].register(temporals.registration)
        temporalResults <- TemporalResultRegistry
          .empty[StudyKey, Px]
          .register(temporalResult.registration)
      yield ManifestFixtures.decoders
        .withRecordings(recordingPlans, recordingResults)
        .withTemporal(temporalPlans, temporalResults)
    )

  private lazy val recordingInput = get(
    StoredArtifact.recordingInput("recording-input", InputPayloadFixtures.input)
  )
  private lazy val recordingPlan = get(
    StoredArtifact.recordingPlan("recording-plan", recordings, ArchiveFixtures.recordingPlan)
  )
  private lazy val recordingArchive = get(
    StoredArtifact.recordingResult(
      "recording-result",
      recordingResult,
      ArchiveFixtures.recordingAnalysis
    )
  )
  private lazy val base = get(
    StoredArtifact.input(
      "base",
      StudyInputCodecs.study[Px],
      InputPayloadFixtures.temporal.study
    )
  )
  private lazy val temporalInput = get(
    StoredArtifact.temporalInput(
      "temporal",
      TemporalInputCodecs.study[Px](StudyEmbedding.ByReference),
      InputPayloadFixtures.temporal
    )
  )
  private lazy val temporalPlan = get(
    StoredArtifact.temporalPlan(
      "temporal-plan",
      temporals,
      ConventionalPlanFixtures.temporalPlan
    )
  )
  private lazy val temporalArchive = get(
    StoredArtifact.temporalResult(
      "temporal-result",
      temporalResult,
      ArchiveFixtures.temporalResult
    )
  )

  private def relations: Vector[ManifestRelation] = Vector(
    ManifestRelation.RecordingPlanInput(recordingPlan.name, recordingInput.name),
    ManifestRelation.RecordingResultOf(
      recordingArchive.name,
      recordingPlan.name,
      recordingInput.name
    ),
    ManifestRelation.TemporalBase(temporalInput.name, base.name),
    ManifestRelation.TemporalPlanInput(temporalPlan.name, temporalInput.name),
    ManifestRelation.TemporalResultOf(
      temporalArchive.name,
      temporalPlan.name,
      temporalInput.name
    )
  )

  private lazy val saved = get(
    SavedManifest.of(
      Vector(
        recordingInput,
        recordingPlan,
        recordingArchive,
        base,
        temporalInput,
        temporalPlan,
        temporalArchive
      ),
      relations
    )
  )

  private def resolve(
      manifest: SavedManifest,
      registered: ArtifactDecoders[StudyKey, Px] = decoders
  ): Either[Vector[ResolveError], ResolvedManifest[StudyKey, Px]] =
    ArtifactResolver.resolve(manifest.address, manifest.source, registered).left.map(_.toVector)

  test("recording and temporal plans and results resolve, related to their inputs") {
    val resolved = get(resolve(saved))
    assertEquals(
      resolved.manifest.entries.map(e => e.name.value -> e.role.wire),
      Vector(
        "recording-input"  -> "recording-input",
        "recording-plan"   -> "recording-plan",
        "recording-result" -> "recording-result",
        "base"             -> "study-input",
        "temporal"         -> "temporal-study-input",
        "temporal-plan"    -> "temporal-plan",
        "temporal-result"  -> "temporal-result"
      )
    )
    val plan     = get(resolved.recordingPlan(name("recording-plan")).toRight("plan"))
    val analysis = get(resolved.recordingResult(name("recording-result")).toRight("result"))
    val input    = get(resolved.recordingInput(name("recording-input")).toRight("input"))
    assertEquals(plan.description, ArchiveFixtures.recordingPlan.description)
    assertEquals(plan.disagreements(input), Vector.empty)
    assertEquals(analysis.analysis.description, plan.description)
    assertEquals(
      get(analysis.encode),
      get(recordingResult.codec.encode(ArchiveFixtures.recordingAnalysis))
    )
    // The loaded plan runs on the verified input through its unit witness.
    val rerun = get(plan.run(input))
    assertEquals(
      get(plan.encode).hcursor.downField("value").get[String]("input"),
      Right(rerun.input.digest)
    )
    assertEquals(rerun.description, analysis.analysis.description)

    val temporalLoaded =
      get(resolved.temporalPlan(name("temporal-plan")).toRight("temporal plan"))
    val temporalValue = get(resolved.temporalInput(name("temporal")).toRight("temporal input"))
    val archived      = get(resolved.temporalResult(name("temporal-result")).toRight("result"))
    assertEquals(temporalLoaded.prerequisites(Some(temporalValue)), Vector.empty)
    assertEquals(archived.result.input, temporalValue.reference)
    assertEquals(archived.result.description, temporalLoaded.description)
    assertEquals(archived.result.cells.size, 8)
  }

  test(
    "without registrations, every recording and temporal plan and result is refused by schema"
  ) {
    val plain   = ManifestFixtures.decoders
    val refused = resolve(saved, plain).left.toOption.getOrElse(fail("resolved"))
    def schemaOf(artifact: StoredArtifact) = artifact.entry.schema
    assertEquals(
      refused,
      Vector(
        ResolveError.Decode(
          recordingPlan.name,
          CodecError.UnsupportedSchema("recording-plan", schemaOf(recordingPlan), Vector.empty)
        ),
        ResolveError.Decode(
          recordingArchive.name,
          CodecError.UnsupportedSchema(
            "recording-result",
            DefinitionId.recordingResult,
            Vector.empty
          )
        ),
        ResolveError.Decode(
          temporalPlan.name,
          CodecError.UnsupportedSchema("temporal-plan", schemaOf(temporalPlan), Vector.empty)
        ),
        ResolveError.Decode(
          temporalArchive.name,
          CodecError.UnsupportedSchema(
            "temporal-result",
            DefinitionId.temporalResult,
            Vector.empty
          )
        )
      )
    )
  }

  test("a registered plan without its result codec refuses the archive by method") {
    val partial = get(
      for
        plans    <- RecordingRegistry.empty.register(recordings.registration)
        temporal <- TemporalRegistry.empty[StudyKey, Px].register(temporals.registration)
      yield ManifestFixtures.decoders
        .withRecordings(plans, RecordingResultRegistry.empty)
        .withTemporal(temporal, TemporalResultRegistry.empty[StudyKey, Px])
    )
    assertEquals(
      resolve(saved, partial).left.toOption,
      Some(
        Vector(
          ResolveError.Decode(
            recordingArchive.name,
            CodecError.MissingResultCodec(recordings.method.id)
          ),
          ResolveError.Decode(
            temporalArchive.name,
            CodecError.MissingResultCodec(DefinitionId.cosine)
          )
        )
      )
    )
  }

  private lazy val otherSource = get(
    RecordingInput.of(
      RecordingRef("another-source"),
      InputPayloadFixtures.input.channels,
      InputPayloadFixtures.input.viewing,
      InputPayloadFixtures.input.synchronization
    )
  )
  private val sourceDisagreement = RelationMismatch.RecordingPrerequisites(
    Vector(
      RecordingInputError.PlanDisagreement(
        "source",
        "synthetic-left-headfixed-500",
        "another-source"
      )
    )
  )

  test("a recording plan that disagrees with its input is refused by both relations") {
    val stored   = get(StoredArtifact.recordingInput("recording-input", otherSource))
    val manifest = get(
      SavedManifest.of(Vector(stored, recordingPlan, recordingArchive), relations.take(2))
    )
    assertEquals(
      resolve(manifest).left.toOption,
      Some(
        Vector(
          ResolveError.Relation(relations(0), sourceDisagreement),
          ResolveError.Relation(relations(1), sourceDisagreement)
        )
      )
    )
  }

  test("a result related to another input with the same channels is refused by its evidence") {
    // The plan runs on the original input; the result is related to an input
    // with the same channels (the result's input reference) but another source.
    val other    = get(StoredArtifact.recordingInput("other-input", otherSource))
    val resultOf = ManifestRelation.RecordingResultOf(
      recordingArchive.name,
      recordingPlan.name,
      other.name
    )
    val manifest = get(
      SavedManifest.of(
        Vector(recordingInput, other, recordingPlan, recordingArchive),
        Vector(relations(0), resultOf)
      )
    )
    assertEquals(
      resolve(manifest).left.toOption,
      Some(Vector(ResolveError.Relation(resultOf, sourceDisagreement)))
    )
  }

  test("a result related to another plan or input is refused by the result-of relation") {
    // A plan with another interpolation gap: the archive describes the original.
    val changed = get(
      RecordingPlan.of(
        ArchiveFixtures.recordingPlan.input,
        ArchiveFixtures.recordingPlan.source,
        ArchiveFixtures.recordingPlan.display,
        ArchiveFixtures.recordingPlan.trackerClock,
        ArchiveFixtures.recordingPlan.analysisClock,
        ArchiveFixtures.recordingPlan.angularFrameId,
        ArchiveFixtures.recordingPlan.viewing,
        ArchiveFixtures.recordingPlan.synchronizationModel,
        ArchiveFixtures.recordingPlan.marks,
        ArchiveFixtures.recordingPlan.residualLimit,
        eyes4s.detect.InterpolationGap.none,
        ArchiveFixtures.recordingPlan.areas,
        ArchiveFixtures.recordingPlan.method,
        ArchiveFixtures.recordingPlan.parameters
      )
    )
    val stored   = get(StoredArtifact.recordingPlan("recording-plan", recordings, changed))
    val manifest = get(
      SavedManifest.of(Vector(recordingInput, stored, recordingArchive), relations.take(2))
    )
    assertEquals(
      resolve(manifest).left.toOption,
      Some(
        Vector(
          ResolveError.Relation(
            relations(1),
            RelationMismatch.Description(
              PlanChange.between(
                changed.description,
                ArchiveFixtures.recordingAnalysis.description
              )
            )
          )
        )
      )
    )
    assertEquals(
      PlanChange
        .between(changed.description, ArchiveFixtures.recordingAnalysis.description)
        .map(_.field),
      Vector("interpolationGapMicros")
    )
    // The temporal plan and result over the complete temporal input, which has another digest.
    val complete = get(
      StoredArtifact.temporalInput(
        "temporal",
        TemporalInputCodecs.study[Px](StudyEmbedding.ByReference),
        InputPayloadFixtures.temporalComplete
      )
    )
    val temporalManifest = get(
      SavedManifest.of(
        Vector(base, complete, temporalPlan, temporalArchive),
        relations.drop(2)
      )
    )
    val expected = InputPayloadFixtures.temporal.reference.digest
    val found    = InputPayloadFixtures.temporalComplete.reference.digest
    assertEquals(
      resolve(temporalManifest).left.toOption,
      Some(
        Vector(
          ResolveError.Relation(
            relations(3),
            RelationMismatch.TemporalPrerequisites(
              Vector(TemporalStudyError.Input(PlanError.ArtifactMismatch(expected, found)))
            )
          ),
          ResolveError.Relation(relations(4), RelationMismatch.ResultInput(found, expected))
        )
      )
    )
  }

  test("plans and results need exactly one relation of their kind, between the right roles") {
    val entries = Vector(recordingInput, recordingPlan, recordingArchive).map(_.entry)
    assertEquals(
      ScientificManifest.of(entries, relations.take(1)).left.toOption,
      Some(
        ManifestError.RelationCount(
          recordingArchive.name,
          "recording-result-of",
          0,
          "exactly one"
        )
      )
    )
    assertEquals(
      ScientificManifest.of(entries, relations.drop(1).take(1)).left.toOption,
      Some(
        ManifestError.RelationCount(
          recordingPlan.name,
          "recording-plan-input",
          0,
          "exactly one"
        )
      )
    )
    val swapped =
      ManifestRelation.RecordingPlanInput(recordingArchive.name, recordingInput.name)
    assertEquals(
      ScientificManifest.of(entries, Vector(swapped)).left.toOption,
      Some(
        ManifestError.RoleMismatch(
          swapped,
          recordingArchive.name,
          ArtifactRole.RecordingPlan,
          ArtifactRole.RecordingResult
        )
      )
    )
    // Every new role and relation kind round-trips through the manifest codec.
    assertEquals(
      ScientificManifest.codec.decode(get(ScientificManifest.codec.encode(saved.manifest))),
      Right(saved.manifest)
    )
  }

  test("recording plans register only where the manifest unit is display pixels") {
    val degrees: ArtifactDecoders[StudyKey, Deg] = get(ArtifactDecoders.study[Deg])
    assert(degrees.recordingPlan(io.circe.Json.obj()).isLeft)
    assert(
      typeCheckErrors(
        "degrees.withRecordings(RecordingRegistry.empty, RecordingResultRegistry.empty)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "decoders.withRecordings(RecordingRegistry.empty, RecordingResultRegistry.empty)"
      ).isEmpty
    )
  }
