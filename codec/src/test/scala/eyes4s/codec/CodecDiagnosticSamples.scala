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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import eyes4s.plan.DiagnosticSamples.family
import io.circe.Json

/** A sample of every case of every codec error family, projected through the
  * codec's `Diagnose` instances. Same-typed fields hold distinct values so a
  * swapped operand is detected.
  */
object CodecDiagnosticSamples:
  import CodecDiagnostics.given

  private def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  private val k1                        = DiagnosticSamples.k1
  private val k2                        = DiagnosticSamples.k2
  private val a                         = ByteDigest.sha256(IArray[Byte](1, 2, 3))
  private val b                         = ByteDigest.sha256(IArray[Byte](4, 5, 6))
  def name(value: String): ArtifactName = get(ArtifactName.of(value))

  private val plan   = get(ArtifactName.of("plan"))
  private val input  = get(ArtifactName.of("input"))
  private val result = get(ArtifactName.of("result"))
  private val layout = get(
    PayloadLayout.of(ElementKind.Float64, Vector(3, 2), ArrayOrder.RowMajor)
  )
  private val column   = get(PayloadLayout.column(ElementKind.Int32, 4))
  private val ref      = PayloadRef(a, layout)
  private val digest   = "0123456789abcdef"
  private val digest2  = "fedcba9876543210"
  private val planOf   = ManifestRelation.PlanInput(plan, input)
  private val resultOf = ManifestRelation.ResultOf(result, plan, input)
  private val change   = PlanChange("grid", Vector.empty, Vector(Provenance.Param.Text("g")))
  private val decode   = CodecError.Schema(DefinitionId.study, DefinitionId.studyInput)
  private val row      = CodecError.Reconstruction(ReconstructionError.Denominator(k1, 1, 2, 3))

  val all: Vector[FamilySamples] = Vector(
    family[CodecError]("CodecError")(
      CodecError.InvalidJson("{", "unexpected end"),
      CodecError.Field("value.grid", Json.obj("nx" -> Json.fromInt(0)), "positive"),
      decode,
      CodecError.Definition(PlanError.MissingArtifact(digest)),
      CodecError.DuplicateKeys(DefinitionId.trials, Vector(0, 2)),
      CodecError.MissingIdentity("frame", "display"),
      CodecError.IdentityConflict(
        "clock",
        "tracker",
        Json.fromString("left"),
        Json.fromString("right")
      ),
      CodecError.MissingMethod(DefinitionId.cosine),
      CodecError.DuplicateMethod(DefinitionId.cosine),
      CodecError.MissingKeySchema(DefinitionId.studyKey),
      CodecError.DuplicateKeySchema(DefinitionId.studyKey),
      CodecError.Entry("scales[0]", row),
      CodecError.Unsupported("scales[1]", "temporal failure"),
      CodecError.InputIdentity(digest, digest2),
      CodecError.Admission(AdmissionError.QuarantineAdmitted(4, 7)),
      CodecError.Recording("samples", RecordingError.NonMonotonic(3, 10L, 5L)),
      CodecError.Support(
        "support",
        DetectionSupportError.EventSpanHasNoSamples(
          RecordingRef("rec"),
          2,
          get(Interval.of(ClockId("c"), Instant.micros(1), Instant.micros(9)))
        )
      ),
      CodecError.Synchronization(
        "sync",
        SyncEvidenceError.DegenerateSourceVariance(ClockId("s"), ClockId("t"), 0.5)
      ),
      CodecError.Input(RecordingInputError.BinocularChannels(RecordingRef("rec"))),
      CodecError.Temporal(TemporalStudyError.MissingEpoch(digest)),
      CodecError.SampleBound("channels", 5000, 4096),
      CodecError.SynchronizationFit("sync", 10L, 1.5, 12L, 2.5),
      row,
      CodecError.Result(
        StudyResultError.Scale(1, StudyResultError.OrphanPair(k1, k2))
      ),
      CodecError.ScoreComponents(Vector("value"), Vector("shape")),
      CodecError.MissingResultCodec(DefinitionId.cosine),
      CodecError.DuplicateResultCodec(DefinitionId.cosine),
      CodecError.Payload("layout", PayloadError.NegativeExtent(1, -3)),
      CodecError.MissingPayload("channels.x", ref),
      CodecError.Manifest(ManifestError.DuplicateName(plan)),
      CodecError.Text(17, "invalid continuation byte"),
      CodecError.UnsupportedSchema(
        "plan",
        DefinitionId.studyInput,
        Vector(DefinitionId.study, DefinitionId.cosine)
      ),
      CodecError.Derived(
        "detection.labels[3]",
        Json.fromString("fixation"),
        Json.fromString("missing")
      ),
      CodecError.RecordingResult(
        RecordingResultError.Stage("angular", "clock", "display", "tracker")
      ),
      CodecError.NonCanonical(
        "heldOutFolds",
        Json.arr(Json.fromString("b"), Json.fromString("a")),
        Json.arr(Json.fromString("a"), Json.fromString("b")),
        "members are written in ascending order"
      ),
      CodecError.TemporalResult(
        TemporalResultError.Cell(
          "recall-encode",
          "early",
          TemporalResultError.Density(k1, Some(digest), digest2)
        )
      ),
      CodecError.Report(eyes4s.results.ReportError.DuplicateQuery(k1)),
      CodecError.ReportSpec(eyes4s.results.SpecError.NegativeScale(-2)),
      CodecError.Covariates(eyes4s.results.CovariateError.BlankUnit(" "))
    ),
    family[ResolveError]("ResolveError")(
      ResolveError.MissingManifest(a),
      ResolveError.UnreadableManifest(a, "permission denied"),
      ResolveError.ManifestDigest(a, b),
      ResolveError.ManifestDecode(a, decode),
      ResolveError.RefusedManifest(a, SourceFailure.NotRegularFile("manifest")),
      ResolveError.Missing(plan),
      ResolveError.Unreadable(plan, "permission denied"),
      ResolveError.Refused(plan, SourceFailure.Missing),
      ResolveError.Length(plan, 10L, 12),
      ResolveError.Digest(plan, a, b),
      ResolveError.Text(plan, 17),
      ResolveError.Syntax(plan, "unexpected end"),
      ResolveError.Schema(plan, DefinitionId.study, DefinitionId.studyInput),
      ResolveError.Decode(result, CodecError.Entry("scales[0]", row)),
      ResolveError.Identity(input, digest, digest2),
      ResolveError.Relation(
        resultOf,
        RelationMismatch.Admission(AdmissionError.UnadmittedTrial(3))
      )
    ),
    family[SourceFailure]("SourceFailure")(
      SourceFailure.Missing,
      SourceFailure.Unreadable("permission denied"),
      SourceFailure.OutsideRoot("../plan.json", "/project"),
      SourceFailure.NotRegularFile("plans"),
      SourceFailure.Oversize("plan.json", 2048L, 1024L)
    ),
    family[RelationMismatch]("RelationMismatch")(
      RelationMismatch.Prerequisites(
        Vector(PlanError.MissingArtifact(digest), PlanError.ArtifactMismatch(digest, digest2))
      ),
      RelationMismatch.ResultInput(digest, digest2),
      RelationMismatch.Description(Vector(change)),
      RelationMismatch.Admission(AdmissionError.UnknownTrial(Vector(3, 4))),
      RelationMismatch.RefusedAdmission,
      RelationMismatch.BaseStudy(digest, digest2),
      RelationMismatch.RecordingIdentity(digest, digest2),
      RelationMismatch.UnreferencedPayload(ref),
      RelationMismatch.Unavailable(Vector(plan, input)),
      RelationMismatch.RecordingPrerequisites(
        Vector(RecordingInputError.PlanDisagreement("source", "a", "b"))
      ),
      RelationMismatch.TemporalPrerequisites(
        Vector(TemporalStudyError.Input(PlanError.ArtifactMismatch(digest, digest2)))
      ),
      RelationMismatch.ReportSpec("memory", "window"),
      RelationMismatch.ReportBinding("result", "sha256:" + "a" * 64, "sha256:" + "b" * 64),
      RelationMismatch.ReportInput("input", "base"),
      RelationMismatch.ReportLedger("ledger", "input"),
      RelationMismatch.ReportMembers(2, Vector("p9/a")),
      RelationMismatch.ReportCell("item=a", "Difference", "value", "0.25", "1000.25"),
      RelationMismatch.ReportRecomputed("accounting", "kept=4", "kept=3"),
      RelationMismatch.ReportComponents(Vector("first", "second"), Vector("value"))
    ),
    family[ManifestError]("ManifestError")(
      ManifestError.InvalidName(" padded"),
      ManifestError.DuplicateName(plan),
      ManifestError.MediaMismatch(plan, ArtifactRole.StudyPlan, MediaKind.Binary),
      ManifestError.NegativeLength(plan, -1L),
      ManifestError.IdentityPresence(plan, ArtifactRole.StudyPlan, Some(digest)),
      ManifestError.PayloadDeclaration(
        plan,
        ArtifactRole.Payload,
        DefinitionId.study,
        Some(column)
      ),
      ManifestError.LayoutLength(plan, 10L, 16),
      ManifestError.UnknownEntry(planOf, result),
      ManifestError
        .RoleMismatch(planOf, input, ArtifactRole.StudyInput, ArtifactRole.Recording),
      ManifestError.PayloadOwner(planOf, DefinitionId.study),
      ManifestError.DuplicateRelation(resultOf),
      ManifestError.RelationCount(plan, "plan-input", 2, "exactly one")
    ),
    family[PayloadError]("PayloadError")(
      PayloadError.EmptyShape,
      PayloadError.NegativeExtent(1, -3),
      PayloadError
        .TooLarge(ElementKind.Float64, Vector(70000, 80000), PayloadLayout.maximumBytes),
      PayloadError.Count(layout, 5),
      PayloadError.Element(ElementKind.Float64, ElementKind.Int64),
      PayloadError.Length(48, 40),
      PayloadError.Digest(a, b),
      PayloadError.NotANumber(2)
    ),
    family[ByteDigestError]("ByteDigestError")(
      ByteDigestError.WrongLength("abc", 3),
      ByteDigestError.InvalidCharacter("0g", 1, 'g')
    )
  )
