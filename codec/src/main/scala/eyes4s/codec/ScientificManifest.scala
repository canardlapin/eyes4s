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

import cats.syntax.all.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** A manifest-local artifact name: non-empty, without surrounding
  * whitespace or control characters, and representable in strict UTF-8.
  * It names an entry; where the bytes live is the application's decision.
  */
final case class ArtifactName private (value: String) derives CanEqual:
  override def toString: String = value

object ArtifactName:
  def of(value: String): Either[ManifestError, ArtifactName] =
    Either.cond(
      value.nonEmpty && value == value.trim && !value.exists(_.isControl) &&
        Utf8.encode(value).isRight,
      new ArtifactName(value),
      ManifestError.InvalidName(value)
    )

/** How an entry's bytes are encoded. */
enum MediaKind(val wire: String) derives CanEqual:
  /** Strict UTF-8 JSON carrying a versioned schema envelope. */
  case JsonText extends MediaKind("application/json")

  /** A little-endian packed payload described by a [[PayloadLayout]]. */
  case Binary extends MediaKind("application/octet-stream")

/** What an entry is in the scientific object graph, which fixes its media
  * kind and whether it carries a semantic identity: the 16-hex portable
  * `ContentHash` that plans and results reference as an `ArtifactRef`.
  */
enum ArtifactRole(val wire: String, val media: MediaKind, val identityBearing: Boolean)
    derives CanEqual:
  case StudyPlan       extends ArtifactRole("study-plan", MediaKind.JsonText, false)
  case StudyInput      extends ArtifactRole("study-input", MediaKind.JsonText, true)
  case AdmissionLedger extends ArtifactRole("admission-ledger", MediaKind.JsonText, false)
  case StudyResult     extends ArtifactRole("study-result", MediaKind.JsonText, false)

  /** A standalone monocular, binocular or packed recording. */
  case Recording      extends ArtifactRole("recording", MediaKind.JsonText, true)
  case RecordingInput extends ArtifactRole("recording-input", MediaKind.JsonText, true)
  case TemporalInput  extends ArtifactRole("temporal-study-input", MediaKind.JsonText, true)
  case Payload        extends ArtifactRole("payload", MediaKind.Binary, false)

  /** A recording plan, a completed recording analysis, a temporal study plan
    * and a completed temporal study; none carries a semantic identity of its
    * own, since plans and results reference their inputs.
    */
  case RecordingPlan   extends ArtifactRole("recording-plan", MediaKind.JsonText, false)
  case RecordingResult extends ArtifactRole("recording-result", MediaKind.JsonText, false)
  case TemporalPlan    extends ArtifactRole("temporal-plan", MediaKind.JsonText, false)
  case TemporalResult  extends ArtifactRole("temporal-result", MediaKind.JsonText, false)

  /** A report specification and an evaluated report (`eyes4s.report-spec@1`,
    * `eyes4s.report@1`); a report cites what it was evaluated over by
    * canonical digest.
    */
  case ReportSpec extends ArtifactRole("report-spec", MediaKind.JsonText, false)
  case Report     extends ArtifactRole("report", MediaKind.JsonText, false)

/** One stored artifact: its role, the schema of its content, its media kind,
  * the exact byte length and SHA-256 of its bytes, the semantic identity for
  * identity-bearing roles and the layout of a payload.
  *
  * The byte digest and the semantic identity are separate on purpose: a
  * re-formatted JSON document has new bytes and therefore a new digest, but
  * the same scientific identity.
  */
final case class ManifestEntry private (
    name: ArtifactName,
    role: ArtifactRole,
    schema: DefinitionId,
    media: MediaKind,
    length: Long,
    sha256: ByteDigest,
    identity: Option[ContentHash],
    layout: Option[PayloadLayout]
) derives CanEqual

object ManifestEntry:
  /** Refuses a media kind other than the role's, a negative length, an
    * identity on a role without one (or none on a role with one), and a
    * payload without the packed-array schema and a layout of its exact length.
    */
  def of(
      name: ArtifactName,
      role: ArtifactRole,
      schema: DefinitionId,
      media: MediaKind,
      length: Long,
      sha256: ByteDigest,
      identity: Option[ContentHash],
      layout: Option[PayloadLayout]
  ): Either[ManifestError, ManifestEntry] =
    val payload = role == ArtifactRole.Payload
    for
      _ <- Either.cond(media == role.media, (), ManifestError.MediaMismatch(name, role, media))
      _ <- Either.cond(length >= 0L, (), ManifestError.NegativeLength(name, length))
      _ <- Either.cond(
        identity.isDefined == role.identityBearing,
        (),
        ManifestError.IdentityPresence(name, role, identity.map(_.render))
      )
      _ <- Either.cond(
        payload == layout.isDefined && (!payload || schema == DefinitionId.packedArray),
        (),
        ManifestError.PayloadDeclaration(name, role, schema, layout)
      )
      _ <- layout.traverse_(l =>
        Either.cond(
          l.byteLength.toLong == length,
          (),
          ManifestError.LayoutLength(name, length, l.byteLength)
        )
      )
    yield new ManifestEntry(name, role, schema, media, length, sha256, identity, layout)

/** A typed edge of the scientific object graph. The resolver checks every
  * relation against the decoded values, never against names or labels.
  */
enum ManifestRelation derives CanEqual:
  /** The plan's declared input is the input entry. Exactly one per plan. */
  case PlanInput(plan: ArtifactName, input: ArtifactName)

  /** The result was produced by the plan on the input. Exactly one per result. */
  case ResultOf(result: ArtifactName, plan: ArtifactName, input: ArtifactName)

  /** The ledger is consistent with this input: its admitted records address
    * every input trial once per fixation, and it is not a refused import. A
    * ledger carries its source's digest, not the input's, so this is a check
    * of keys and fixation counts rather than of identity. At most one per
    * ledger.
    */
  case LedgerOf(ledger: ArtifactName, input: ArtifactName)

  /** The temporal input's base study is the input entry. At most one per
    * temporal input; required when the base is embedded by reference.
    */
  case TemporalBase(temporal: ArtifactName, base: ArtifactName)

  /** The recording input's channels are the recording entry. At most one per
    * recording input.
    */
  case RecordingOf(input: ArtifactName, recording: ArtifactName)

  /** A packed recording references the payload. At least one per payload. */
  case PayloadOf(owner: ArtifactName, payload: ArtifactName)

  /** The recording plan runs on the recording input: its declared
    * provenance is the input's evidence and its prerequisites hold on the
    * input's channels. Exactly one per recording plan.
    */
  case RecordingPlanInput(plan: ArtifactName, input: ArtifactName)

  /** The recording analysis was produced by the plan on the input's
    * channels. Exactly one per recording result.
    */
  case RecordingResultOf(result: ArtifactName, plan: ArtifactName, input: ArtifactName)

  /** The temporal plan's prerequisites hold on the temporal input. Exactly
    * one per temporal plan.
    */
  case TemporalPlanInput(plan: ArtifactName, input: ArtifactName)

  /** The temporal result was produced by the plan on the temporal input.
    * Exactly one per temporal result.
    */
  case TemporalResultOf(result: ArtifactName, plan: ArtifactName, input: ArtifactName)

  /** The report is the specification evaluated over the result, which was
    * computed on the input, with covariates from the ledger when it reads
    * any: the report's binding names the canonical digests of the result,
    * the input, the result's plan and the ledger, and its specification is
    * the spec entry's. Exactly one per report.
    */
  case ReportOf(
      report: ArtifactName,
      spec: ArtifactName,
      result: ArtifactName,
      input: ArtifactName,
      covariates: Option[ArtifactName]
  )

  def kind: String = this match
    case PlanInput(_, _)            => "plan-input"
    case ResultOf(_, _, _)          => "result-of"
    case LedgerOf(_, _)             => "ledger-of"
    case TemporalBase(_, _)         => "temporal-base"
    case RecordingOf(_, _)          => "recording-of"
    case PayloadOf(_, _)            => "payload-of"
    case RecordingPlanInput(_, _)   => "recording-plan-input"
    case RecordingResultOf(_, _, _) => "recording-result-of"
    case TemporalPlanInput(_, _)    => "temporal-plan-input"
    case TemporalResultOf(_, _, _)  => "temporal-result-of"
    case ReportOf(_, _, _, _, _)    => "report-of"

  /** Every endpoint as its wire field, entry name and required role; the
    * relation's source entry first.
    */
  def endpoints: Vector[(String, ArtifactName, ArtifactRole)] = this match
    case PlanInput(plan, input) =>
      Vector(("plan", plan, ArtifactRole.StudyPlan), ("input", input, ArtifactRole.StudyInput))
    case ResultOf(result, plan, input) =>
      Vector(
        ("result", result, ArtifactRole.StudyResult),
        ("plan", plan, ArtifactRole.StudyPlan),
        ("input", input, ArtifactRole.StudyInput)
      )
    case LedgerOf(ledger, input) =>
      Vector(
        ("ledger", ledger, ArtifactRole.AdmissionLedger),
        ("input", input, ArtifactRole.StudyInput)
      )
    case TemporalBase(temporal, base) =>
      Vector(
        ("temporal", temporal, ArtifactRole.TemporalInput),
        ("base", base, ArtifactRole.StudyInput)
      )
    case RecordingOf(input, recording) =>
      Vector(
        ("input", input, ArtifactRole.RecordingInput),
        ("recording", recording, ArtifactRole.Recording)
      )
    case PayloadOf(owner, payload) =>
      Vector(
        ("owner", owner, ArtifactRole.Recording),
        ("payload", payload, ArtifactRole.Payload)
      )
    case RecordingPlanInput(plan, input) =>
      Vector(
        ("plan", plan, ArtifactRole.RecordingPlan),
        ("input", input, ArtifactRole.RecordingInput)
      )
    case RecordingResultOf(result, plan, input) =>
      Vector(
        ("result", result, ArtifactRole.RecordingResult),
        ("plan", plan, ArtifactRole.RecordingPlan),
        ("input", input, ArtifactRole.RecordingInput)
      )
    case TemporalPlanInput(plan, input) =>
      Vector(
        ("plan", plan, ArtifactRole.TemporalPlan),
        ("input", input, ArtifactRole.TemporalInput)
      )
    case TemporalResultOf(result, plan, input) =>
      Vector(
        ("result", result, ArtifactRole.TemporalResult),
        ("plan", plan, ArtifactRole.TemporalPlan),
        ("input", input, ArtifactRole.TemporalInput)
      )
    case ReportOf(report, spec, result, input, covariates) =>
      Vector(
        ("report", report, ArtifactRole.Report),
        ("spec", spec, ArtifactRole.ReportSpec),
        ("result", result, ArtifactRole.StudyResult),
        ("input", input, ArtifactRole.StudyInput)
      ) ++ covariates.map(("covariates", _, ArtifactRole.AdmissionLedger))

  def source: ArtifactName = this match
    case PlanInput(plan, _)              => plan
    case ResultOf(result, _, _)          => result
    case LedgerOf(ledger, _)             => ledger
    case TemporalBase(temporal, _)       => temporal
    case RecordingOf(input, _)           => input
    case PayloadOf(owner, _)             => owner
    case RecordingPlanInput(plan, _)     => plan
    case RecordingResultOf(result, _, _) => result
    case TemporalPlanInput(plan, _)      => plan
    case TemporalResultOf(result, _, _)  => result
    case ReportOf(report, _, _, _, _)    => report

  def render: String =
    endpoints
      .map { case (field, name, _) => s"$field=${name.value}" }
      .mkString(s"$kind(", ", ", ")")

/** Why a manifest, entry or artifact name is malformed; every case names
  * the entry or relation at fault.
  */
enum ManifestError derives CanEqual:
  case InvalidName(value: String)
  case DuplicateName(name: ArtifactName)
  case MediaMismatch(name: ArtifactName, role: ArtifactRole, media: MediaKind)
  case NegativeLength(name: ArtifactName, length: Long)
  case IdentityPresence(name: ArtifactName, role: ArtifactRole, identity: Option[String])
  case PayloadDeclaration(
      name: ArtifactName,
      role: ArtifactRole,
      schema: DefinitionId,
      layout: Option[PayloadLayout]
  )
  case LayoutLength(name: ArtifactName, declared: Long, layout: Int)
  case UnknownEntry(relation: ManifestRelation, name: ArtifactName)
  case RoleMismatch(
      relation: ManifestRelation,
      name: ArtifactName,
      expected: ArtifactRole,
      found: ArtifactRole
  )
  case PayloadOwner(relation: ManifestRelation, schema: DefinitionId)
  case DuplicateRelation(relation: ManifestRelation)
  case RelationCount(name: ArtifactName, kind: String, count: Int, expected: String)

  def message: String = this match
    case InvalidName(value) =>
      s"Artifact name '$value' must be non-empty UTF-8 text without surrounding spaces or controls."
    case DuplicateName(name) => s"Artifact name '${name.value}' appears more than once."
    case MediaMismatch(name, role, media) =>
      s"Entry '${name.value}' is a ${role.wire} stored as ${media.wire}; expected ${role.media.wire}."
    case NegativeLength(name, length) => s"Entry '${name.value}' declares length $length."
    case IdentityPresence(name, role, identity) =>
      if role.identityBearing then
        s"Entry '${name.value}' is a ${role.wire} and must declare its semantic identity."
      else
        s"Entry '${name.value}' is a ${role.wire}, which has no semantic identity, " +
          s"but declares ${identity.getOrElse("")}."
    case PayloadDeclaration(name, role, schema, layout) =>
      s"Entry '${name.value}' (${role.wire}, schema ${schema.name}@${schema.version}, " +
        s"layout ${layout.fold("none")(l => l.element.wire + l.shape.mkString("[", ",", "]"))}): " +
        s"exactly the payload role carries a layout and the " +
        s"${DefinitionId.packedArray.name}@${DefinitionId.packedArray.version} schema."
    case LayoutLength(name, declared, layout) =>
      s"Entry '${name.value}' declares $declared bytes but its layout needs $layout."
    case UnknownEntry(relation, name) =>
      s"Relation ${relation.render} names '${name.value}', which is not an entry."
    case RoleMismatch(relation, name, expected, found) =>
      s"Relation ${relation.render} needs '${name.value}' to be a ${expected.wire}, " +
        s"but it is a ${found.wire}."
    case PayloadOwner(relation, schema) =>
      s"Relation ${relation.render} needs a ${DefinitionId.packedRecording.name} owner, " +
        s"not ${schema.name}@${schema.version}."
    case DuplicateRelation(relation) => s"Relation ${relation.render} is declared twice."
    case RelationCount(name, kind, count, expected) =>
      s"Entry '${name.value}' has $count $kind relations; expected $expected."

/** The bounded scientific object graph of a saved study: its entries in
  * write order and the typed relations between them (`eyes4s.manifest@1`).
  *
  * The manifest is itself digest-addressable: its address is the SHA-256 of
  * its stored bytes, and [[ScientificManifest.bytes]] is the canonical form
  * the writer stores. It contains no floating-point numbers, so that form is
  * byte-identical on the JVM and Scala.js.
  */
final case class ScientificManifest private (
    entries: Vector[ManifestEntry],
    relations: Vector[ManifestRelation]
) derives CanEqual:
  def entry(name: ArtifactName): Option[ManifestEntry] = entries.find(_.name == name)

object ScientificManifest:
  val schema: DefinitionId = DefinitionId.manifest

  /** Structural checks only; semantic relations are checked on resolution.
    * Names are unique; relations name existing entries of the required roles;
    * a payload owner is a packed recording; no relation repeats; every plan
    * and result (study, recording or temporal) has exactly one relation of
    * its kind, every ledger, temporal input and recording input at most one,
    * and every payload at least one owner.
    */
  def of(
      entries: Vector[ManifestEntry],
      relations: Vector[ManifestRelation]
  ): Either[ManifestError, ScientificManifest] =
    val byName = entries.map(e => e.name -> e).toMap

    /** Relations of `kind` whose source is `name`, or whose payload is `name`. */
    def count(name: ArtifactName, kind: String): Int =
      relations.count {
        case ManifestRelation.PayloadOf(_, payload) => kind == "payload-of" && payload == name
        case r                                      => r.kind == kind && r.source == name
      }
    def multiplicity(
        role: ArtifactRole,
        kind: String,
        valid: Int => Boolean,
        expected: String
    ): Option[ManifestError] =
      entries.filter(_.role == role).map(e => e.name -> count(e.name, kind)).collectFirst {
        case (name, n) if !valid(n) => ManifestError.RelationCount(name, kind, n, expected)
      }
    val duplicateName = entries
      .groupBy(_.name)
      .collect { case (name, group) if group.size > 1 => name }
      .toVector
      .sortBy(_.value)
      .headOption
      .map(ManifestError.DuplicateName.apply)
    val endpointErrors = relations.view.flatMap { relation =>
      relation.endpoints
        .map((_, name, role) => (name, role, byName.get(name)))
        .collectFirst {
          case (name, _, None) => ManifestError.UnknownEntry(relation, name)
          case (name, role, Some(e)) if e.role != role =>
            ManifestError.RoleMismatch(relation, name, role, e.role)
        }
        .orElse(relation match
          case ManifestRelation.PayloadOf(owner, _) =>
            byName
              .get(owner)
              .filter(_.schema != DefinitionId.packedRecording)
              .map(e => ManifestError.PayloadOwner(relation, e.schema))
          case _ => None)
    }.headOption
    val duplicateRelation = relations.zipWithIndex.collectFirst {
      case (relation, index) if relations.indexOf(relation) != index =>
        ManifestError.DuplicateRelation(relation)
    }
    val counts = multiplicity(ArtifactRole.StudyPlan, "plan-input", _ == 1, "exactly one")
      .orElse(multiplicity(ArtifactRole.StudyResult, "result-of", _ == 1, "exactly one"))
      .orElse(multiplicity(ArtifactRole.AdmissionLedger, "ledger-of", _ <= 1, "at most one"))
      .orElse(multiplicity(ArtifactRole.TemporalInput, "temporal-base", _ <= 1, "at most one"))
      .orElse(multiplicity(ArtifactRole.RecordingInput, "recording-of", _ <= 1, "at most one"))
      .orElse(multiplicity(ArtifactRole.Payload, "payload-of", _ >= 1, "at least one"))
      .orElse(
        multiplicity(ArtifactRole.RecordingPlan, "recording-plan-input", _ == 1, "exactly one")
      )
      .orElse(
        multiplicity(ArtifactRole.RecordingResult, "recording-result-of", _ == 1, "exactly one")
      )
      .orElse(
        multiplicity(ArtifactRole.TemporalPlan, "temporal-plan-input", _ == 1, "exactly one")
      )
      .orElse(
        multiplicity(ArtifactRole.TemporalResult, "temporal-result-of", _ == 1, "exactly one")
      )
      .orElse(multiplicity(ArtifactRole.Report, "report-of", _ == 1, "exactly one"))
    duplicateName
      .orElse(endpointErrors)
      .orElse(duplicateRelation)
      .orElse(counts)
      .toLeft(new ScientificManifest(entries, relations))

  val codec: VersionedCodec[ScientificManifest] =
    VersionedCodec.checked[ScientificManifest](schema)(m => Right(write(m)))(read)

  /** The canonical stored form: UTF-8 of the pretty-printed envelope. */
  def bytes(manifest: ScientificManifest): Either[CodecError, IArray[Byte]] =
    codec.encode(manifest).flatMap(json => Documents.utf8(json.spaces2))

  /** The SHA-256 of [[bytes]]: the address a store keeps the manifest under. */
  def address(manifest: ScientificManifest): Either[CodecError, ByteDigest] =
    bytes(manifest).map(ByteDigest.sha256)

  private def write(manifest: ScientificManifest): Json = Json.obj(
    "entries" -> Json.arr(manifest.entries.map { e =>
      Json.obj(
        "name"     -> Json.fromString(e.name.value),
        "role"     -> Json.fromString(e.role.wire),
        "schema"   -> Wire.id(e.schema),
        "media"    -> Json.fromString(e.media.wire),
        "length"   -> Json.fromString(e.length.toString),
        "sha256"   -> Json.fromString(e.sha256.hex),
        "identity" -> e.identity.fold(Json.Null)(h => Json.fromString(h.render)),
        "layout"   -> e.layout.fold(Json.Null)(PayloadLayout.json)
      )
    }*),
    "relations" -> Json.arr(manifest.relations.map { r =>
      // A report without covariates writes its absent ledger as null.
      val absent = r match
        case ManifestRelation.ReportOf(_, _, _, _, None) => Vector("covariates" -> Json.Null)
        case _                                           => Vector.empty
      Json.fromFields(
        (("kind" -> Json.fromString(r.kind)) +:
          r.endpoints.map { case (field, name, _) => field -> Json.fromString(name.value) }) ++
          absent
      )
    }*)
  )

  private def name(json: Json, field: String): Either[CodecError, ArtifactName] =
    Wire
      .field[String](json, field)
      .flatMap(value => ArtifactName.of(value).left.map(CodecError.Manifest.apply))

  private def readEntry(json: Json): Either[CodecError, ManifestEntry] = for
    entryName <- name(json, "name")
    roleName  <- Wire.field[String](json, "role")
    role      <- ArtifactRole.values
      .find(_.wire == roleName)
      .toRight(CodecError.Field("role", json, s"unknown artifact role '$roleName'"))
    schema    <- Wire.definition(json, "schema")
    mediaName <- Wire.field[String](json, "media")
    media     <- MediaKind.values
      .find(_.wire == mediaName)
      .toRight(CodecError.Field("media", json, s"unknown media kind '$mediaName'"))
    lengthText <- Wire.field[String](json, "length")
    length     <- lengthText.toLongOption
      .filter(n => n >= 0L && n.toString == lengthText)
      .toRight(
        CodecError.Field(
          "length",
          json,
          "expected a canonical non-negative 64-bit decimal string"
        )
      )
    hex      <- Wire.field[String](json, "sha256")
    sha256   <- ByteDigest.parse(hex).left.map(e => CodecError.Field("sha256", json, e.message))
    rendered <- Wire.field[Option[String]](json, "identity")
    identity <- rendered.traverse(text =>
      ContentHash
        .parse(text)
        .toRight(CodecError.Field("identity", json, "expected 16 lowercase hexadecimal digits"))
    )
    layoutJson <- Wire.field[Option[Json]](json, "layout")
    layout     <- layoutJson.traverse(PayloadLayout.read).left.map(Wire.at("layout"))
    entry      <- ManifestEntry
      .of(entryName, role, schema, media, length, sha256, identity, layout)
      .left
      .map(CodecError.Manifest.apply)
  yield entry

  private def readRelation(json: Json): Either[CodecError, ManifestRelation] =
    Wire.field[String](json, "kind").flatMap {
      case "plan-input" =>
        (name(json, "plan"), name(json, "input")).mapN(ManifestRelation.PlanInput.apply)
      case "result-of" =>
        (name(json, "result"), name(json, "plan"), name(json, "input"))
          .mapN(ManifestRelation.ResultOf.apply)
      case "ledger-of" =>
        (name(json, "ledger"), name(json, "input")).mapN(ManifestRelation.LedgerOf.apply)
      case "temporal-base" =>
        (name(json, "temporal"), name(json, "base")).mapN(ManifestRelation.TemporalBase.apply)
      case "recording-of" =>
        (name(json, "input"), name(json, "recording")).mapN(ManifestRelation.RecordingOf.apply)
      case "payload-of" =>
        (name(json, "owner"), name(json, "payload")).mapN(ManifestRelation.PayloadOf.apply)
      case "recording-plan-input" =>
        (name(json, "plan"), name(json, "input")).mapN(
          ManifestRelation.RecordingPlanInput.apply
        )
      case "recording-result-of" =>
        (name(json, "result"), name(json, "plan"), name(json, "input"))
          .mapN(ManifestRelation.RecordingResultOf.apply)
      case "temporal-plan-input" =>
        (name(json, "plan"), name(json, "input")).mapN(ManifestRelation.TemporalPlanInput.apply)
      case "temporal-result-of" =>
        (name(json, "result"), name(json, "plan"), name(json, "input"))
          .mapN(ManifestRelation.TemporalResultOf.apply)
      case "report-of" =>
        (
          name(json, "report"),
          name(json, "spec"),
          name(json, "result"),
          name(json, "input"),
          Wire
            .field[Option[String]](json, "covariates")
            .flatMap(_.traverse(v => ArtifactName.of(v).left.map(CodecError.Manifest.apply)))
        ).mapN(ManifestRelation.ReportOf.apply)
      case other => Left(CodecError.Field("kind", json, s"unknown relation kind '$other'"))
    }

  private def read(json: Json): Either[CodecError, ScientificManifest] = for
    rawEntries <- Wire.field[Vector[Json]](json, "entries")
    entries    <- rawEntries.zipWithIndex.traverse { case (entry, index) =>
      readEntry(entry).left.map(Wire.at(s"entries[$index]"))
    }
    rawRelations <- Wire.field[Vector[Json]](json, "relations")
    relations    <- rawRelations.zipWithIndex.traverse { case (relation, index) =>
      readRelation(relation).left.map(Wire.at(s"relations[$index]"))
    }
    manifest <- of(entries, relations).left.map(CodecError.Manifest.apply)
  yield manifest

/** One artifact ready to store: its manifest entry and a private copy of its
  * exact bytes. Build JSON artifacts from a typed value through a registered
  * codec, or from bytes already on disk (for example a pinned fixture), and
  * payloads from verified packed arrays.
  */
final class StoredArtifact private (val entry: ManifestEntry, private val data: IArray[Byte]):
  def name: ArtifactName  = entry.name
  def bytes: IArray[Byte] = data

/** A packed recording ready to store: the recording document, its payloads
  * and the `payload-of` relations joining them.
  */
final case class PackedArtifacts(
    recording: StoredArtifact,
    payloads: Vector[StoredArtifact],
    relations: Vector[ManifestRelation]
)

object StoredArtifact:
  /** Store a JSON document as the UTF-8 of its pretty-printed form. */
  def document(
      name: String,
      role: ArtifactRole,
      document: Json,
      identity: Option[ContentHash]
  ): Either[CodecError, StoredArtifact] =
    Documents.utf8(document.spaces2).flatMap(bytes(name, role, _, identity))

  /** Store existing JSON bytes verbatim. They must be strict UTF-8 JSON with
    * a schema envelope; the entry records that schema, the exact length and
    * the SHA-256 of these bytes.
    */
  def bytes(
      name: String,
      role: ArtifactRole,
      bytes: IArray[Byte],
      identity: Option[ContentHash]
  ): Either[CodecError, StoredArtifact] =
    val owned = Bytes.copy(bytes)
    for
      artifact <- ArtifactName.of(name).left.map(CodecError.Manifest.apply)
      _        <- Either.cond(
        role.media == MediaKind.JsonText,
        (),
        CodecError.Manifest(ManifestError.MediaMismatch(artifact, role, MediaKind.JsonText))
      )
      json   <- Documents.parse(owned)
      schema <- Wire.definition(json, "schema")
      entry  <- ManifestEntry
        .of(
          artifact,
          role,
          schema,
          MediaKind.JsonText,
          owned.length.toLong,
          ByteDigest.sha256(owned),
          identity,
          None
        )
        .left
        .map(CodecError.Manifest.apply)
    yield new StoredArtifact(entry, owned)

  /** Store a verified payload under its own digest and layout. */
  def payload(name: String, payload: VerifiedPayload): Either[CodecError, StoredArtifact] =
    for
      artifact <- ArtifactName.of(name).left.map(CodecError.Manifest.apply)
      entry    <- ManifestEntry
        .of(
          artifact,
          ArtifactRole.Payload,
          DefinitionId.packedArray,
          MediaKind.Binary,
          payload.bytes.length.toLong,
          payload.ref.sha256,
          None,
          Some(payload.ref.layout)
        )
        .left
        .map(CodecError.Manifest.apply)
    yield new StoredArtifact(entry, payload.bytes)

  def plan[K, U <: Unit2D, P, S, D](
      name: String,
      persistence: StudyCodec[K, U, P, S, D],
      value: eyes4s.plan.StudyPlan[K, U, P, S, D]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec.encode(value).flatMap(document(name, ArtifactRole.StudyPlan, _, None))

  def input[K, U <: Unit2D](
      name: String,
      persistence: StudyInputCodec[K, U],
      value: eyes4s.plan.StudyInput[K, U]
  ): Either[CodecError, StoredArtifact] =
    persistence.input
      .encode(value)
      .flatMap(document(name, ArtifactRole.StudyInput, _, Some(value.hash)))

  def ledger[K, U <: Unit2D](
      name: String,
      persistence: StudyInputCodec[K, U],
      value: eyes4s.plan.AdmissionLedger[K]
  ): Either[CodecError, StoredArtifact] =
    persistence.ledger
      .encode(value)
      .flatMap(document(name, ArtifactRole.AdmissionLedger, _, None))

  def result[K, U <: Unit2D, P, S, D](
      name: String,
      persistence: StudyResultCodec[K, U, P, S, D],
      value: eyes4s.plan.StudyResult[K, U, S, D]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec.encode(value).flatMap(document(name, ArtifactRole.StudyResult, _, None))

  def recording[U <: Unit2D: UnitLabel](
      name: String,
      value: eyes4s.core.Recording[U]
  ): Either[CodecError, StoredArtifact] =
    RecordingInputCodecs
      .recording[U]
      .encode(value)
      .flatMap(document(name, ArtifactRole.Recording, _, Some(value.contentHash)))

  def binocular[U <: Unit2D: UnitLabel](
      name: String,
      value: BinocularRecording[U]
  ): Either[CodecError, StoredArtifact] =
    RecordingInputCodecs
      .binocular[U]
      .encode(value)
      .flatMap(document(name, ArtifactRole.Recording, _, Some(value.contentHash)))

  def recordingInput[U <: Unit2D: UnitLabel](
      name: String,
      value: eyes4s.plan.RecordingInput[U]
  ): Either[CodecError, StoredArtifact] =
    RecordingInputCodecs
      .input[U]
      .encode(value)
      .flatMap(document(name, ArtifactRole.RecordingInput, _, Some(value.hash)))

  def temporalInput[K, U <: Unit2D](
      name: String,
      persistence: TemporalInputCodec[K, U],
      value: TemporalStudyInput[K, U]
  ): Either[CodecError, StoredArtifact] =
    persistence.input
      .encode(value)
      .flatMap(document(name, ArtifactRole.TemporalInput, _, Some(value.hash)))

  def reportSpec(
      name: String,
      value: eyes4s.results.ReportSpec
  ): Either[CodecError, StoredArtifact] =
    ReportCodecs.reportSpec
      .encode(value)
      .flatMap(document(name, ArtifactRole.ReportSpec, _, None))

  def report[K](
      name: String,
      persistence: VersionedCodec[eyes4s.results.Report[K]],
      value: eyes4s.results.Report[K]
  ): Either[CodecError, StoredArtifact] =
    persistence.encode(value).flatMap(document(name, ArtifactRole.Report, _, None))

  def recordingPlan[P](
      name: String,
      persistence: RecordingPlanCodec[P],
      value: eyes4s.plan.RecordingPlan[P]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec
      .encode(value)
      .flatMap(document(name, ArtifactRole.RecordingPlan, _, None))

  def recordingResult[P](
      name: String,
      persistence: RecordingResultCodec[P],
      value: RecordingAnalysis[P]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec
      .encode(value)
      .flatMap(document(name, ArtifactRole.RecordingResult, _, None))

  def temporalPlan[K, U <: Unit2D, P, S, D](
      name: String,
      persistence: TemporalStudyCodec[K, U, P, S, D],
      value: TemporalStudyPlan[K, U, P, S, D]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec
      .encode(value)
      .flatMap(document(name, ArtifactRole.TemporalPlan, _, None))

  def temporalResult[K, U <: Unit2D, P, S, D](
      name: String,
      persistence: TemporalResultCodec[K, U, P, S, D],
      value: TemporalStudyResult[K, U, P, S, D]
  ): Either[CodecError, StoredArtifact] =
    persistence.codec
      .encode(value)
      .flatMap(document(name, ArtifactRole.TemporalResult, _, None))

  /** A packed recording and its four payloads, named `name.tMicros`,
    * `name.support`, `name.lineage` and `name.values`.
    */
  def packedRecording[U <: Unit2D: UnitLabel](
      name: String,
      value: eyes4s.core.Recording[U]
  ): Either[CodecError, PackedArtifacts] =
    for
      packed    <- PackedRecordingCodecs.recording[U].encode(value)
      recording <- document(
        name,
        ArtifactRole.Recording,
        packed.document,
        Some(value.contentHash)
      )
      payloads <- Vector("tMicros", "support", "lineage", "values")
        .zip(packed.payloads)
        .traverse { case (column, payload) =>
          StoredArtifact.payload(s"$name.$column", payload)
        }
    yield PackedArtifacts(
      recording,
      payloads,
      payloads.map(p => ManifestRelation.PayloadOf(recording.name, p.name))
    )

/** A written manifest: the manifest value, its canonical bytes and address,
  * and the artifacts it lists. The application stores these bytes wherever
  * it likes; [[source]] serves them from memory.
  */
final class SavedManifest private (
    val manifest: ScientificManifest,
    val bytes: IArray[Byte],
    val artifacts: Vector[StoredArtifact]
):
  val address: ByteDigest = ByteDigest.sha256(bytes)

  def source: ByteSource =
    ByteSource.inMemory(Map(address -> bytes), artifacts.map(a => a.name -> a.bytes).toMap)

object SavedManifest:
  def of(
      artifacts: Vector[StoredArtifact],
      relations: Vector[ManifestRelation]
  ): Either[CodecError, SavedManifest] = for
    manifest <- ScientificManifest
      .of(artifacts.map(_.entry), relations)
      .left
      .map(CodecError.Manifest.apply)
    bytes <- ScientificManifest.bytes(manifest)
  yield new SavedManifest(manifest, bytes, artifacts)

/** Strict UTF-8 JSON documents. */
private[codec] object Documents:
  def utf8(text: String): Either[CodecError, IArray[Byte]] =
    Utf8
      .encode(text)
      .left
      .map(index => CodecError.Text(index, "a lone surrogate has no UTF-8 form"))

  def parse(bytes: IArray[Byte]): Either[CodecError, Json] =
    Utf8
      .decode(bytes)
      .left
      .map(offset => CodecError.Text(offset, "malformed UTF-8 sequence"))
      .flatMap(text =>
        io.circe.parser
          .parse(text)
          .left
          .map(e => CodecError.InvalidJson(text.take(256), e.message))
      )
