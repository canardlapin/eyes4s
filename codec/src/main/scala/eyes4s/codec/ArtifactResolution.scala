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

import cats.data.NonEmptyVector
import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import scala.util.control.NonFatal

/** What the resolver asks an application's storage for. */
enum ByteRequest derives CanEqual:
  /** The manifest stored under this address. */
  case Manifest(address: ByteDigest)

  /** The bytes of one manifest entry. */
  case Entry(entry: ManifestEntry)

/** Why storage could not supply bytes. */
enum SourceFailure derives CanEqual:
  case Missing
  case Unreadable(reason: String)

  /** Storage refused to read a location that lies outside its root, for
    * example through `..`, an absolute name or a symbolic link.
    */
  case OutsideRoot(location: String, root: String)

  /** Storage refused to read something other than a regular file. */
  case NotRegularFile(location: String)

  /** Storage refused to read more bytes than the request can use. */
  case Oversize(location: String, size: Long, limit: Long)

  def message: String = this match
    case Missing                         => "missing"
    case Unreadable(reason)              => reason
    case OutsideRoot(location, root)     => s"'$location' lies outside $root"
    case NotRegularFile(location)        => s"'$location' is not a regular file"
    case Oversize(location, size, limit) =>
      s"'$location' has at least $size bytes; at most $limit can be used"

/** The application's storage, injected: a total function from a request to
  * bytes. eyes4s never opens a file or a network connection to resolve a
  * manifest; where each entry lives (a project directory, a database, an
  * archive) is the application's decision.
  */
trait ByteSource:
  def read(request: ByteRequest): Either[SourceFailure, IArray[Byte]]

object ByteSource:
  def apply(f: ByteRequest => Either[SourceFailure, IArray[Byte]]): ByteSource =
    new ByteSource:
      def read(request: ByteRequest) = f(request)

  /** Manifests by address and entries by name, held in memory. */
  def inMemory(
      manifests: Map[ByteDigest, IArray[Byte]],
      entries: Map[ArtifactName, IArray[Byte]]
  ): ByteSource = apply {
    case ByteRequest.Manifest(address) => manifests.get(address).toRight(SourceFailure.Missing)
    case ByteRequest.Entry(entry)      => entries.get(entry.name).toRight(SourceFailure.Missing)
  }

  /** A content-addressed store: every blob is found by its SHA-256, the
    * manifest by its address and each entry by its declared digest.
    */
  def contentAddressed(blobs: Iterable[IArray[Byte]]): ByteSource =
    val byDigest = blobs.map(b => ByteDigest.sha256(b) -> b).toMap
    apply {
      case ByteRequest.Manifest(address) => byDigest.get(address).toRight(SourceFailure.Missing)
      case ByteRequest.Entry(entry) => byDigest.get(entry.sha256).toRight(SourceFailure.Missing)
    }

/** How a declared relation disagrees with the decoded artifacts. */
enum RelationMismatch derives CanEqual:
  /** `plan.prerequisites(input)` refused the input. */
  case Prerequisites(errors: Vector[PlanError])

  /** The result's input reference is not the input entry's identity. */
  case ResultInput(expected: String, found: String)

  /** The result's plan description differs from the plan's. */
  case Description(changes: Vector[PlanChange])

  /** `ledger.checkAgainst(input)` refused the input. */
  case Admission(error: AdmissionError)

  /** A refused import admitted no input. */
  case RefusedAdmission

  /** The temporal input's base study is another input. */
  case BaseStudy(expected: String, found: String)

  /** The recording input's channels are another recording. */
  case RecordingIdentity(expected: String, found: String)

  /** The owner does not reference the related payload. */
  case UnreferencedPayload(reference: PayloadRef)

  /** The relation's endpoints have no decoded values of their roles; the
    * relation is refused rather than skipped.
    */
  case Unavailable(endpoints: Vector[ArtifactName])

  /** `RecordingInput.disagreements(input, plan)` refused the recording input. */
  case RecordingPrerequisites(errors: Vector[RecordingInputError])

  /** `plan.prerequisites(input)` refused the temporal input. */
  case TemporalPrerequisites(errors: Vector[TemporalStudyError])

  /** The report's specification is not the spec entry's. */
  case ReportSpec(report: String, stored: String)

  /** The report is bound to another `field` (plan, input, result or
    * covariates) than the stored document, by canonical digest.
    */
  case ReportBinding(field: String, bound: String, stored: String)

  /** The report names `input`, but its result was computed on `computed`. */
  case ReportInput(input: String, computed: String)

  /** The report's covariate ledger is not a ledger of its input (no
    * `ledger-of` relation joins them).
    */
  case ReportLedger(ledger: String, input: String)

  /** The report's cells cite result rows that are not rows of the bound
    * result at the report's `scale`: a trial it did not estimate, or a
    * reference to another scale or another role's row.
    */
  case ReportMembers(scale: Int, unknown: Vector[String])

  /** Re-evaluating the report over the stored documents gives another cell:
    * the first that differs, by group, role and component.
    */
  case ReportCell(
      group: String,
      role: String,
      component: String,
      stored: String,
      recomputed: String
  )

  /** Re-evaluating the report gives another `part` (groups, contrasts,
    * accounting or findings), or refuses to evaluate it.
    */
  case ReportRecomputed(part: String, stored: String, recomputed: String)

  /** The plan's method names score components `planned`, the result's method
    * `result`: the result was not scored by the plan's method.
    */
  case ReportComponents(planned: Vector[String], result: Vector[String])

  def message: String = this match
    case Prerequisites(errors)        => errors.map(_.message).mkString(" ")
    case ResultInput(expected, found) =>
      s"The result was computed on input $found, not $expected."
    case Description(changes) =>
      s"The result's plan description differs in ${changes.map(_.field).mkString(", ")}."
    case Admission(error) => error.message
    case RefusedAdmission => "The ledger records a refused import, which admits no input."
    case BaseStudy(expected, found) =>
      s"The temporal input's base study is $found, not $expected."
    case RecordingIdentity(expected, found) =>
      s"The recording input's channels are recording $found, not $expected."
    case UnreferencedPayload(ref) =>
      s"The owner does not reference payload ${ref.sha256.hex}."
    case Unavailable(endpoints) =>
      s"No decoded value of the required role for ${endpoints.map(_.value).mkString(", ")}."
    case RecordingPrerequisites(errors) => errors.map(_.message).mkString(" ")
    case TemporalPrerequisites(errors)  => errors.map(_.message).mkString(" ")
    case ReportSpec(report, stored)     =>
      s"The report was evaluated under specification '$report', not the stored '$stored'."
    case ReportBinding(field, bound, stored) =>
      s"The report is bound to $field $bound, but the stored $field is $stored."
    case ReportInput(input, computed) =>
      s"The report names input '$input', but its result was computed on '$computed'."
    case ReportLedger(ledger, input) =>
      s"The report reads covariates from '$ledger', which no ledger-of relation joins to '$input'."
    case ReportMembers(scale, unknown) =>
      s"The report's cells cite $unknown, which are not rows of the result at scale $scale."
    case ReportCell(group, role, component, stored, recomputed) =>
      s"Re-evaluating the report gives another $role cell of $component in $group: " +
        s"stored $stored, recomputed $recomputed."
    case ReportRecomputed(part, stored, recomputed) =>
      s"Re-evaluating the report gives other $part: stored $stored, recomputed $recomputed."
    case ReportComponents(planned, result) =>
      s"The plan's method scores components $planned, but the result's method scores $result."

/** Why a manifest or one of its artifacts was refused. Every case names the
  * manifest address, the entry or the relation at fault; integrity cases are
  * reported before any artifact is decoded.
  */
enum ResolveError derives CanEqual:
  case MissingManifest(address: ByteDigest)
  case UnreadableManifest(address: ByteDigest, reason: String)
  case ManifestDigest(address: ByteDigest, actual: ByteDigest)
  case ManifestDecode(address: ByteDigest, underlying: CodecError)

  /** Storage refused to read the manifest (outside its root, not a regular
    * file, or larger than its bound); nothing was read.
    */
  case RefusedManifest(address: ByteDigest, failure: SourceFailure)
  case Missing(entry: ArtifactName)
  case Unreadable(entry: ArtifactName, reason: String)

  /** Storage refused to read the entry (outside its root, not a regular file,
    * or larger than its declared length); nothing was read.
    */
  case Refused(entry: ArtifactName, failure: SourceFailure)
  case Length(entry: ArtifactName, declared: Long, actual: Int)
  case Digest(entry: ArtifactName, declared: ByteDigest, actual: ByteDigest)
  case Text(entry: ArtifactName, offset: Int)
  case Syntax(entry: ArtifactName, reason: String)
  case Schema(entry: ArtifactName, declared: DefinitionId, found: DefinitionId)
  case Decode(entry: ArtifactName, underlying: CodecError)
  case Identity(entry: ArtifactName, declared: String, reconstructed: String)
  case Relation(relation: ManifestRelation, mismatch: RelationMismatch)

  /** The entry at fault, when the error concerns one. */
  def entryName: Option[ArtifactName] = this match
    case Missing(e)        => Some(e)
    case Unreadable(e, _)  => Some(e)
    case Refused(e, _)     => Some(e)
    case Length(e, _, _)   => Some(e)
    case Digest(e, _, _)   => Some(e)
    case Text(e, _)        => Some(e)
    case Syntax(e, _)      => Some(e)
    case Schema(e, _, _)   => Some(e)
    case Decode(e, _)      => Some(e)
    case Identity(e, _, _) => Some(e)
    case Relation(r, _)    => Some(r.source)
    case MissingManifest(_) | UnreadableManifest(_, _) | ManifestDigest(_, _) |
        ManifestDecode(_, _) | RefusedManifest(_, _) =>
      None

  def message: String = this match
    case MissingManifest(address)            => s"No manifest is stored under ${address.hex}."
    case UnreadableManifest(address, reason) =>
      s"The manifest stored under ${address.hex} could not be read: $reason"
    case ManifestDigest(address, actual) =>
      s"The manifest stored under ${address.hex} has bytes digesting to ${actual.hex}."
    case ManifestDecode(address, e) => s"The manifest stored under ${address.hex}: ${e.message}"
    case RefusedManifest(address, failure) =>
      s"Storage refused to read the manifest under ${address.hex}: ${failure.message}."
    case Missing(e)            => s"Artifact '${e.value}' is missing."
    case Unreadable(e, reason) => s"Artifact '${e.value}' could not be read: $reason"
    case Refused(e, failure)   =>
      s"Storage refused to read artifact '${e.value}': ${failure.message}."
    case Length(e, declared, actual) =>
      s"Artifact '${e.value}' declares $declared bytes but has $actual."
    case Digest(e, declared, actual) =>
      s"Artifact '${e.value}' declares SHA-256 ${declared.hex} but its bytes digest to ${actual.hex}."
    case Text(e, offset)   => s"Artifact '${e.value}' is not strict UTF-8 at byte $offset."
    case Syntax(e, reason) => s"Artifact '${e.value}' is not JSON: $reason"
    case Schema(e, declared, found) =>
      s"Artifact '${e.value}' declares schema ${declared.name}@${declared.version} but holds " +
        s"${found.name}@${found.version}."
    case Decode(e, underlying)                => s"Artifact '${e.value}': ${underlying.message}"
    case Identity(e, declared, reconstructed) =>
      s"Artifact '${e.value}' declares identity $declared but reconstructs $reconstructed."
    case Relation(relation, mismatch) => s"Relation ${relation.render}: ${mismatch.message}"

/** Evidence that the spatial unit `U` is display pixels, the only unit a
  * [[RecordingPlan]] runs in. The trait is sealed and its one instance is for
  * `Unit2D.Px`, where the conversion is the identity, so decoders that are
  * generic in `U` check a recording plan against a recording input of their
  * own unit without a cast. A manifest in any other unit cannot register
  * recording plans.
  */
sealed trait PixelUnit[U <: Unit2D]:
  def input(value: RecordingInput[U]): RecordingInput[Px]

object PixelUnit:
  given pixels: PixelUnit[Px] = new PixelUnit[Px]:
    def input(value: RecordingInput[Px]): RecordingInput[Px] = value

/** A decoded recording plan in a manifest of unit `U`: its detector
  * parameters stay abstract, and its checks against a recording input go
  * through the unit witness of its registration.
  */
trait LoadedRecordingPlan[U <: Unit2D] extends LoadedRecording:
  def description: Vector[(String, Vector[Provenance.Param])] = plan.description

  /** `RecordingInput.disagreements` of the plan on this input: empty when the
    * plan may run on it without changing any provenance it records.
    */
  def disagreements(input: RecordingInput[U]): Vector[RecordingInputError]

  /** Run the plan on the input's monocular channels once every disagreement
    * is ruled out.
    */
  def run(input: RecordingInput[U]): Either[RecordingInputError, RecordingAnalysis[Parameters]]

object LoadedRecordingPlan:
  /** A decoded plan whose input checks go through the witness that `U` is
    * display pixels.
    */
  def of[U <: Unit2D](
      loaded: LoadedRecording
  )(using pixels: PixelUnit[U]): LoadedRecordingPlan[U] =
    new LoadedRecordingPlan[U]:
      type Parameters = loaded.Parameters
      val plan: RecordingPlan[loaded.Parameters]                               = loaded.plan
      def encode: Either[CodecError, Json]                                     = loaded.encode
      def disagreements(input: RecordingInput[U]): Vector[RecordingInputError] =
        RecordingInput.disagreements(pixels.input(input), plan)
      def run(
          input: RecordingInput[U]
      ): Either[RecordingInputError, RecordingAnalysis[loaded.Parameters]] =
        val pixelInput = pixels.input(input)
        for
          _         <- RecordingInput.disagreements(pixelInput, plan).headOption.toLeft(())
          recording <- pixelInput.monocular.toRight(
            RecordingInputError.BinocularChannels(pixelInput.source)
          )
          analysis <- plan.run(recording).left.map(RecordingInputError.Plan.apply)
        yield analysis

/** The registered decoders a manifest is resolved through. Each receives a
  * parsed JSON document whose bytes, length and schema envelope were already
  * verified; none of them reads storage. Recording and temporal plans and
  * results decode only through the registries added with [[withRecordings]]
  * and [[withTemporal]]; without them such an entry is refused as an
  * unsupported schema.
  */
trait ArtifactDecoders[K, U <: Unit2D]:
  def plan(document: Json): Either[CodecError, LoadedStudy[K, U]]
  def input(document: Json): Either[CodecError, StudyInput[K, U]]
  def ledger(document: Json): Either[CodecError, AdmissionLedger[K]]
  def result(document: Json): Either[CodecError, LoadedResult[K, U]]

  /** A standalone recording; `payloads` serves the verified payloads a packed
    * recording's `payload-of` relations name.
    */
  def recording(
      document: Json,
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, RecordingChannels[U]]
  def recordingInput(document: Json): Either[CodecError, RecordingInput[U]]

  /** A temporal input; `base` serves the related base study input, if any. */
  def temporalInput(
      document: Json,
      base: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
  ): Either[CodecError, TemporalStudyInput[K, U]]

  /** A recording plan; refused, with no supported schema, unless registered
    * through [[withRecordings]]. A decorator that wraps other decoders
    * extends [[ArtifactDecoders.Delegating]], which forwards this and every
    * other decoder, so wrapping never drops a registration.
    */
  def recordingPlan(document: Json): Either[CodecError, LoadedRecordingPlan[U]] =
    ArtifactDecoders.unregistered("recording-plan", document)

  /** A recording analysis; refused unless registered through [[withRecordings]]. */
  def recordingResult(document: Json): Either[CodecError, LoadedRecordingResult] =
    ArtifactDecoders.unregistered("recording-result", document)

  /** A temporal study plan; refused unless registered through [[withTemporal]]. */
  def temporalPlan(document: Json): Either[CodecError, LoadedTemporal[K, U]] =
    ArtifactDecoders.unregistered("temporal-plan", document)

  /** A temporal study result; refused unless registered through [[withTemporal]]. */
  def temporalResult(document: Json): Either[CodecError, LoadedTemporalResult[K, U]] =
    ArtifactDecoders.unregistered("temporal-result", document)

  /** A report over this manifest's keys; refused unless registered through
    * [[withReports]]. A report specification needs no registration.
    */
  def report(document: Json): Either[CodecError, eyes4s.results.Report[K]] =
    ArtifactDecoders.unregistered("report", document)

  /** These decoders, with reports decoded through `reports` (usually
    * `ReportCodecs.report(keys)` over the study key codec).
    */
  final def withReports(
      reports: VersionedCodec[eyes4s.results.Report[K]]
  ): ArtifactDecoders[K, U] =
    new ArtifactDecoders.Delegating[K, U](this):
      override def report(document: Json) =
        ArtifactDecoders.admitted("report", document, Vector(reports.schema))(
          reports.decode(document)
        )

  /** These decoders, with recording plans and results decoded through the
    * given registries. `pixels` witnesses that this manifest's unit is the
    * display pixels a recording plan runs in; the plans' checks against
    * recording inputs go through it.
    */
  final def withRecordings(
      plans: RecordingRegistry,
      results: RecordingResultRegistry
  )(using pixels: PixelUnit[U]): ArtifactDecoders[K, U] =
    new ArtifactDecoders.Delegating[K, U](this):
      override def recordingPlan(document: Json) =
        ArtifactDecoders.admitted("recording-plan", document, plans.schemas)(
          plans.decode(document).map(LoadedRecordingPlan.of[U](_))
        )
      override def recordingResult(document: Json) =
        ArtifactDecoders.admitted("recording-result", document, results.schemas)(
          results.decode(document)
        )

  /** These decoders, with temporal plans and results decoded through the
    * given registries.
    */
  final def withTemporal(
      plans: TemporalRegistry[K, U],
      results: TemporalResultRegistry[K, U]
  ): ArtifactDecoders[K, U] =
    new ArtifactDecoders.Delegating[K, U](this):
      override def temporalPlan(document: Json) =
        ArtifactDecoders.admitted("temporal-plan", document, plans.schemas)(
          plans.decode(document)
        )
      override def temporalResult(document: Json) =
        ArtifactDecoders.admitted("temporal-result", document, results.schemas)(
          results.decode(document)
        )

object ArtifactDecoders:
  /** Every decoder of `base`, forwarded. Extend this to decorate decoders
    * (count calls, add a registration, log): a subclass overrides the
    * decoders it changes and forwards the rest, including those a later
    * release adds, so a decorator never silently turns a registered decoder
    * into a refusal.
    */
  open class Delegating[K, U <: Unit2D](base: ArtifactDecoders[K, U])
      extends ArtifactDecoders[K, U]:
    def plan(document: Json)   = base.plan(document)
    def input(document: Json)  = base.input(document)
    def ledger(document: Json) = base.ledger(document)
    def result(document: Json) = base.result(document)
    def recording(document: Json, payloads: PayloadRef => Option[VerifiedPayload]) =
      base.recording(document, payloads)
    def recordingInput(document: Json) = base.recordingInput(document)
    def temporalInput(
        document: Json,
        study: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
    )                                            = base.temporalInput(document, study)
    override def recordingPlan(document: Json)   = base.recordingPlan(document)
    override def recordingResult(document: Json) = base.recordingResult(document)
    override def temporalPlan(document: Json)    = base.temporalPlan(document)
    override def temporalResult(document: Json)  = base.temporalResult(document)
    override def report(document: Json)          = base.report(document)

  /** Nothing is registered for the role: refuse the document's schema. */
  private def unregistered[A](role: String, document: Json): Either[CodecError, A] =
    Wire
      .definition(document, "schema")
      .flatMap(found => Left(CodecError.UnsupportedSchema(role, found, Vector.empty)))

  /** Decode a document of a schema some registration declares; refuse
    * another schema naming the registered ones. An empty registry decodes,
    * so its refusal names the missing method (`MissingMethod`,
    * `MissingResultCodec`).
    */
  private def admitted[A](role: String, document: Json, supported: Vector[DefinitionId])(
      decode: => Either[CodecError, A]
  ): Either[CodecError, A] =
    Wire
      .definition(document, "schema")
      .flatMap(found =>
        if supported.isEmpty || supported.contains(found) then decode
        else Left(CodecError.UnsupportedSchema(role, found, supported))
      )

  /** Decoders over explicit registries; recordings use the built-in
    * `recording@1`, `binocular-recording@1` and `packed-recording@1` codecs.
    */
  def of[K, U <: Unit2D](
      plans: StudyRegistry[K, U],
      inputs: StudyInputRegistry[K, U],
      results: StudyResultRegistry[K, U]
  )(using unit: UnitLabel[U]): ArtifactDecoders[K, U] =
    new ArtifactDecoders[K, U]:
      def plan(document: Json)   = plans.decode(document)
      def input(document: Json)  = inputs.decodeInput(document)
      def ledger(document: Json) = inputs.decodeLedger(document)
      def result(document: Json) = results.decode(document)

      def recording(document: Json, payloads: PayloadRef => Option[VerifiedPayload]) =
        val supported = Vector(
          RecordingInputCodecs.recordingSchema,
          RecordingInputCodecs.binocularSchema,
          PackedRecordingCodecs.schema
        )
        Wire.definition(document, "schema").flatMap {
          case RecordingInputCodecs.recordingSchema =>
            RecordingInputCodecs
              .recording[U]
              .decode(document)
              .map(RecordingChannels.Monocular(_))
          case RecordingInputCodecs.binocularSchema =>
            RecordingInputCodecs
              .binocular[U]
              .decode(document)
              .map(RecordingChannels.Binocular(_))
          case PackedRecordingCodecs.schema =>
            PackedRecordingCodecs
              .recording[U]
              .decode(document, payloads)
              .map(RecordingChannels.Monocular(_))
          case other => Left(CodecError.UnsupportedSchema("recording", other, supported))
        }

      def recordingInput(document: Json) = RecordingInputCodecs.input[U].decode(document)

      def temporalInput(
          document: Json,
          base: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
      ) = for
        payload <- Wire.field[Json](document, "value")
        schema  <- Wire.definition(payload, "keySchema")
        study   <- inputs.entries
          .find(_.keys.schema == schema)
          .toRight(CodecError.MissingKeySchema(schema))
        value <- new TemporalInputCodec(
          TemporalInputCodecs.input,
          study,
          StudyEmbedding.ByReference,
          base
        ).input.decode(document)
      yield value

  /** The ordinary participant/stimulus/phase route of every registered map
    * method ([[ComparisonMethods.all]]).
    */
  def study[U <: Unit2D: UnitLabel]: Either[CodecError, ArtifactDecoders[StudyKey, U]] = for
    plans <- ComparisonMethods.all.foldLeft(
      Right(StudyRegistry.empty[StudyKey, U]): Either[CodecError, StudyRegistry[StudyKey, U]]
    )((registry, method) =>
      registry.flatMap(_.register(StudyCodecs.similarity[U](method).registration))
    )
    inputs  <- StudyInputRegistry.empty[StudyKey, U].register(StudyInputCodecs.study[U])
    results <- ComparisonMethods.all.foldLeft(
      Right(StudyResultRegistry.empty[StudyKey, U]): Either[
        CodecError,
        StudyResultRegistry[StudyKey, U]
      ]
    )((registry, method) =>
      registry.flatMap(_.register(StudyResultCodecs.registered[U](method).registration))
    )
  yield of(plans, inputs, results)

/** A verified, decoded scientific object graph, in manifest order. Every
  * value was admitted only after its bytes matched the declared length and
  * SHA-256, its envelope the declared schema, its reconstruction the declared
  * semantic identity, and every relation the decoded values.
  */
final class ResolvedManifest[K, U <: Unit2D] private[codec] (
    val manifest: ScientificManifest,
    val plans: Vector[(ArtifactName, LoadedStudy[K, U])],
    val inputs: Vector[(ArtifactName, StudyInput[K, U])],
    val ledgers: Vector[(ArtifactName, AdmissionLedger[K])],
    val results: Vector[(ArtifactName, LoadedResult[K, U])],
    val recordings: Vector[(ArtifactName, RecordingChannels[U])],
    val recordingInputs: Vector[(ArtifactName, RecordingInput[U])],
    val temporalInputs: Vector[(ArtifactName, TemporalStudyInput[K, U])],
    val payloads: Vector[(ArtifactName, VerifiedPayload)],
    val recordingPlans: Vector[(ArtifactName, LoadedRecordingPlan[U])],
    val recordingResults: Vector[(ArtifactName, LoadedRecordingResult)],
    val temporalPlans: Vector[(ArtifactName, LoadedTemporal[K, U])],
    val temporalResults: Vector[(ArtifactName, LoadedTemporalResult[K, U])],
    val reportSpecs: Vector[(ArtifactName, eyes4s.results.ReportSpec)] = Vector.empty,
    val reports: Vector[(ArtifactName, eyes4s.results.Report[K])] = Vector.empty
):
  def plan(name: ArtifactName): Option[LoadedStudy[K, U]]    = plans.collectFirst(at(name))
  def input(name: ArtifactName): Option[StudyInput[K, U]]    = inputs.collectFirst(at(name))
  def ledger(name: ArtifactName): Option[AdmissionLedger[K]] = ledgers.collectFirst(at(name))
  def result(name: ArtifactName): Option[LoadedResult[K, U]] = results.collectFirst(at(name))
  def recording(name: ArtifactName): Option[RecordingChannels[U]] =
    recordings.collectFirst(at(name))
  def recordingInput(name: ArtifactName): Option[RecordingInput[U]] =
    recordingInputs.collectFirst(at(name))
  def temporalInput(name: ArtifactName): Option[TemporalStudyInput[K, U]] =
    temporalInputs.collectFirst(at(name))
  def recordingPlan(name: ArtifactName): Option[LoadedRecordingPlan[U]] =
    recordingPlans.collectFirst(at(name))
  def recordingResult(name: ArtifactName): Option[LoadedRecordingResult] =
    recordingResults.collectFirst(at(name))
  def temporalPlan(name: ArtifactName): Option[LoadedTemporal[K, U]] =
    temporalPlans.collectFirst(at(name))
  def temporalResult(name: ArtifactName): Option[LoadedTemporalResult[K, U]] =
    temporalResults.collectFirst(at(name))
  def reportSpec(name: ArtifactName): Option[eyes4s.results.ReportSpec] =
    reportSpecs.collectFirst(at(name))
  def report(name: ArtifactName): Option[eyes4s.results.Report[K]] =
    reports.collectFirst(at(name))

  private def at[A](name: ArtifactName): PartialFunction[(ArtifactName, A), A] = {
    case (n, value) if n == name => value
  }

/** Verify and reconstruct a manifest's object graph from an injected
  * [[ByteSource]]. Pure: the only reads are the source's, each entry is read
  * once, and nothing is cached.
  *
  * Resolution runs in three phases and reports every error of a phase:
  *
  *   1. integrity: every entry is read and copied, then its length and SHA-256
  *      are checked. Any failure stops resolution here, so a changed artifact
  *      is never parsed or decoded;
  *   2. decoding: strict UTF-8, JSON, the envelope's schema against the
  *      declared schema, the registered decoder, and the reconstructed
  *      semantic identity against the declared one;
  *   3. relations: every relation is checked against the decoded values.
  *
  * Ownership: every buffer the source returns is copied before it is
  * verified, and only the copy is decoded, so a caller that mutates its array
  * after returning it cannot change what was verified or admitted.
  */
object ArtifactResolver:
  type Resolution[A] = Either[NonEmptyVector[ResolveError], A]

  /** Read the manifest stored under `address`, check its digest and decode it. */
  def manifest(
      address: ByteDigest,
      source: ByteSource
  ): Either[ResolveError, ScientificManifest] =
    for
      raw <- read(source, ByteRequest.Manifest(address)).left.map {
        case SourceFailure.Missing            => ResolveError.MissingManifest(address)
        case SourceFailure.Unreadable(reason) =>
          ResolveError.UnreadableManifest(address, reason)
        case refusal => ResolveError.RefusedManifest(address, refusal)
      }
      bytes  = Bytes.copy(raw)
      actual = ByteDigest.sha256(bytes)
      _    <- Either.cond(actual == address, (), ResolveError.ManifestDigest(address, actual))
      json <- Documents.parse(bytes).left.map(ResolveError.ManifestDecode(address, _))
      manifest <- ScientificManifest.codec
        .decode(json)
        .left
        .map(ResolveError.ManifestDecode(address, _))
    yield manifest

  /** Resolve the manifest stored under `address` and its whole graph. */
  def resolve[K, U <: Unit2D](
      address: ByteDigest,
      source: ByteSource,
      decoders: ArtifactDecoders[K, U]
  ): Resolution[ResolvedManifest[K, U]] =
    manifest(address, source)
      .leftMap(NonEmptyVector.one)
      .flatMap(resolveManifest(_, source, decoders))

  /** Resolve an already decoded manifest's graph. */
  def resolveManifest[K, U <: Unit2D](
      manifest: ScientificManifest,
      source: ByteSource,
      decoders: ArtifactDecoders[K, U]
  ): Resolution[ResolvedManifest[K, U]] =
    for
      verified <- accumulate(
        manifest.entries.map(entry => verify(entry, source).map(entry -> _))
      )
      decoded <- decode(manifest, verified, decoders)
      _       <- relations(manifest, decoded, verified)
    yield assemble(manifest, decoded)

  private def read(
      source: ByteSource,
      request: ByteRequest
  ): Either[SourceFailure, IArray[Byte]] =
    try source.read(request)
    catch
      case NonFatal(e) =>
        Left(SourceFailure.Unreadable(s"${e.getClass.getName}: ${e.getMessage}"))

  private def verify(
      entry: ManifestEntry,
      source: ByteSource
  ): Either[ResolveError, IArray[Byte]] =
    read(source, ByteRequest.Entry(entry)) match
      case Left(SourceFailure.Missing)            => Left(ResolveError.Missing(entry.name))
      case Left(SourceFailure.Unreadable(reason)) =>
        Left(ResolveError.Unreadable(entry.name, reason))
      case Left(refusal) => Left(ResolveError.Refused(entry.name, refusal))
      case Right(raw)    =>
        if raw.length.toLong != entry.length then
          Left(ResolveError.Length(entry.name, entry.length, raw.length))
        else
          val bytes  = Bytes.copy(raw)
          val actual = ByteDigest.sha256(bytes)
          Either.cond(
            actual == entry.sha256,
            bytes,
            ResolveError.Digest(entry.name, entry.sha256, actual)
          )

  private def accumulate[A](results: Vector[Either[ResolveError, A]]): Resolution[Vector[A]] =
    NonEmptyVector.fromVector(results.collect { case Left(e) => e }) match
      case Some(errors) => Left(errors)
      case None         => Right(results.collect { case Right(a) => a })

  /** Decoded values keyed by entry name. */
  private enum Decoded[K, U <: Unit2D]:
    case Plan(value: LoadedStudy[K, U])
    case Input(value: StudyInput[K, U])
    case Ledger(value: AdmissionLedger[K])
    case Result(value: LoadedResult[K, U])
    case Channels(value: RecordingChannels[U], references: Vector[PayloadRef])
    case Recorded(value: RecordingInput[U])
    case Temporal(value: TemporalStudyInput[K, U])
    case Payload(value: VerifiedPayload)
    case RecordingPlan(value: LoadedRecordingPlan[U])
    case RecordingResult(value: LoadedRecordingResult)
    case TemporalPlan(value: LoadedTemporal[K, U])
    case TemporalResult(value: LoadedTemporalResult[K, U])
    case Spec(value: eyes4s.results.ReportSpec)
    case Reported(value: eyes4s.results.Report[K])

  private def decode[K, U <: Unit2D](
      manifest: ScientificManifest,
      verified: Vector[(ManifestEntry, IArray[Byte])],
      decoders: ArtifactDecoders[K, U]
  ): Resolution[Map[ArtifactName, Decoded[K, U]]] =
    // A payload entry's layout is present by construction of ManifestEntry.
    val packed: Vector[(ManifestEntry, VerifiedPayload)] = verified.flatMap { (entry, bytes) =>
      entry.layout.map(l =>
        entry -> VerifiedPayload.trusted(PayloadRef(entry.sha256, l), bytes)
      )
    }
    val payloads  = packed.map((entry, payload) => entry.name -> payload).toMap
    val documents = verified.collect {
      case (entry, bytes) if entry.role != ArtifactRole.Payload =>
        entry -> document(entry, bytes)
    }
    // Study inputs first: a temporal input embedding its base by reference needs it.
    val inputs = documents.collect {
      case (entry, doc) if entry.role == ArtifactRole.StudyInput =>
        entry -> doc.flatMap(json =>
          attempt(entry, decoders.input(json)).flatMap(value =>
            checkIdentity(entry, value.hash).as(Decoded.Input(value))
          )
        )
    }
    val decodedInputs = inputs.collect { case (entry, Right(Decoded.Input(value))) =>
      entry.name -> value
    }.toMap
    val others = documents.collect {
      case (entry, doc) if entry.role != ArtifactRole.StudyInput =>
        entry -> doc.flatMap(json =>
          decodeOne(manifest, entry, json, decoders, payloads, decodedInputs)
        )
    }
    val all = (inputs ++ others ++ packed.map((entry, payload) =>
      entry -> Right(Decoded.Payload(payload))
    )).map((entry, value) => entry.name -> value).toMap
    accumulate(
      manifest.entries.flatMap(entry => all.get(entry.name).map(_.map(entry.name -> _)))
    )
      .map(_.toMap)

  private def document(entry: ManifestEntry, bytes: IArray[Byte]): Either[ResolveError, Json] =
    for
      text <- Utf8.decode(bytes).left.map(ResolveError.Text(entry.name, _))
      json <- io.circe.parser
        .parse(text)
        .left
        .map(e => ResolveError.Syntax(entry.name, e.message))
      found <- Wire.definition(json, "schema").left.map(ResolveError.Decode(entry.name, _))
      _     <- Either.cond(
        found == entry.schema,
        (),
        ResolveError.Schema(entry.name, entry.schema, found)
      )
    yield json

  private def attempt[A](
      entry: ManifestEntry,
      value: Either[CodecError, A]
  ): Either[ResolveError, A] =
    value.left.map(ResolveError.Decode(entry.name, _))

  private def checkIdentity(
      entry: ManifestEntry,
      reconstructed: ContentHash
  ): Either[ResolveError, Unit] =
    entry.identity match
      case Some(declared) if declared == reconstructed => Right(())
      case declared                                    =>
        Left(
          ResolveError.Identity(entry.name, declared.fold("")(_.render), reconstructed.render)
        )

  private def decodeOne[K, U <: Unit2D](
      manifest: ScientificManifest,
      entry: ManifestEntry,
      json: Json,
      decoders: ArtifactDecoders[K, U],
      payloads: Map[ArtifactName, VerifiedPayload],
      inputs: Map[ArtifactName, StudyInput[K, U]]
  ): Either[ResolveError, Decoded[K, U]] =
    entry.role match
      case ArtifactRole.StudyPlan => attempt(entry, decoders.plan(json)).map(Decoded.Plan(_))
      case ArtifactRole.AdmissionLedger =>
        attempt(entry, decoders.ledger(json)).map(Decoded.Ledger(_))
      case ArtifactRole.StudyResult =>
        attempt(entry, decoders.result(json)).map(Decoded.Result(_))
      case ArtifactRole.RecordingInput =>
        attempt(entry, decoders.recordingInput(json)).flatMap(value =>
          checkIdentity(entry, value.hash).as(Decoded.Recorded(value))
        )
      case ArtifactRole.TemporalInput =>
        val base = manifest.relations
          .collectFirst {
            case ManifestRelation.TemporalBase(temporal, base) if temporal == entry.name => base
          }
          .flatMap(inputs.get)
        attempt(
          entry,
          decoders.temporalInput(json, ref => base.filter(_.reference == ref))
        ).flatMap(value => checkIdentity(entry, value.hash).as(Decoded.Temporal(value)))
      case ArtifactRole.Recording =>
        val related = manifest.relations
          .collect {
            case ManifestRelation.PayloadOf(owner, payload) if owner == entry.name => payload
          }
          .flatMap(payloads.get)
        val references =
          if entry.schema == PackedRecordingCodecs.schema then
            attempt(entry, PackedRecordingCodecs.references(json))
          else Right(Vector.empty)
        for
          refs  <- references
          value <- attempt(
            entry,
            decoders.recording(json, ref => related.find(_.ref == ref))
          )
          _ <- checkIdentity(entry, value.contentHash)
        yield Decoded.Channels(value, refs)
      case ArtifactRole.StudyInput =>
        attempt(entry, decoders.input(json)).flatMap(value =>
          checkIdentity(entry, value.hash).as(Decoded.Input(value))
        )
      case ArtifactRole.Payload =>
        Left(ResolveError.Schema(entry.name, entry.schema, DefinitionId.packedArray))
      case ArtifactRole.RecordingPlan =>
        attempt(entry, decoders.recordingPlan(json)).map(Decoded.RecordingPlan(_))
      case ArtifactRole.RecordingResult =>
        attempt(entry, decoders.recordingResult(json)).map(Decoded.RecordingResult(_))
      case ArtifactRole.TemporalPlan =>
        attempt(entry, decoders.temporalPlan(json)).map(Decoded.TemporalPlan(_))
      case ArtifactRole.TemporalResult =>
        attempt(entry, decoders.temporalResult(json)).map(Decoded.TemporalResult(_))
      case ArtifactRole.ReportSpec =>
        attempt(entry, ReportCodecs.reportSpec.decode(json)).map(Decoded.Spec(_))
      case ArtifactRole.Report => attempt(entry, decoders.report(json)).map(Decoded.Reported(_))

  private def relations[K, U <: Unit2D](
      manifest: ScientificManifest,
      decoded: Map[ArtifactName, Decoded[K, U]],
      verified: Vector[(ManifestEntry, IArray[Byte])]
  ): Resolution[Unit] =
    // The canonical digest of a stored document, as a report's binding cites it.
    def digestOf(name: ArtifactName): Option[String] =
      verified
        .collectFirst { case (entry, bytes) if entry.name == name => document(entry, bytes) }
        .flatMap(_.toOption)
        .flatMap(json => CanonicalDigest.document[Json](json).toOption)
        .map(_.sha256.hex)
    val errors = manifest.relations.flatMap { relation =>
      def fail(mismatch: RelationMismatch) = Some(ResolveError.Relation(relation, mismatch))
      (relation, relation.endpoints.map(e => decoded.get(e._2))) match
        case (
              ManifestRelation.PlanInput(_, _),
              Vector(Some(Decoded.Plan(plan)), Some(Decoded.Input(input)))
            ) =>
          val refused = plan.prerequisites(Some(input))
          if refused.isEmpty then None else fail(RelationMismatch.Prerequisites(refused))
        case (
              ManifestRelation.ResultOf(_, _, _),
              Vector(
                Some(Decoded.Result(result)),
                Some(Decoded.Plan(plan)),
                Some(Decoded.Input(input))
              )
            ) =>
          if result.result.input != input.reference then
            fail(
              RelationMismatch.ResultInput(input.reference.digest, result.result.input.digest)
            )
          else if result.result.description != plan.description then
            fail(
              RelationMismatch.Description(
                PlanChange.between(plan.description, result.result.description)
              )
            )
          else None
        case (
              ManifestRelation.LedgerOf(_, _),
              Vector(Some(Decoded.Ledger(ledger)), Some(Decoded.Input(input)))
            ) =>
          if ledger.outcome == AdmissionOutcome.Refused then
            fail(RelationMismatch.RefusedAdmission)
          else
            ledger
              .checkAgainst(input)
              .left
              .toOption
              .flatMap(e => fail(RelationMismatch.Admission(e)))
        case (
              ManifestRelation.TemporalBase(_, _),
              Vector(Some(Decoded.Temporal(temporal)), Some(Decoded.Input(base)))
            ) =>
          if temporal.study.reference == base.reference then None
          else
            fail(
              RelationMismatch.BaseStudy(base.reference.digest, temporal.study.reference.digest)
            )
        case (
              ManifestRelation.RecordingOf(_, _),
              Vector(Some(Decoded.Recorded(input)), Some(Decoded.Channels(channels, _)))
            ) =>
          if input.channels.contentHash == channels.contentHash then None
          else
            fail(
              RelationMismatch.RecordingIdentity(
                channels.contentHash.render,
                input.channels.contentHash.render
              )
            )
        case (
              ManifestRelation.PayloadOf(_, _),
              Vector(Some(Decoded.Channels(_, references)), Some(Decoded.Payload(payload)))
            ) =>
          if references.contains(payload.ref) then None
          else fail(RelationMismatch.UnreferencedPayload(payload.ref))
        case (
              ManifestRelation.RecordingPlanInput(_, _),
              Vector(Some(Decoded.RecordingPlan(plan)), Some(Decoded.Recorded(input)))
            ) =>
          val refused = plan.disagreements(input)
          if refused.isEmpty then None
          else fail(RelationMismatch.RecordingPrerequisites(refused))
        case (
              ManifestRelation.RecordingResultOf(_, _, _),
              Vector(
                Some(Decoded.RecordingResult(result)),
                Some(Decoded.RecordingPlan(plan)),
                Some(Decoded.Recorded(input))
              )
            ) =>
          // The result references the input's channels only, which do not
          // cover its source, viewing geometry or marks: the plan it ran must
          // also agree with this input's evidence.
          val channels     = input.channels.contentHash.render
          lazy val refused = plan.disagreements(input)
          if result.analysis.input.digest != channels then
            fail(RelationMismatch.ResultInput(channels, result.analysis.input.digest))
          else if result.analysis.description != plan.plan.description then
            fail(
              RelationMismatch.Description(
                PlanChange.between(plan.plan.description, result.analysis.description)
              )
            )
          else if refused.nonEmpty then fail(RelationMismatch.RecordingPrerequisites(refused))
          else None
        case (
              ManifestRelation.TemporalPlanInput(_, _),
              Vector(Some(Decoded.TemporalPlan(plan)), Some(Decoded.Temporal(input)))
            ) =>
          val refused = plan.prerequisites(Some(input))
          if refused.isEmpty then None
          else fail(RelationMismatch.TemporalPrerequisites(refused))
        case (
              ManifestRelation.TemporalResultOf(_, _, _),
              Vector(
                Some(Decoded.TemporalResult(result)),
                Some(Decoded.TemporalPlan(plan)),
                Some(Decoded.Temporal(input))
              )
            ) =>
          if result.result.input != input.reference then
            fail(
              RelationMismatch.ResultInput(input.reference.digest, result.result.input.digest)
            )
          else if result.result.description != plan.description then
            fail(
              RelationMismatch.Description(
                PlanChange.between(plan.description, result.result.description)
              )
            )
          else None
        case (ManifestRelation.ReportOf(report, spec, result, input, ledger), _) =>
          (
            decoded.get(report),
            decoded.get(spec),
            decoded.get(result),
            decoded.get(input),
            ledger.map(decoded.get)
          ) match
            case (
                  Some(Decoded.Reported(value)),
                  Some(Decoded.Spec(stored)),
                  Some(Decoded.Result(loaded)),
                  Some(Decoded.Input(_)),
                  covariates
                ) if covariates.forall(_.exists(_.isInstanceOf[Decoded.Ledger[?, ?]])) =>
              val computed = manifest.relations.collectFirst {
                case ManifestRelation.ResultOf(`result`, plan, on) => (plan, on)
              }
              val plan  = computed.map(_._1)
              val bound = value.binding
              // Every cell member must be the cell role's row, at the report's
              // scale, of a trial the bound result estimated there: a report of
              // other trials, or of another scale, cites nothing here.
              val scale     = value.spec.scale
              val estimated = loaded.result.scales
                .lift(scale)
                .fold(Set.empty[K])(_.estimation.map(_._1).toSet)
              def own(role: eyes4s.results.Role, ref: ResultRef[K]): Boolean =
                (role, ref) match
                  case (
                        eyes4s.results.Role.Matched,
                        ResultRef.Reduction(`scale`, StudyDesign.Matched, k)
                      ) =>
                    estimated.contains(k)
                  case (
                        eyes4s.results.Role.Control,
                        ResultRef.Reduction(`scale`, StudyDesign.Control, k)
                      ) =>
                    estimated.contains(k)
                  case (eyes4s.results.Role.Difference, ResultRef.ContrastRow(`scale`, k)) =>
                    estimated.contains(k)
                  case _ => false
              val strangers = value.cells
                .flatMap(c => c.members.filterNot(own(c.role, _)))
                .distinct
              if value.spec != stored then
                fail(RelationMismatch.ReportSpec(value.spec.id.value, stored.id.value))
              else if computed.exists(_._2 != input) then
                fail(
                  RelationMismatch.ReportInput(
                    input.value,
                    computed.fold("")(_._2.value)
                  )
                )
              else if ledger.exists(l =>
                  !manifest.relations.contains(ManifestRelation.LedgerOf(l, input))
                )
              then
                fail(
                  RelationMismatch.ReportLedger(ledger.fold("")(_.value), input.value)
                )
              else if strangers.nonEmpty then
                fail(RelationMismatch.ReportMembers(scale, strangers.map(_.toString)))
              else
                Vector(
                  ("plan", Some(bound.plan.hex), plan.flatMap(digestOf)),
                  ("input", Some(bound.input.hex), digestOf(input)),
                  ("result", Some(bound.result.hex), digestOf(result)),
                  ("covariates", bound.covariates.map(_.hex), ledger.flatMap(digestOf))
                ).collectFirst {
                  case (field, cited, found) if cited != found =>
                    RelationMismatch.ReportBinding(
                      field,
                      cited.fold("none")("sha256:" + _),
                      found.fold("none")("sha256:" + _)
                    )
                }.orElse {
                  // The binding names these documents: the stored report must
                  // be exactly what the shipped reduction computes over them.
                  (
                    plan.flatMap(decoded.get),
                    decoded.get(input),
                    ledger.map(decoded.get)
                  ) match
                    case (Some(Decoded.Plan(study)), Some(Decoded.Input(on)), covariates)
                        if study.description == loaded.result.description &&
                          loaded.result.input == on.reference =>
                      val sourceLedger = covariates.flatten.collect { case Decoded.Ledger(l) =>
                        l
                      }
                      ReportSources.reevaluate(study, on, loaded, sourceLedger, value)
                    // The result-of relation refuses a result another plan
                    // computed; there is nothing to re-evaluate against.
                    case _ => None
                }.flatMap(fail)
            case _ => fail(RelationMismatch.Unavailable(relation.endpoints.map(_._2)))
        case _ =>
          // Unreachable after a successful decoding phase over a well-formed
          // manifest; refused rather than skipped should it ever be reached.
          fail(RelationMismatch.Unavailable(relation.endpoints.map(_._2)))
    }
    NonEmptyVector.fromVector(errors).toLeft(())

  private def assemble[K, U <: Unit2D](
      manifest: ScientificManifest,
      decoded: Map[ArtifactName, Decoded[K, U]]
  ): ResolvedManifest[K, U] =
    val ordered = manifest.entries.flatMap(e => decoded.get(e.name).map(e.name -> _))
    new ResolvedManifest(
      manifest,
      ordered.collect { case (n, Decoded.Plan(v)) => n -> v },
      ordered.collect { case (n, Decoded.Input(v)) => n -> v },
      ordered.collect { case (n, Decoded.Ledger(v)) => n -> v },
      ordered.collect { case (n, Decoded.Result(v)) => n -> v },
      ordered.collect { case (n, Decoded.Channels(v, _)) => n -> v },
      ordered.collect { case (n, Decoded.Recorded(v)) => n -> v },
      ordered.collect { case (n, Decoded.Temporal(v)) => n -> v },
      ordered.collect { case (n, Decoded.Payload(v)) => n -> v },
      ordered.collect { case (n, Decoded.RecordingPlan(v)) => n -> v },
      ordered.collect { case (n, Decoded.RecordingResult(v)) => n -> v },
      ordered.collect { case (n, Decoded.TemporalPlan(v)) => n -> v },
      ordered.collect { case (n, Decoded.TemporalResult(v)) => n -> v },
      ordered.collect { case (n, Decoded.Spec(v)) => n -> v },
      ordered.collect { case (n, Decoded.Reported(v)) => n -> v }
    )
