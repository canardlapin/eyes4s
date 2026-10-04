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

import eyes4s.plan.*
import io.circe.Json

/** The code table of the codec's error families: decoding, artifact
  * resolution, storage failures, manifest relations, manifests, payloads and
  * byte digests. Codes are unique across this table and [[DiagnosticCatalog]].
  */
object CodecDiagnosticCatalog:
  import DiagnosticFamily.error

  val codec: DiagnosticFamily = error("codec")(
    "InvalidJson",
    "Field",
    "Schema",
    "Definition",
    "DuplicateKeys",
    "MissingIdentity",
    "IdentityConflict",
    "MissingMethod",
    "DuplicateMethod",
    "MissingKeySchema",
    "DuplicateKeySchema",
    "Entry",
    "Unsupported",
    "InputIdentity",
    "Admission",
    "Recording",
    "Support",
    "Synchronization",
    "Input",
    "Temporal",
    "SampleBound",
    "SynchronizationFit",
    "Reconstruction",
    "Result",
    "ScoreComponents",
    "MissingResultCodec",
    "DuplicateResultCodec",
    "Payload",
    "MissingPayload",
    "Manifest",
    "Text",
    "UnsupportedSchema",
    "Derived",
    "RecordingResult",
    "TemporalResult",
    "NonCanonical",
    "Report",
    "ReportSpec",
    "Covariates"
  )
  val resolve: DiagnosticFamily = error("resolve")(
    "MissingManifest",
    "UnreadableManifest",
    "ManifestDigest",
    "ManifestDecode",
    "RefusedManifest",
    "Missing",
    "Unreadable",
    "Refused",
    "Length",
    "Digest",
    "Text",
    "Syntax",
    "Schema",
    "Decode",
    "Identity",
    "Relation"
  )
  val sourceFailure: DiagnosticFamily = error("source-failure")(
    "Missing",
    "Unreadable",
    "OutsideRoot",
    "NotRegularFile",
    "Oversize"
  )
  val relation: DiagnosticFamily = error("relation")(
    "Prerequisites",
    "ResultInput",
    "Description",
    "Admission",
    "RefusedAdmission",
    "BaseStudy",
    "RecordingIdentity",
    "UnreferencedPayload",
    "Unavailable",
    "RecordingPrerequisites",
    "TemporalPrerequisites",
    "ReportSpec",
    "ReportBinding",
    "ReportInput",
    "ReportLedger",
    "ReportMembers",
    "ReportCell",
    "ReportRecomputed",
    "ReportComponents",
    "SourceBinding",
    "RunPlan",
    "RunInput",
    // CR4 S2: a generic analysis relation across families, or an undeclared embedded input.
    "AnalysisFamily",
    "UndeclaredEmbedding",
    // CR4 S2 review: a declared embedded input the plan does not expose.
    "EmptyEmbedding"
  )
  val manifest: DiagnosticFamily = error("manifest")(
    "InvalidName",
    "DuplicateName",
    "MediaMismatch",
    "NegativeLength",
    "IdentityPresence",
    "PayloadDeclaration",
    "LayoutLength",
    "UnknownEntry",
    "RoleMismatch",
    "PayloadOwner",
    "DuplicateRelation",
    "RelationCount",
    // CR4 S2: an analysis result's input that is not an input entry.
    "AnalysisInput"
  )
  val payload: DiagnosticFamily = error("payload")(
    "EmptyShape",
    "NegativeExtent",
    "TooLarge",
    "Count",
    "Element",
    "Length",
    "Digest",
    "NotANumber"
  )
  val byteDigest: DiagnosticFamily =
    error("byte-digest")("WrongLength", "InvalidCharacter")
  val density: DiagnosticFamily = error("density")(
    "UnknownScale",
    "UnknownKey",
    "AmbiguousKey",
    "Failed",
    "MissingPayload",
    "Payload",
    "PayloadReference",
    "RecomputeUnavailable",
    "SourceMismatch",
    "DigestMismatch",
    "Decode",
    "Geometry",
    "Materialize",
    "UnknownRow",
    "RowKeyMismatch"
  )

  val runStamp: DiagnosticFamily = error("run-stamp")("ChangedPlan", "ChangedInput")

  val families: Vector[DiagnosticFamily] =
    Vector(
      codec,
      resolve,
      sourceFailure,
      relation,
      manifest,
      payload,
      byteDigest,
      density,
      runStamp
    )

  val codes: Vector[DiagnosticCode] = families.flatMap(_.codes)

  /** Every family of both tables, plan's first. */
  val all: Vector[DiagnosticFamily] = DiagnosticCatalog.families ++ families

/** Operand and locus helpers for the codec's own structured values. */
private[eyes4s] object CodecDiagnosticSupport:
  import DiagnosticSupport.*

  def digest(value: ByteDigest): Operand[Nothing]    = artifact(value.hex)
  def entry(value: ArtifactName): Operand[Nothing]   = name(value.value)
  def role(value: ArtifactRole): Operand[Nothing]    = token(value.wire)
  def media(value: MediaKind): Operand[Nothing]      = token(value.wire)
  def json(value: Json): Operand[Nothing]            = text(value.noSpaces)
  def element(value: ElementKind): Operand[Nothing]  = token(value.wire)
  def layout(value: PayloadLayout): Operand[Nothing] = fields(
    "element"   -> element(value.element),
    "shape"     -> ints(value.shape),
    "order"     -> token(value.order.wire),
    "byteOrder" -> token(value.byteOrder.wire)
  )
  def payloadRef(value: PayloadRef): Operand[Nothing] =
    fields("sha256" -> digest(value.sha256), "layout" -> layout(value.layout))
  def relation(value: ManifestRelation): Operand[Nothing] =
    Operand.Fields(
      (("kind" -> token(value.kind)) +: value.endpoints
        .map((field, name, _) => field -> entry(name))) ++ Option
        .when(value.analysisInputs.nonEmpty)(
          "inputs" -> Operand.Items(value.analysisInputs.map(entry))
        )
        .toVector
    )
  def changes(values: Vector[PlanChange]): Operand[Nothing] =
    Operand.Items(
      values.map(change =>
        fields(
          "field"  -> name(change.field),
          "before" -> params(change.before),
          "after"  -> params(change.after)
        )
      )
    )

  def at(value: ArtifactName): Locus[Nothing]     = Locus.Entry(value.value)
  def at(value: ManifestRelation): Locus[Nothing] =
    Locus.Relation(value.kind, value.source.value)
  def manifestAt(value: ByteDigest): Locus[Nothing] = Locus.Artifact(value.hex)

/** The projections behind [[CodecDiagnostics]]. Wrapped plan, admission,
  * recording and result errors project through their own families, so a
  * decode failure names the trial, record or sample it concerns. Keys keep
  * their runtime values here; the instances wrap them as [[ErasedKey]].
  */
private[codec] object CodecProjections:
  import CodecDiagnosticSupport.*
  import DiagnosticSupport.*
  import CodecDiagnosticCatalog as C

  private def merged(prefix: Vector[Locus[Any]], inner: Diagnostic[Any]): Vector[Locus[Any]] =
    DiagnosticSupport.merge(prefix, inner)

  def codec(e: CodecError): Diagnostic[Any] =
    import CodecError.*
    def path(value: String): Vector[Locus[Any]] = Vector(Locus.Path(value))
    def wrap(inner: Diagnostic[Any])            =
      diagnostic(C.codec, e, e.message, inner.subject)(cause(inner))
    def atPath(where: String, inner: Diagnostic[Any]) =
      diagnostic(C.codec, e, e.message, merged(path(where), inner))(name(where), cause(inner))
    e match
      case InvalidJson(input, reason) =>
        diagnostic[Any](C.codec, e, e.message)(text(input), text(reason))
      case Field(where, input, reason) =>
        diagnostic(C.codec, e, e.message, path(where))(name(where), json(input), text(reason))
      case Schema(expected, found) =>
        diagnostic[Any](C.codec, e, e.message)(definition(expected), definition(found))
      case Definition(underlying)         => wrap(Projections.plan(underlying))
      case DuplicateKeys(schema, indices) =>
        diagnostic[Any](C.codec, e, e.message)(definition(schema), ints(indices))
      case MissingIdentity(kind, id) =>
        diagnostic[Any](C.codec, e, e.message)(token(kind), name(id))
      case IdentityConflict(kind, id, existing, incoming) =>
        diagnostic[Any](C.codec, e, e.message)(
          token(kind),
          name(id),
          json(existing),
          json(incoming)
        )
      case MissingMethod(method) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(method)))(definition(method))
      case DuplicateMethod(method) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(method)))(definition(method))
      case MissingKeySchema(schema) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(schema)))(definition(schema))
      case DuplicateKeySchema(schema) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(schema)))(definition(schema))
      case Entry(where, underlying)   => atPath(where, codec(underlying))
      case Unsupported(where, reason) =>
        diagnostic(C.codec, e, e.message, path(where))(name(where), text(reason))
      case InputIdentity(declared, reconstructed) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Artifact(declared)))(
          artifact(declared),
          artifact(reconstructed)
        )
      case Admission(underlying)        => wrap(Projections.admission(underlying))
      case Recording(where, underlying) => atPath(where, Diagnose.recording(underlying))
      case Support(where, underlying)   => atPath(where, Diagnose.detectionSupport(underlying))
      case Synchronization(where, underlying) =>
        atPath(where, Diagnose.syncEvidence(underlying))
      case Input(underlying)                    => wrap(Projections.recordingInput(underlying))
      case Temporal(underlying)                 => wrap(Projections.temporal(underlying))
      case SampleBound(where, samples, maximum) =>
        diagnostic(C.codec, e, e.message, path(where))(name(where), int(samples), int(maximum))
      case SynchronizationFit(where, declaredOffset, declaredDrift, refitOffset, refitDrift) =>
        diagnostic(C.codec, e, e.message, path(where))(
          name(where),
          Operand.Micros(declaredOffset),
          real(declaredDrift),
          Operand.Micros(refitOffset),
          real(refitDrift)
        )
      case Reconstruction(underlying)       => wrap(Projections.reconstruction(underlying))
      case Result(underlying)               => wrap(Projections.result(underlying))
      case ScoreComponents(expected, found) =>
        diagnostic[Any](C.codec, e, e.message)(names(expected), names(found))
      case MissingResultCodec(method) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(method)))(definition(method))
      case DuplicateResultCodec(method) =>
        diagnostic(C.codec, e, e.message, Vector(Locus.Definition(method)))(definition(method))
      case Payload(where, underlying)       => atPath(where, payload(underlying))
      case MissingPayload(where, reference) =>
        diagnostic(C.codec, e, e.message, path(where))(name(where), payloadRef(reference))
      case Manifest(underlying) => wrap(manifest(underlying))
      case Text(offset, reason) =>
        diagnostic[Any](C.codec, e, e.message)(int(offset), text(reason))
      case UnsupportedSchema(kind, found, supported) =>
        diagnostic[Any](C.codec, e, e.message)(
          token(kind),
          definition(found),
          Operand.Items(supported.map(definition))
        )
      case Derived(where, declared, derived) =>
        diagnostic(C.codec, e, e.message, path(where))(
          name(where),
          json(declared),
          json(derived)
        )
      case RecordingResult(underlying) => wrap(Projections.recordingResult(underlying))
      case TemporalResult(underlying)  => wrap(Projections.temporalResult(underlying))
      case NonCanonical(where, found, canonical, rule) =>
        diagnostic(C.codec, e, e.message, path(where))(
          name(where),
          json(found),
          json(canonical),
          text(rule)
        )
      case Report(underlying) =>
        wrap(eyes4s.results.ResultsDiagnostics.reportError[Any](underlying))
      case ReportSpec(underlying) =>
        wrap(eyes4s.results.ResultsDiagnostics.reportSpec(underlying))
      case Covariates(underlying) =>
        wrap(eyes4s.results.ResultsDiagnostics.covariate[Any](underlying))

  def resolve(e: ResolveError): Diagnostic[Any] =
    import ResolveError.*
    def of(name: ArtifactName)      = Vector(at(name))
    def stored(address: ByteDigest) = Vector(manifestAt(address))
    e match
      case MissingManifest(address) =>
        diagnostic(C.resolve, e, e.message, stored(address))(digest(address))
      case UnreadableManifest(address, reason) =>
        diagnostic(C.resolve, e, e.message, stored(address))(digest(address), text(reason))
      case ManifestDigest(address, actual) =>
        diagnostic(C.resolve, e, e.message, stored(address))(digest(address), digest(actual))
      case ManifestDecode(address, underlying) =>
        val inner = codec(underlying)
        diagnostic(C.resolve, e, e.message, merged(stored(address), inner))(
          digest(address),
          cause(inner)
        )
      case RefusedManifest(address, failure) =>
        diagnostic(C.resolve, e, e.message, stored(address))(
          digest(address),
          cause(sourceFailure(failure))
        )
      case Missing(name) => diagnostic(C.resolve, e, e.message, of(name))(entry(name))
      case Unreadable(name, reason) =>
        diagnostic(C.resolve, e, e.message, of(name))(entry(name), text(reason))
      case Refused(name, failure) =>
        diagnostic(C.resolve, e, e.message, of(name))(
          entry(name),
          cause(sourceFailure(failure))
        )
      case Length(name, declared, actual) =>
        diagnostic(C.resolve, e, e.message, of(name))(entry(name), long(declared), int(actual))
      case Digest(name, declared, actual) =>
        diagnostic(C.resolve, e, e.message, of(name))(
          entry(name),
          digest(declared),
          digest(actual)
        )
      case Text(name, offset) =>
        diagnostic(C.resolve, e, e.message, of(name))(entry(name), int(offset))
      case Syntax(name, reason) =>
        diagnostic(C.resolve, e, e.message, of(name))(entry(name), text(reason))
      case Schema(name, declared, found) =>
        diagnostic(C.resolve, e, e.message, of(name))(
          entry(name),
          definition(declared),
          definition(found)
        )
      case Decode(name, underlying) =>
        val inner = codec(underlying)
        diagnostic(C.resolve, e, e.message, merged(of(name), inner))(entry(name), cause(inner))
      case Identity(name, declared, reconstructed) =>
        diagnostic(C.resolve, e, e.message, of(name))(
          entry(name),
          artifact(declared),
          artifact(reconstructed)
        )
      case Relation(value, mismatch) =>
        val inner = relationMismatch(mismatch)
        diagnostic(C.resolve, e, e.message, merged(Vector(at(value)), inner))(
          relation(value),
          cause(inner)
        )

  def sourceFailure(e: SourceFailure): Diagnostic[Nothing] =
    import SourceFailure.*
    val d = diagnostic[Nothing](C.sourceFailure, e, e.message)
    e match
      case Missing                         => d()
      case Unreadable(reason)              => d(text(reason))
      case OutsideRoot(location, root)     => d(text(location), text(root))
      case NotRegularFile(location)        => d(text(location))
      case Oversize(location, size, limit) => d(text(location), long(size), long(limit))

  def relationMismatch(e: RelationMismatch): Diagnostic[Any] =
    import RelationMismatch.*
    e match
      case Prerequisites(errors) =>
        val inner = errors.map(Projections.plan)
        diagnostic(C.relation, e, e.message, inner.flatMap(_.subject).distinct)(
          Operand.Causes(inner)
        )
      case ResultInput(expected, found) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Artifact(expected)))(
          artifact(expected),
          artifact(found)
        )
      case Description(values) => diagnostic[Any](C.relation, e, e.message)(changes(values))
      case RunPlan(reported, current, values) =>
        diagnostic[Any](C.relation, e, e.message)(
          digest(reported),
          digest(current),
          changes(values)
        )
      case RunInput(reported, current) =>
        diagnostic[Any](C.relation, e, e.message)(digest(reported), digest(current))
      case AnalysisFamily(result, plan, expected) =>
        diagnostic[Any](C.relation, e, e.message)(
          definition(result),
          definition(plan),
          definition(expected)
        )
      case UndeclaredEmbedding(plan) =>
        diagnostic[Any](C.relation, e, e.message)(definition(plan))
      case EmptyEmbedding(plan) =>
        diagnostic[Any](C.relation, e, e.message)(definition(plan))
      case Admission(error) =>
        val inner = Projections.admission(error)
        diagnostic(C.relation, e, e.message, inner.subject)(cause(inner))
      case RefusedAdmission           => diagnostic[Any](C.relation, e, e.message)()
      case BaseStudy(expected, found) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Artifact(expected)))(
          artifact(expected),
          artifact(found)
        )
      case RecordingIdentity(expected, found) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Artifact(expected)))(
          artifact(expected),
          artifact(found)
        )
      case UnreferencedPayload(reference) =>
        diagnostic[Any](C.relation, e, e.message)(payloadRef(reference))
      case Unavailable(endpoints) =>
        diagnostic(C.relation, e, e.message, endpoints.map(at))(names(endpoints.map(_.value)))
      case RecordingPrerequisites(errors) =>
        val inner = errors.map(Projections.recordingInput)
        diagnostic(C.relation, e, e.message, inner.flatMap(_.subject).distinct)(
          Operand.Causes(inner)
        )
      case TemporalPrerequisites(errors) =>
        val inner = errors.map(Projections.temporal)
        diagnostic(C.relation, e, e.message, inner.flatMap(_.subject).distinct)(
          Operand.Causes(inner)
        )
      case ReportSpec(report, stored) =>
        diagnostic[Any](C.relation, e, e.message)(name(report), name(stored))
      case SourceBinding(field, expected, found) =>
        diagnostic[Any](C.relation, e, e.message)(token(field), text(expected), text(found))
      case ReportBinding(field, bound, stored) =>
        diagnostic[Any](C.relation, e, e.message)(token(field), text(bound), text(stored))
      case ReportInput(input, computed) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Entry(input)))(
          name(input),
          name(computed)
        )
      case ReportLedger(ledger, input) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Entry(ledger)))(
          name(ledger),
          name(input)
        )
      case ReportMembers(scale, unknown) =>
        diagnostic(C.relation, e, e.message, Vector(Locus.Scale(scale)))(
          int(scale),
          names(unknown)
        )
      case ReportCell(group, role, component, stored, recomputed) =>
        diagnostic[Any](C.relation, e, e.message)(
          name(group),
          token(role),
          name(component),
          text(stored),
          text(recomputed)
        )
      case ReportRecomputed(part, stored, recomputed) =>
        diagnostic[Any](C.relation, e, e.message)(token(part), text(stored), text(recomputed))
      case ReportComponents(planned, result) =>
        diagnostic[Any](C.relation, e, e.message)(names(planned), names(result))

  def manifest(e: ManifestError): Diagnostic[Nothing] =
    import ManifestError.*
    def of(name: ArtifactName) = Vector(at(name))
    e match
      case InvalidName(value)  => diagnostic[Nothing](C.manifest, e, e.message)(text(value))
      case DuplicateName(name) =>
        diagnostic(C.manifest, e, e.message, of(name))(entry(name))
      case MediaMismatch(name, value, kind) =>
        diagnostic(C.manifest, e, e.message, of(name))(entry(name), role(value), media(kind))
      case NegativeLength(name, length) =>
        diagnostic(C.manifest, e, e.message, of(name))(entry(name), long(length))
      case IdentityPresence(name, value, identity) =>
        diagnostic(C.manifest, e, e.message, of(name))(
          entry(name),
          role(value),
          optional(identity.map(artifact))
        )
      case PayloadDeclaration(name, value, schema, declared) =>
        diagnostic(C.manifest, e, e.message, of(name))(
          entry(name),
          role(value),
          definition(schema),
          optional(declared.map(layout))
        )
      case LayoutLength(name, declared, bytes) =>
        diagnostic(C.manifest, e, e.message, of(name))(entry(name), long(declared), int(bytes))
      case UnknownEntry(value, name) =>
        diagnostic(C.manifest, e, e.message, Vector(at(value), at(name)))(
          relation(value),
          entry(name)
        )
      case RoleMismatch(value, name, expected, found) =>
        diagnostic(C.manifest, e, e.message, Vector(at(value), at(name)))(
          relation(value),
          entry(name),
          role(expected),
          role(found)
        )
      case PayloadOwner(value, schema) =>
        diagnostic(C.manifest, e, e.message, Vector(at(value)))(
          relation(value),
          definition(schema)
        )
      case DuplicateRelation(value) =>
        diagnostic(C.manifest, e, e.message, Vector(at(value)))(relation(value))
      case RelationCount(name, kind, count, expected) =>
        diagnostic(C.manifest, e, e.message, of(name))(
          entry(name),
          token(kind),
          int(count),
          text(expected)
        )
      case AnalysisInput(value, name, found) =>
        diagnostic(C.manifest, e, e.message, Vector(at(value), at(name)))(
          relation(value),
          entry(name),
          role(found)
        )

  def payload(e: PayloadError): Diagnostic[Nothing] =
    import PayloadError.*
    val d = diagnostic[Nothing](C.payload, e, e.message)
    e match
      case EmptyShape                          => d()
      case NegativeExtent(axis, extent)        => d(int(axis), int(extent))
      case TooLarge(kind, shape, maximumBytes) =>
        d(element(kind), ints(shape), int(maximumBytes))
      case Count(value, values)         => d(layout(value), int(values))
      case Element(declared, requested) => d(element(declared), element(requested))
      case Length(declared, actual)     => d(int(declared), int(actual))
      case Digest(declared, actual)     => d(digest(declared), digest(actual))
      case NotANumber(index)            => d(int(index))

  def byteDigest(e: ByteDigestError): Diagnostic[Nothing] =
    import ByteDigestError.*
    val d = diagnostic[Nothing](C.byteDigest, e, e.message)
    e match
      case WrongLength(value, length)                => d(text(value), int(length))
      case InvalidCharacter(value, index, character) =>
        d(text(value), int(index), text(character.toString))

  def density[K](e: DensityError[K]): Diagnostic[Any] =
    import DensityError.*
    def at(scale: Int, key: K): Vector[Locus[Any]] =
      Vector(Locus.Scale(scale), Locus.Trial(key))
    e match
      case UnknownScale(scale, count) =>
        diagnostic(C.density, e, e.message, Vector(Locus.Scale(scale)))(int(scale), int(count))
      case UnknownKey(scale, key) =>
        diagnostic(C.density, e, e.message, at(scale, key))(int(scale), Operand.Key(key))
      case AmbiguousKey(scale, key, count) =>
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          int(count)
        )
      case Failed(scale, key, failure) =>
        val inner = Projections.failure(failure)
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          cause(inner)
        )
      case MissingPayload(scale, key, reference) =>
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          payloadRef(reference)
        )
      case Payload(scale, key, underlying) =>
        val inner = payload(underlying)
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          cause(inner)
        )
      case PayloadReference(scale, key, expected, actual) =>
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          payloadRef(expected),
          payloadRef(actual)
        )
      case RecomputeUnavailable(scale, key) =>
        diagnostic(C.density, e, e.message, at(scale, key))(int(scale), Operand.Key(key))
      case SourceMismatch(scale, key, expected, actual) =>
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          text(expected),
          text(actual)
        )
      case DigestMismatch(scale, key, expected, actual) =>
        diagnostic(C.density, e, e.message, at(scale, key))(
          int(scale),
          Operand.Key(key),
          digest(expected),
          digest(actual)
        )
      case Decode(scale, key, underlying) =>
        val inner = codec(underlying)
        diagnostic(C.density, e, e.message, merged(at(scale, key), inner))(
          int(scale),
          Operand.Key(key),
          cause(inner)
        )
      case Geometry(underlying) =>
        val inner = Projections.inspection(underlying)
        diagnostic(C.density, e, e.message, inner.subject)(cause(inner))
      case Materialize(underlying) =>
        val inner = codec(underlying)
        diagnostic(C.density, e, e.message, inner.subject)(cause(inner))
      case UnknownRow(scale, row, count) =>
        diagnostic(C.density, e, e.message, Vector(Locus.Scale(scale), Locus.InputTrial(row)))(
          int(scale),
          int(row),
          int(count)
        )
      case RowKeyMismatch(scale, row, expected, actual) =>
        diagnostic(C.density, e, e.message, at(scale, expected) :+ Locus.InputTrial(row))(
          int(scale),
          int(row),
          Operand.Key(expected),
          Operand.Key(actual)
        )

/** The codec's [[Diagnose]] instances. Import `CodecDiagnostics.given` for
  * `Diagnostic.of` over the codec's families. A decoded study's key type is
  * not known statically where the codec reports, so trial keys inside a
  * wrapped plan, admission or result error are [[ErasedKey]]s; narrow a
  * diagnostic to the application's key type with `ErasedKey.narrow`.
  */
object CodecDiagnostics:
  import CodecDiagnosticCatalog as C
  import CodecProjections.*

  private def erased[E](of: DiagnosticFamily)(f: E => Diagnostic[Any]): Diagnose[E, ErasedKey] =
    Diagnose.instance(of)(error => f(error).mapKeys(new ErasedKey(_)))

  given codecError: Diagnose[CodecError, ErasedKey]      = erased(C.codec)(codec)
  given resolveError: Diagnose[ResolveError, ErasedKey]  = erased(C.resolve)(resolve)
  given sourceFailures: Diagnose[SourceFailure, Nothing] =
    Diagnose.instance(C.sourceFailure)(sourceFailure)
  given relationMismatches: Diagnose[RelationMismatch, ErasedKey] =
    erased(C.relation)(relationMismatch)
  given manifestError: Diagnose[ManifestError, Nothing] =
    Diagnose.instance(C.manifest)(manifest)
  given payloadError: Diagnose[PayloadError, Nothing] = Diagnose.instance(C.payload)(payload)
  given byteDigestError: Diagnose[ByteDigestError, Nothing] =
    Diagnose.instance(C.byteDigest)(byteDigest)
  given densityError[K]: Diagnose[DensityError[K], ErasedKey] =
    erased(C.density)(density[K])

  given runStampError[P, I]: Diagnose[RunStampError[P, I], Nothing] =
    Diagnose.instance(C.runStamp) { error =>
      import DiagnosticSupport.diagnostic
      import CodecDiagnosticSupport.{digest, changes}
      error match
        case RunStampError.ChangedPlan(reported, current, values) =>
          diagnostic[Nothing](C.runStamp, error, error.message)(
            digest(reported.sha256),
            digest(current.sha256),
            changes(values)
          )
        case RunStampError.ChangedInput(reported, current) =>
          diagnostic[Nothing](C.runStamp, error, error.message)(
            digest(reported.sha256),
            digest(current.sha256)
          )
    }
