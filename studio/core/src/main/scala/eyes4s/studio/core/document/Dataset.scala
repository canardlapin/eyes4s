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

package eyes4s.studio.core.document

import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest, CodecError, VersionedCodec}
import eyes4s.kernel.Correction
import eyes4s.plan.{
  AdmissionDecision as CoreAdmissionDecision,
  AdmissionPolicy,
  AppliedCorrection,
  ArtifactRef,
  AttributeColumn,
  AttributeKind,
  CorrectionScope,
  OffScreenPolicy
}
import eyes4s.studio.core.backend.{DatasetRevision, TrialKey}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

// ---------------------------------------------------------------------------
// Bindings to eyes4s artifacts
// ---------------------------------------------------------------------------

/** Kinds of eyes4s artifact a document binds to by [[CanonicalDigest]]. They
  * are markers: studio names the artifact's kind, eyes4s owns its type.
  */
sealed trait StudyPlanArtifact
sealed trait AdmissionLedgerArtifact
sealed trait TrialInventoryArtifact
sealed trait ResultArchiveArtifact

/** The collision-resistant identity (CR3 [[CanonicalDigest]]) of an eyes4s
  * artifact a document value stands on, or `Unbound` while no eyes4s artifact
  * exists for it (a value served by `FakeStudyBackend`, or one eyes4s has not
  * yet produced). A digest is never invented to fill the gap.
  */
enum CoreBinding[A]:
  case Unbound()
  case Bound(digest: CanonicalDigest[A])

  def render: String = this match
    case Unbound()     => "unbound"
    case Bound(digest) => digest.display

object CoreBinding:
  given [A]: CanEqual[CoreBinding[A], CoreBinding[A]] = CanEqual.derived

  def unbound[A]: CoreBinding[A] = Unbound()

  given [A]: Encoder[CoreBinding[A]] = Encoder.instance {
    case Unbound()     => Json.obj("Unbound" -> Json.obj())
    case Bound(digest) => Json.obj("Bound" -> Json.obj("sha256" -> digest.sha256.hex.asJson))
  }

  given [A]: Decoder[CoreBinding[A]] = Decoder.instance { c =>
    c.keys.map(_.toVector) match
      case Some(Vector("Unbound")) => Right(Unbound())
      case Some(Vector("Bound"))   =>
        c.downField("Bound")
          .get[String]("sha256")
          .flatMap(hex =>
            CanonicalDigest
              .parse[A](hex)
              .bimap(e => DecodingFailure(e.message, c.history), Bound(_))
          )
      case other => Left(DecodingFailure(s"expected Unbound or Bound, got $other", c.history))
  }

// ---------------------------------------------------------------------------
// Sources
// ---------------------------------------------------------------------------

/** What an imported file is to the dataset. */
enum SourceRole derives CanEqual, Codec.AsObject:
  case Fixations, Trials

  def label: String = this match
    case Fixations => "fixations"
    case Trials    => "trial inventory"

/** The name a source was imported under: relative, `/`-separated, with no
  * empty, `.` or `..` segment. It is not where the bundle keeps the bytes:
  * `ProjectBundle.inputPath` maps a source to its one stored path,
  * `inputs/<sha256>/<last segment of this path>` (S2.3).
  */
final case class SourcePath private (value: String) derives CanEqual

object SourcePath:
  def of(value: String): Either[DocumentError, SourcePath] =
    if value.trim.isEmpty then Left(DocumentError.Blank("source path"))
    else if value.startsWith("/") then
      Left(DocumentError.BadPath(value, "it is absolute; paths are relative to the project"))
    else if value.contains('\\') then
      Left(DocumentError.BadPath(value, "it contains a backslash; the separator is '/'"))
    else if value.split("/", -1).exists(s => s.isEmpty || s == "." || s == "..") then
      Left(DocumentError.BadPath(value, "it has an empty, '.' or '..' segment"))
    else Right(new SourcePath(value))

  given Codec[SourcePath] = DocumentCodecs.validated(of, _.value)

/** The eyes4s semantic identity of a source: `SourceRef.records`, the 64-bit
  * digest of its parsed records (16 lowercase hexadecimal digits). It is a
  * change detector, not a cryptographic identity; the source's bytes carry
  * that.
  */
final case class SemanticIdentity private (value: String) derives CanEqual

object SemanticIdentity:
  def of(value: String): Either[DocumentError, SemanticIdentity] =
    ArtifactRef
      .parse[Vector[Vector[String]]](value)
      .bimap(_ => DocumentError.BadSemanticIdentity(value), r => new SemanticIdentity(r.digest))

  def fromCore(ref: ArtifactRef[Vector[Vector[String]]]): SemanticIdentity =
    new SemanticIdentity(ref.digest)

  given Codec[SemanticIdentity] = DocumentCodecs.validated(of, _.value)

private[document] object DigestCodecs:
  given Codec[ByteDigest] = Codec.from(
    Decoder[String].emap(s => ByteDigest.parse(s).left.map(_.message)),
    Encoder[String].contramap(_.hex)
  )

/** A [[CanonicalDigest]] as JSON: its 64 lowercase hexadecimal digits. */
object DigestJson:
  given [A]: Codec[CanonicalDigest[A]] = Codec.from(
    Decoder[String].emap(s => CanonicalDigest.parse[A](s).left.map(_.message)),
    Encoder[String].contramap(_.sha256.hex)
  )

/** One imported file: its import name, the SHA-256 of its exact bytes
  * and, once eyes4s has parsed it, its semantic identity.
  */
final case class Source(
    role: SourceRole,
    path: SourcePath,
    bytes: ByteDigest,
    semantic: Option[SemanticIdentity]
) derives CanEqual

object Source:
  import DigestCodecs.given
  given Codec.AsObject[Source] = Codec.AsObject.derived

/** The files one dataset revision was imported from, one per role, in role
  * order. A fixation source is required; a trial inventory is optional.
  */
final case class Sources private (entries: Vector[Source]) derives CanEqual:
  def fixations: Option[Source] = entries.find(_.role == SourceRole.Fixations)
  def trials: Option[Source]    = entries.find(_.role == SourceRole.Trials)

object Sources:
  def of(entries: Vector[Source]): Either[DocumentError, Sources] =
    val roles = entries.map(_.role)
    for
      _ <- SourceRole.values.toVector.traverse_ { role =>
        val paths = entries.filter(_.role == role).map(_.path.value)
        Either.cond(paths.size <= 1, (), DocumentError.DuplicateSource(role, paths))
      }
      _ <- entries.groupBy(_.path).toVector.sortBy(_._1.value).traverse_ { (path, sharing) =>
        Either.cond(
          sharing.size <= 1,
          (),
          DocumentError.SharedPath(path.value, sharing.map(_.role))
        )
      }
      _ <- Either.cond(
        roles.contains(SourceRole.Fixations),
        (),
        DocumentError.MissingSource(SourceRole.Fixations, roles)
      )
    yield new Sources(entries.sortBy(_.role.ordinal))

  given Codec[Sources] = DocumentCodecs.validated(of, _.entries)

// ---------------------------------------------------------------------------
// Column mapping and declared units
// ---------------------------------------------------------------------------

/** What a source column means to eyes4s. */
enum ColumnRole derives CanEqual, Codec.AsObject:
  case Participant, Phase, Trial, Occurrence, Item, Response, Ordinal, SampleCount, X, Y,
    Onset, Duration

  def label: String = this match
    case SampleCount => "sample count"
    case other       => other.productPrefix.toLowerCase

object ColumnRole:
  /** Import and admission require these (DESIGN_SPEC section 9): the trial
    * key eyes4s reads (participant, phase and trial,
    * `FixationKeyReader.trial`), the ordinal and sample count, the position
    * and the times. [[ColumnMapping.admissible]] checks them.
    */
  val required: Vector[ColumnRole] =
    Vector(Participant, Phase, Trial, Ordinal, SampleCount, X, Y, Onset, Duration)

  /** Every stored mapping has these: the roles S5.2 required, before the
    * phase was (S5.3). [[ColumnMapping.of]], and so decoding, checks only
    * these, so a project saved without a phase column still loads; it needs
    * a re-map before it is committed or admitted.
    */
  val stored: Vector[ColumnRole] =
    Vector(Participant, Trial, Ordinal, SampleCount, X, Y, Onset, Duration)

/** A column name as it appears in the source header. */
final case class ColumnName private (value: String) derives CanEqual

object ColumnName:
  def of(value: String): Either[DocumentError, ColumnName] =
    Either.cond(value.trim.nonEmpty, new ColumnName(value), DocumentError.Blank("column name"))

  given Codec[ColumnName] = DocumentCodecs.validated(of, _.value)

final case class ColumnBinding(role: ColumnRole, column: ColumnName)
    derives CanEqual,
      Codec.AsObject

/** Which column plays each role: every stored role once, no column in two
  * roles, in role order. A mapping lacking a role import requires
  * ([[ColumnRole.required]]) loads, but only [[ColumnMapping.admissible]]
  * mappings are committed or admitted.
  */
final case class ColumnMapping private (bindings: Vector[ColumnBinding]) derives CanEqual:
  def column(role: ColumnRole): Option[ColumnName] = bindings.find(_.role == role).map(_.column)

  /** The roles import requires that no column plays (a mapping stored
    * before S5.3 may lack the phase); empty when it can be committed.
    */
  def missingForImport: Vector[ColumnRole] =
    ColumnRole.required.filterNot(r => bindings.exists(_.role == r))

object ColumnMapping:
  def of(bindings: Vector[ColumnBinding]): Either[DocumentError, ColumnMapping] =
    for
      _ <- ColumnRole.values.toVector.traverse_ { role =>
        val columns = bindings.filter(_.role == role).map(_.column.value)
        Either.cond(columns.size <= 1, (), DocumentError.DuplicateColumnRole(role, columns))
      }
      _ <- bindings.groupBy(_.column).toVector.sortBy(_._1.value).traverse_ { (column, bs) =>
        Either.cond(bs.size <= 1, (), DocumentError.SharedColumn(column.value, bs.map(_.role)))
      }
      missing = ColumnRole.stored.filterNot(r => bindings.exists(_.role == r))
      _ <- Either.cond(missing.isEmpty, (), DocumentError.MissingColumnRoles(missing))
    yield new ColumnMapping(bindings.sortBy(_.role.ordinal))

  /** The commit and admission check (S5.3): every role import requires has a
    * column, or the error names the missing ones. The import wizard's commit,
    * `ImportSources`, `ReviseDataset` and `VerifyDataset` call it.
    */
  def admissible(mapping: ColumnMapping): Either[DocumentError, ColumnMapping] =
    val missing = mapping.missingForImport
    Either.cond(missing.isEmpty, mapping, DocumentError.MissingColumnRoles(missing))

  given Codec[ColumnMapping] = DocumentCodecs.validated(of, _.bindings)

/** The declared type of an attribute column (eyes4s `AttributeKind`). */
enum AttributeKindChoice derives CanEqual, Codec.AsObject:
  /** Any text, kept as written. */
  case Text

  /** A signed decimal integer. */
  case Integer

  /** A finite decimal number. */
  case Number

  def core: AttributeKind = this match
    case Text    => AttributeKind.Text
    case Integer => AttributeKind.Integer
    case Number  => AttributeKind.Number

/** A source column without a role, declared as an attribute: UI-H reads
  * only declared columns, so a column that is to pass through is declared.
  */
final case class AttributeBinding(column: ColumnName, kind: AttributeKindChoice)
    derives CanEqual,
      Codec.AsObject:
  def core: AttributeColumn = AttributeColumn(column.value, kind.core)

/** The attribute columns of a source, in header order, each named once. */
final case class DeclaredAttributes private (bindings: Vector[AttributeBinding])
    derives CanEqual:
  def isEmpty: Boolean              = bindings.isEmpty
  def columns: Vector[ColumnName]   = bindings.map(_.column)
  def core: Vector[AttributeColumn] = bindings.map(_.core)

object DeclaredAttributes:
  val empty: DeclaredAttributes = new DeclaredAttributes(Vector.empty)

  def of(bindings: Vector[AttributeBinding]): Either[DocumentError, DeclaredAttributes] =
    bindings
      .map(_.column)
      .distinct
      .collectFirst {
        case c if bindings.count(_.column == c) > 1 => DocumentError.RepeatedAttribute(c.value)
      }
      .toLeft(new DeclaredAttributes(bindings))

  given Encoder[DeclaredAttributes] = Encoder[Vector[AttributeBinding]].contramap(_.bindings)

  /** A missing field reads as no attributes: documents and journals written
    * before attributes were recorded (S5.2) had none.
    */
  given Decoder[DeclaredAttributes] = new Decoder[DeclaredAttributes]:
    private val values = Decoder[Vector[AttributeBinding]].emap(of(_).left.map(_.message))
    def apply(c: io.circe.HCursor): Decoder.Result[DeclaredAttributes]              = values(c)
    override def tryDecode(c: io.circe.ACursor): Decoder.Result[DeclaredAttributes] =
      if c.failed then Right(empty) else values.tryDecode(c)

enum TimeUnit derives CanEqual, Codec.AsObject:
  case Milliseconds, Microseconds, Seconds

  def symbol: String = this match
    case Milliseconds => "ms"
    case Microseconds => "µs"
    case Seconds      => "s"

/** Units the analyst declared for the source; eyes4s never infers them.
  * `time` is `None` while onset and duration units are undeclared.
  */
final case class DeclaredUnits(time: Option[TimeUnit]) derives CanEqual, Codec.AsObject:
  def render: String = time.fold("time units undeclared")(u => s"time declared ${u.symbol}")

// ---------------------------------------------------------------------------
// Geometry
// ---------------------------------------------------------------------------

private[document] object Checks:
  def positive(field: String, value: Int): Either[DocumentError, Int] =
    Either.cond(value > 0, value, DocumentError.NotPositive(field, value.toDouble))

  def positive(field: String, value: Double): Either[DocumentError, Double] =
    if !value.isFinite then Left(DocumentError.NotFinite(field, value))
    else Either.cond(value > 0.0, value, DocumentError.NotPositive(field, value))

  def finite(field: String, value: Double): Either[DocumentError, Double] =
    Either.cond(value.isFinite, value, DocumentError.NotFinite(field, value))

  def within(
      field: String,
      value: Double,
      lower: Double,
      upper: Double
  ): Either[DocumentError, Double] =
    Either.cond(
      value >= lower && value <= upper,
      value,
      DocumentError.OutOfRange(field, value, lower, upper)
    )

  def nonBlank(field: String, value: String): Either[DocumentError, String] =
    Either.cond(value.trim.nonEmpty, value, DocumentError.Blank(field))

/** The screen in pixels; its frame is the admission frame. */
final case class ScreenSize private (width: Int, height: Int) derives CanEqual:
  def render: String = s"${width}×$height px"

object ScreenSize:
  def of(width: Int, height: Int): Either[DocumentError, ScreenSize] =
    (Checks.positive("screen width", width), Checks.positive("screen height", height))
      .mapN(new ScreenSize(_, _))

  given Encoder.AsObject[ScreenSize] =
    Encoder.forProduct2("width", "height")(s => (s.width, s.height))
  given Decoder[ScreenSize] =
    Decoder.forProduct2("width", "height")(of).emap(_.left.map(_.message))

/** Where the stimulus image lies on the screen, in screen pixels (y down,
  * from the top-left corner); the analysis window is this image frame.
  */
final case class ImagePlacement private (left: Int, top: Int, width: Int, height: Int)
    derives CanEqual:
  def render: String = s"${width}×$height px at ($left, $top)"

object ImagePlacement:
  def of(left: Int, top: Int, width: Int, height: Int): Either[DocumentError, ImagePlacement] =
    (Checks.positive("image width", width), Checks.positive("image height", height))
      .mapN((w, h) => new ImagePlacement(left, top, w, h))

  given Encoder.AsObject[ImagePlacement] =
    Encoder.forProduct4("left", "top", "width", "height")(p =>
      (p.left, p.top, p.width, p.height)
    )
  given Decoder[ImagePlacement] =
    Decoder.forProduct4("left", "top", "width", "height")(of).emap(_.left.map(_.message))

/** Pixels per degree of visual angle as the analyst DECLARED it: a linear
  * conversion, not a calibration (DESIGN_SPEC section 9).
  */
final case class DeclaredPixelsPerDegree private (value: Double) derives CanEqual

object DeclaredPixelsPerDegree:
  def of(value: Double): Either[DocumentError, DeclaredPixelsPerDegree] =
    Checks.positive("pixels per degree", value).map(new DeclaredPixelsPerDegree(_))

  given Codec[DeclaredPixelsPerDegree] = DocumentCodecs.validated(of, _.value)

/** The display geometry of a dataset: the image lies inside the screen. */
final case class Geometry private (
    screen: ScreenSize,
    image: ImagePlacement,
    pixelsPerDegree: DeclaredPixelsPerDegree
) derives CanEqual

object Geometry:
  def of(
      screen: ScreenSize,
      image: ImagePlacement,
      pixelsPerDegree: DeclaredPixelsPerDegree
  ): Either[DocumentError, Geometry] =
    val inside = image.left >= 0 && image.top >= 0 &&
      image.left.toLong + image.width <= screen.width &&
      image.top.toLong + image.height <= screen.height
    Either.cond(
      inside,
      new Geometry(screen, image, pixelsPerDegree),
      DocumentError.PlacementOffScreen(image, screen)
    )

  given Encoder.AsObject[Geometry] =
    Encoder.forProduct3("screen", "image", "pixelsPerDegree")(g =>
      (g.screen, g.image, g.pixelsPerDegree)
    )
  given Decoder[Geometry] =
    Decoder.forProduct3("screen", "image", "pixelsPerDegree")(of).emap(_.left.map(_.message))

// ---------------------------------------------------------------------------
// Admission
// ---------------------------------------------------------------------------

/** eyes4s `OffScreenPolicy`; changing it is a "Dataset · re-admit" change. */
enum OffScreenChoice derives CanEqual, Codec.AsObject:
  case ExcludeRecord, QuarantineTrial

  def core: OffScreenPolicy = this match
    case ExcludeRecord   => OffScreenPolicy.ExcludeRecord
    case QuarantineTrial => OffScreenPolicy.QuarantineTrial

object OffScreenChoice:
  def of(policy: OffScreenPolicy): OffScreenChoice = policy match
    case OffScreenPolicy.ExcludeRecord   => ExcludeRecord
    case OffScreenPolicy.QuarantineTrial => QuarantineTrial

/** A finite displacement in screen pixels. */
final case class Offset private (dx: Double, dy: Double) derives CanEqual

object Offset:
  def of(dx: Double, dy: Double): Either[DocumentError, Offset] =
    (Checks.finite("offset dx", dx), Checks.finite("offset dy", dy)).mapN(new Offset(_, _))

  given Encoder.AsObject[Offset] = Encoder.forProduct2("dx", "dy")(o => (o.dx, o.dy))
  given Decoder[Offset] = Decoder.forProduct2("dx", "dy")(of).emap(_.left.map(_.message))

/** eyes4s `Correction`: flips about the frame's centre line, or a translation. */
enum CoordinateCorrection derives CanEqual, Codec.AsObject:
  case FlipX, FlipY
  case Translate(offset: Offset)

object CoordinateCorrection:
  def of(correction: Correction): Either[DocumentError, CoordinateCorrection] =
    correction match
      case Correction.FlipX             => Right(FlipX)
      case Correction.FlipY             => Right(FlipY)
      case Correction.Translate(dx, dy) => Offset.of(dx, dy).map(Translate(_))

/** Participant names a participant; the key of a trial is studio's typed key. */
final case class ParticipantId private (value: String) derives CanEqual

object ParticipantId:
  def of(value: String): Either[DocumentError, ParticipantId] =
    Checks.nonBlank("participant", value).map(new ParticipantId(_))

  given Codec[ParticipantId] = DocumentCodecs.validated(of, _.value)

/** eyes4s `CorrectionScope` over [[TrialKey]]. */
enum CorrectionTarget derives CanEqual, Codec.AsObject:
  case AllTrials
  case Participant(participant: ParticipantId)
  case Trial(key: TrialKey)

final case class CorrectionRule(target: CorrectionTarget, correction: CoordinateCorrection)
    derives CanEqual,
      Codec.AsObject

/** eyes4s `AdmissionPolicy`: the off-screen policy and the coordinate
  * corrections in rule order. Changing either re-admits the dataset.
  */
final case class AdmissionChoice(
    offScreen: OffScreenChoice,
    corrections: Vector[CorrectionRule]
) derives CanEqual,
      Codec.AsObject

object AdmissionChoice:
  /** New imports (eyes4s `AdmissionPolicy.default`). */
  val default: AdmissionChoice = AdmissionChoice(OffScreenChoice.ExcludeRecord, Vector.empty)

  /** The first two rules of `choice` that both cover some trial, by position,
    * as eyes4s's `AdmissionPolicy.correctionFor` finds them (S5.5 follow-up):
    * eyes4s refuses a policy in which two rules cover one trial. Rules are
    * compared by their targets, without the source: each rule's own trial
    * (a trial rule's key, a participant rule's participant, any trial for
    * all trials) is asked for its covering rules, so two rules overlap
    * exactly when one trial could fall under both.
    */
  def overlap(choice: AdmissionChoice): Option[(Int, Int)] =
    val scopes: Vector[CorrectionScope[TrialKey]] = choice.corrections.map(_.target match
      case CorrectionTarget.AllTrials      => CorrectionScope.AllTrials()
      case CorrectionTarget.Participant(p) => CorrectionScope.Participant(p.value)
      case CorrectionTarget.Trial(key)     => CorrectionScope.Trial(key))
    // The correction does not decide coverage; any one stands for each rule.
    val policy = AdmissionPolicy(
      choice.offScreen.core,
      scopes.map(AppliedCorrection(_, Correction.FlipX))
    )
    val probes = choice.corrections.map(_.target match
      case CorrectionTarget.AllTrials =>
        TrialKey("", eyes4s.studio.core.backend.Phase.Encoding, "", 1)
      case CorrectionTarget.Participant(p) =>
        TrialKey(p.value, eyes4s.studio.core.backend.Phase.Encoding, "", 1)
      case CorrectionTarget.Trial(key) => key)
    probes.iterator
      .map(policy.correctionFor(_, _.participant))
      .collectFirst { case Left(pair) => pair }

  def fromCore[K](
      policy: AdmissionPolicy[K],
      key: K => TrialKey
  ): Either[DocumentError, AdmissionChoice] =
    policy.corrections
      .traverse { applied =>
        val target = applied.scope match
          case CorrectionScope.AllTrials()    => Right(CorrectionTarget.AllTrials)
          case CorrectionScope.Participant(p) =>
            ParticipantId.of(p).map(CorrectionTarget.Participant(_))
          case CorrectionScope.Trial(k) => Right(CorrectionTarget.Trial(key(k)))
        (target, CoordinateCorrection.of(applied.correction)).mapN(CorrectionRule.apply)
      }
      .map(AdmissionChoice(OffScreenChoice.of(policy.offScreen), _))

/** What became of a dataset revision's admission. `Pending` is a draft
  * being edited; `Verifying` has been sent to the backend for verification
  * (story moment t1) and records the content digest
  * ([[DatasetRevisionSpec.contentDigest]]) of exactly what was sent; an
  * admission is accepted only for that content. `Admitted` binds the eyes4s
  * admission ledger and trial inventory it produced, and records the eyes4s
  * `AdmissionDecision` it was admitted under (S5.6): `RequireComplete`, or
  * `ReviewExclusions`, which admits the admissible trials and records the
  * exclusions with their causes in the ledger. `policy` is `None` only for a
  * revision admitted before S5.6 recorded it (`studio.document` version 2 or
  * earlier, or a journal line written before S5.6).
  */
enum AdmissionDecision derives CanEqual:
  case Pending
  case Verifying(content: CanonicalDigest[DatasetRevisionSpec])
  case Admitted(
      policy: Option[CoreAdmissionDecision],
      ledger: CoreBinding[AdmissionLedgerArtifact],
      inventory: CoreBinding[TrialInventoryArtifact]
  )

  def isAdmitted: Boolean = this match
    case Pending | Verifying(_) => false
    case Admitted(_, _, _)      => true

  /** The policy an admitted revision recorded, if it recorded one. */
  def admittedUnder: Option[CoreAdmissionDecision] = this match
    case Admitted(p, _, _)      => p
    case Pending | Verifying(_) => None

object AdmissionDecision:
  import DigestJson.given

  /** eyes4s's `AdmissionDecision` as the studio writes its enums:
    * `{"RequireComplete":{}}` or `{"ReviewExclusions":{}}`.
    */
  given coreDecision: Codec[CoreAdmissionDecision] = Codec.from(
    Decoder.instance { c =>
      c.value.asObject.map(_.keys.toVector) match
        case Some(Vector(name)) =>
          CoreAdmissionDecision.values
            .find(_.toString == name)
            .toRight(DecodingFailure(s"unknown admission decision $name", c.history))
        case other =>
          Left(DecodingFailure(s"expected one admission decision, got $other", c.history))
    },
    Encoder.instance(d => Json.obj(d.toString -> Json.obj()))
  )

  private val derived: Codec.AsObject[AdmissionDecision] = Codec.AsObject.derived

  /** An admission without a recorded policy is written without the member,
    * so a revision admitted before S5.6 keeps its version-2 wire form.
    */
  given Codec.AsObject[AdmissionDecision] = Codec.AsObject.from(
    derived,
    Encoder.AsObject.instance(d =>
      derived
        .encodeObject(d)
        .mapValues(v => if d.admittedUnder.isEmpty then v.mapObject(_.remove("policy")) else v)
    )
  )

// ---------------------------------------------------------------------------
// Dataset revision
// ---------------------------------------------------------------------------

/** One dataset revision (DESIGN_SPEC section 8, "Dataset · re-admit"): the
  * sources, their column mapping and declared units, the display geometry,
  * the admission choices, the admission decision, and the fixation source's
  * attribute columns (S5.2), and the trial inventory's mapping (S5.4).
  * `parent` is the revision it re-imports, if any. Its id is the protocol's
  * [[DatasetRevision]].
  *
  * `attributes` is written only when there are some, and `inventory` only
  * when the revision maps one, so a revision without them keeps the
  * version-1 wire form (and its pins and digests). A revision stored before
  * S5.4 with a trials source and no inventory mapping loads; it needs a
  * re-map before it is committed ([[DatasetRevisionSpec.inventoryMapped]]).
  */
final case class DatasetRevisionSpec(
    id: DatasetRevision,
    parent: Option[DatasetRevision],
    sources: Sources,
    mapping: ColumnMapping,
    units: DeclaredUnits,
    geometry: Geometry,
    admission: AdmissionChoice,
    decision: AdmissionDecision,
    attributes: DeclaredAttributes = DeclaredAttributes.empty,
    inventory: Option[InventoryMapping] = None
) derives CanEqual

object DatasetRevisionSpec:
  private val derived: Codec.AsObject[DatasetRevisionSpec] = Codec.AsObject.derived

  given Codec.AsObject[DatasetRevisionSpec] = Codec.AsObject.from(
    Decoder.instance(c =>
      derived.decodeJson(
        c.value.mapObject(o =>
          if o.contains("attributes") then o else o.add("attributes", Json.arr())
        )
      )
    ),
    Encoder.AsObject.instance(s =>
      val o = derived.encodeObject(s)
      val a = if s.attributes.isEmpty then o.remove("attributes") else o
      if s.inventory.isEmpty then a.remove("inventory") else a
    )
  )

  /** An inventory mapping needs a trials source to map (S5.4). */
  def checkInventory(spec: DatasetRevisionSpec): Either[DocumentError, Unit] =
    Either.cond(
      spec.inventory.isEmpty || spec.sources.trials.isDefined,
      (),
      DocumentError.InventoryWithoutTrials(spec.id)
    )

  /** The commit check (S5.4): a trials source has its columns mapped, so
    * that eyes4s can join it. Import, re-map and verification call it.
    */
  def inventoryMapped(
      id: DatasetRevision,
      sources: Sources,
      inventory: Option[InventoryMapping]
  ): Either[DocumentError, Unit] =
    sources.trials match
      case Some(trials) if inventory.isEmpty =>
        Left(DocumentError.InventoryUnmapped(id, trials.path.value))
      case _ => Right(())

  /** The admission check (S5.4 follow-up): the fixations and the trial
    * inventory name a trial by the same key, so eyes4s joins them on it.
    * Participant, phase and trial are required in both; the occurrence must
    * be mapped in both or in neither. Like an inadmissible mapping (S5.3), a
    * revision may be imported or re-mapped while its keys disagree (one file
    * is mapped before the other); verifying it is refused.
    */
  def keysAgree(
      id: DatasetRevision,
      sources: Sources,
      mapping: ColumnMapping,
      inventory: Option[InventoryMapping]
  ): Either[DocumentError, Unit] =
    (sources.trials, inventory) match
      case (Some(trials), Some(inv)) =>
        val fixations = sources.fixations.fold("the fixations")(_.path.value.split('/').last)
        val file      = trials.path.value.split('/').last
        Vector(ColumnRole.Occurrence)
          .collectFirst(Function.unlift { role =>
            (mapping.column(role).isDefined, inv.column(role).isDefined) match
              case (true, false) =>
                Some(DocumentError.InventoryKeyDisagrees(id, role, fixations, file))
              case (false, true) =>
                Some(DocumentError.InventoryKeyDisagrees(id, role, file, fixations))
              case _ => None
          })
          .toLeft(())
      case _ => Right(())

  /** An attribute column is not also a role's column. */
  def checkAttributes(spec: DatasetRevisionSpec): Either[DocumentError, Unit] =
    spec.attributes.bindings
      .collectFirst(
        Function.unlift(a =>
          spec.mapping.bindings
            .find(_.column == a.column)
            .map(b => DocumentError.AttributeIsMapped(spec.id, a.column.value, b.role))
        )
      )
      .toLeft(())

  /** The CR3 digest of what a revision asks eyes4s to admit: the whole spec
    * with its decision set to `Pending`, so verifying it does not change it.
    */
  def contentDigest(
      spec: DatasetRevisionSpec
  ): Either[CodecError, CanonicalDigest[DatasetRevisionSpec]] =
    contentCodec.flatMap(_.digest(spec.copy(decision = AdmissionDecision.Pending)))

  private val contentCodec: Either[CodecError, VersionedCodec[DatasetRevisionSpec]] =
    StudioSchemaIds.forCodec.map { ids =>
      VersionedCodec.checked[DatasetRevisionSpec](ids.datasetContent)(s =>
        Right(CanonicalJson(s.asJson))
      )(json =>
        json
          .as[DatasetRevisionSpec]
          .left
          .map(f => CodecError.Field("dataset", json, f.getMessage))
      )
    }
