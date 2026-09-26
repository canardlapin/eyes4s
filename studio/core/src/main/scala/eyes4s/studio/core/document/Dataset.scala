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
import eyes4s.plan.{AdmissionPolicy, ArtifactRef, CorrectionScope, OffScreenPolicy}
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

/** A source's path inside the project bundle: relative, `/`-separated, with
  * no empty, `.` or `..` segment.
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

/** One imported file: its path in the bundle, the SHA-256 of its exact bytes
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
  /** Import requires these (DESIGN_SPEC section 9: ordinal and sample count). */
  val required: Vector[ColumnRole] =
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

/** Which column plays each role: every required role once, no column in two
  * roles, in role order.
  */
final case class ColumnMapping private (bindings: Vector[ColumnBinding]) derives CanEqual:
  def column(role: ColumnRole): Option[ColumnName] = bindings.find(_.role == role).map(_.column)

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
      missing = ColumnRole.required.filterNot(r => bindings.exists(_.role == r))
      _ <- Either.cond(missing.isEmpty, (), DocumentError.MissingColumnRoles(missing))
    yield new ColumnMapping(bindings.sortBy(_.role.ordinal))

  given Codec[ColumnMapping] = DocumentCodecs.validated(of, _.bindings)

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
  * admission ledger and trial inventory it produced.
  */
enum AdmissionDecision derives CanEqual:
  case Pending
  case Verifying(content: CanonicalDigest[DatasetRevisionSpec])
  case Admitted(
      ledger: CoreBinding[AdmissionLedgerArtifact],
      inventory: CoreBinding[TrialInventoryArtifact]
  )

  def isAdmitted: Boolean = this match
    case Pending | Verifying(_) => false
    case Admitted(_, _)         => true

object AdmissionDecision:
  import DigestJson.given
  given Codec.AsObject[AdmissionDecision] = Codec.AsObject.derived

// ---------------------------------------------------------------------------
// Dataset revision
// ---------------------------------------------------------------------------

/** One dataset revision (DESIGN_SPEC section 8, "Dataset · re-admit"): the
  * sources, their column mapping and declared units, the display geometry,
  * the admission choices and the admission decision. `parent` is the
  * revision it re-imports, if any. Its id is the protocol's [[DatasetRevision]].
  */
final case class DatasetRevisionSpec(
    id: DatasetRevision,
    parent: Option[DatasetRevision],
    sources: Sources,
    mapping: ColumnMapping,
    units: DeclaredUnits,
    geometry: Geometry,
    admission: AdmissionChoice,
    decision: AdmissionDecision
) derives CanEqual,
      Codec.AsObject

object DatasetRevisionSpec:
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
