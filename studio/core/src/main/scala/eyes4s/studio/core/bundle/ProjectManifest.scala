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

package eyes4s.studio.core.bundle

import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest, CodecError, SchemaLadder, VersionedCodec}
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.document.{CanonicalJson, ScienceContent, SourceRole, StudioSchemaIds}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}

// ---------------------------------------------------------------------------
// Sharing
// ---------------------------------------------------------------------------

/** Whether a kind of input travels with the bundle. */
enum Inclusion derives CanEqual, Codec.AsObject:
  case Included, Withheld

/** What a bundle carries besides its science, chosen explicitly when it is
  * written: participant metadata and stimulus images may be withheld, for
  * instance when a project is shared outside the lab. The fixation and trial
  * sources are the science's inputs and always travel. A withheld input stays
  * listed in the manifest, so a reader can tell "withheld" from "missing".
  */
final case class SharingOptions(participantMetadata: Inclusion, stimulusImages: Inclusion)
    derives CanEqual,
      Codec.AsObject:
  def includes(kind: InputKind): Boolean = kind match
    case InputKind.Source(_)           => true
    case InputKind.ParticipantMetadata => participantMetadata == Inclusion.Included
    case InputKind.StimulusImage       => stimulusImages == Inclusion.Included

object SharingOptions:
  /** Everything travels: a working project's own bundle. */
  val complete: SharingOptions = SharingOptions(Inclusion.Included, Inclusion.Included)

// ---------------------------------------------------------------------------
// Inputs and parts
// ---------------------------------------------------------------------------

/** What a copied input is to the project. */
enum InputKind derives CanEqual, Codec.AsObject:
  case Source(role: SourceRole)
  case ParticipantMetadata
  case StimulusImage

/** One input the project lists: its kind, the SHA-256 and length of its
  * exact bytes and, while its bytes travel with the bundle, its file name.
  * A stored input lies at `inputs/<sha256>/<name>`, so its address is its
  * digest and a copy is never rewritten.
  *
  * A withheld input (see [[SharingOptions]]) keeps its kind, digest and
  * length but has no name and no path: its original file name, which may
  * identify a participant, is omitted rather than hashed, because a hash of
  * a guessable name can be reversed by trying the guesses.
  */
final case class InputEntry private (
    kind: InputKind,
    name: Option[String],
    sha256: ByteDigest,
    length: Long,
    path: Option[BundlePath]
) derives CanEqual:
  def stored: Boolean = path.isDefined

  /** The same input, withheld: its name and path omitted. */
  def withheld: InputEntry = new InputEntry(kind, None, sha256, length, None)

  /** Its order in a manifest: stored inputs by path, then withheld ones. */
  private[bundle] def sortKey: (Int, String, String) =
    (if stored then 0 else 1, path.fold("")(_.value), sha256.hex)

object InputEntry:
  /** A stored input at `inputs/<sha256>/<name>`; `name` is one path segment. */
  def of(
      kind: InputKind,
      name: String,
      sha256: ByteDigest,
      length: Long
  ): Either[BundleError, InputEntry] =
    for
      path <-
        if name.contains('/') then
          Left(BundleError.BadPath(name, PathProblem.Reserved('/', name.indexOf('/'))))
        else BundlePath.in(BundleArea.Inputs, s"${sha256.hex}/$name")
      _ <- Either.cond(length >= 0, (), BundleError.NegativeLength(path.value, length))
    yield new InputEntry(kind, Some(name), sha256, length, Some(path))

  /** A withheld input: listed by kind, digest and length only. */
  def withheld(
      kind: InputKind,
      sha256: ByteDigest,
      length: Long
  ): Either[BundleError, InputEntry] =
    Either.cond(
      length >= 0,
      new InputEntry(kind, None, sha256, length, None),
      BundleError.NegativeLength(s"withheld input ${sha256.hex}", length)
    )

  given Encoder.AsObject[InputEntry] =
    Encoder.forProduct4("kind", "name", "sha256", "length")(e =>
      (e.kind, e.name, e.sha256.hex, e.length)
    )
  given Decoder[InputEntry] = Decoder
    .forProduct4[
      (InputKind, Option[String], String, Long),
      InputKind,
      Option[String],
      String,
      Long
    ]("kind", "name", "sha256", "length")((_, _, _, _))
    .emap((kind, name, hex, length) =>
      ByteDigest
        .parse(hex)
        .left
        .map(_.message)
        .flatMap(d =>
          name
            .fold(withheld(kind, d, length))(n => of(kind, n, d, length))
            .left
            .map(_.message)
        )
    )

/** One stored part of the document: its path and the length and SHA-256 of
  * its exact bytes.
  */
final case class PartEntry(path: BundlePath, sha256: ByteDigest, length: Long) derives CanEqual

object PartEntry:
  def of(path: BundlePath, bytes: IArray[Byte]): PartEntry =
    PartEntry(path, ByteDigest.sha256(bytes), bytes.length.toLong)

  given Encoder.AsObject[PartEntry] =
    Encoder.forProduct3("path", "sha256", "length")(p => (p.path.value, p.sha256.hex, p.length))
  given Decoder[PartEntry] = Decoder
    .forProduct3[(String, String, Long), String, String, Long]("path", "sha256", "length")(
      (_, _, _)
    )
    .emap((path, hex, length) =>
      for
        p <- BundlePath.of(path).left.map(_.message)
        d <- ByteDigest.parse(hex).left.map(_.message)
        _ <- Either.cond(length >= 0, (), BundleError.NegativeLength(path, length).message)
      yield PartEntry(p, d, length)
    )

/** A dataset revision's part and its column mapping's part. */
final case class DatasetParts(dataset: PartEntry, mapping: PartEntry)
    derives CanEqual,
      Codec.AsObject

/** The document's parts, each list in the document's own order. */
final case class DocumentParts(
    datasets: Vector[DatasetParts],
    analyses: Vector[PartEntry],
    draft: Option[PartEntry],
    runs: Vector[PartEntry],
    reporting: Vector[PartEntry],
    figures: Vector[PartEntry]
) derives CanEqual,
      Codec.AsObject:
  /** Every part with the area it must lie in and a name for errors. */
  def located: Vector[(String, BundleArea, PartEntry)] =
    datasets.flatMap(d =>
      Vector(
        ("dataset", BundleArea.Datasets, d.dataset),
        ("mapping", BundleArea.Mappings, d.mapping)
      )
    ) ++ analyses.map(("analysis", BundleArea.Analyses, _)) ++
      draft.map(("draft", BundleArea.Analyses, _)) ++
      runs.map(("run", BundleArea.Runs, _)) ++
      reporting.map(("reporting spec", BundleArea.Reporting, _)) ++
      figures.map(("figure", BundleArea.Figures, _))

  def all: Vector[PartEntry] = located.map(_._3)

// ---------------------------------------------------------------------------
// The manifest
// ---------------------------------------------------------------------------

/** The science a manifest records. Every manifest this release writes is
  * `Verified`: it carries the CR3 digest of the document's science, and
  * opening checks the parts against it. `Unrecorded` is what a version 1
  * manifest lifts to, since version 1 recorded no digest; opening such a
  * bundle never reports it as verified.
  */
enum ScienceRecord derives CanEqual:
  case Verified(digest: CanonicalDigest[ScienceContent])
  case Unrecorded

object ScienceRecord:
  given Encoder[ScienceRecord] = Encoder.instance {
    case Verified(d) => Json.obj("Verified" -> Json.obj("sha256" -> d.sha256.hex.asJson))
    case Unrecorded  => Json.obj("Unrecorded" -> Json.obj())
  }
  given Decoder[ScienceRecord] = Decoder.instance { c =>
    c.keys.map(_.toVector) match
      case Some(Vector("Verified")) =>
        c.downField("Verified")
          .get[String]("sha256")
          .flatMap(h =>
            CanonicalDigest
              .parse[ScienceContent](h)
              .bimap(e => DecodingFailure(e.message, c.history), Verified(_))
          )
      case Some(Vector("Unrecorded")) => Right(Unrecorded)
      case _                          =>
        Left(
          DecodingFailure(
            "science is required: {\"Verified\": {\"sha256\": …}} or {\"Unrecorded\": {}}",
            c.history
          )
        )
  }

/** `project.json`: what a bundle holds (ticket S2.3).
  *
  *  - `document`: the schema version of the document the parts are fragments
  *    of (`studio.document@n`); they are read with that version's reader.
  *  - `science`: the required [[ScienceRecord]]: the CR3 digest of the
  *    document's science, checked on every open.
  *  - `sharing`, `inputs`: see [[SharingOptions]] and [[InputEntry]].
  *  - `parts`: every part, by path, length and SHA-256.
  *  - `presentation`: the document's presentation state, in the same
  *    document schema. It is not science, so it lives here rather than in an
  *    immutable part.
  *
  * Built only through [[ProjectManifest.of]]: every part lies in its own area,
  * no path or input is listed twice, an input is named exactly when
  * `sharing` includes its kind, and inputs are kept in path order.
  */
final case class ProjectManifest private (
    document: DefinitionId,
    science: ScienceRecord,
    sharing: SharingOptions,
    inputs: Vector[InputEntry],
    parts: DocumentParts,
    presentation: Json
) derives CanEqual:
  /** Every path the manifest names: its parts, then its stored inputs. */
  def paths: Vector[BundlePath] = parts.all.map(_.path) ++ inputs.flatMap(_.path)

object ProjectManifest:
  given CanEqual[Json, Json] = CanEqual.derived

  def of(
      document: DefinitionId,
      science: ScienceRecord,
      sharing: SharingOptions,
      inputs: Vector[InputEntry],
      parts: DocumentParts,
      presentation: Json
  ): Either[BundleError, ProjectManifest] =
    val listed = parts.all.map(_.path) ++ inputs.flatMap(_.path)
    for
      _ <- parts.located.traverse_ { (part, area, entry) =>
        entry.path.area match
          case `area` => Right(())
          case _      => Left(BundleError.PartOutsideArea(part, entry.path, area))
      }
      _ <- listed.diff(listed.distinct).headOption.map(BundleError.DuplicatePath(_)).toLeft(())
      _ <- inputs
        .find(e => e.stored != sharing.includes(e.kind))
        .map(BundleError.InputNaming(_, sharing))
        .toLeft(())
      _ <- inputs.diff(inputs.distinct).headOption.map(BundleError.DuplicateInput(_)).toLeft(())
    yield new ProjectManifest(
      document,
      science,
      sharing,
      inputs.sortBy(_.sortKey),
      parts,
      CanonicalJson(presentation)
    )

  private given Encoder[DefinitionId] =
    Encoder.instance(id => Json.obj("name" -> id.name.asJson, "version" -> id.version.asJson))
  private given Decoder[DefinitionId] = Decoder.instance { c =>
    for
      name    <- c.get[String]("name")
      version <- c.get[Int]("version")
      id      <- DefinitionId
        .of(name, version)
        .left
        .map(e => DecodingFailure(s"invalid definition $name@$version: $e", c.history))
    yield id
  }

  /** Version 1, the pre-release layout: no sharing options and no recorded
    * science digest. It expresses exactly the manifests that carry
    * everything and record no digest.
    */
  private def writeV1(m: ProjectManifest): Json =
    Json.obj(
      "document"     -> m.document.asJson,
      "inputs"       -> m.inputs.asJson,
      "parts"        -> m.parts.asJson,
      "presentation" -> m.presentation
    )

  private def writeV2(m: ProjectManifest): Json =
    writeV1(m).deepMerge(Json.obj("science" -> m.science.asJson, "sharing" -> m.sharing.asJson))

  private def read(
      json: Json,
      sharing: HCursor => Decoder.Result[SharingOptions],
      science: HCursor => Decoder.Result[ScienceRecord]
  ): Either[CodecError, ProjectManifest] =
    val c = json.hcursor
    (for
      document     <- c.get[DefinitionId]("document")
      shared       <- sharing(c)
      digest       <- science(c)
      inputs       <- c.get[Vector[InputEntry]]("inputs")
      parts        <- c.get[DocumentParts]("parts")
      presentation <- c.get[Json]("presentation")
    yield (document, digest, shared, inputs, parts, presentation)).left
      .map(f => CodecError.Field("manifest", json, f.getMessage))
      .flatMap((d, s, sh, i, p, pr) =>
        of(d, s, sh, i, p, pr).left.map(e => CodecError.Field("manifest", json, e.message))
      )

  private def expressedByV1(m: ProjectManifest): Boolean =
    m.sharing == SharingOptions.complete && m.science == ScienceRecord.Unrecorded

  /** Every version of the manifest schema (CR3). Version 1 lifts to version
    * 2 by stating what it always meant: everything travels, and no science
    * digest was recorded (`Unrecorded`, never `Verified`). Version 2 requires
    * `science`: a missing or null record is refused.
    */
  val ladder: Either[CodecError, SchemaLadder[ProjectManifest]] =
    StudioSchemaIds.forCodec.map { ids =>
      SchemaLadder
        .of[ProjectManifest]("studio project manifest", ids.project)(m =>
          Right(CanonicalJson(writeV1(m)))
        )(json =>
          read(json, _ => Right(SharingOptions.complete), _ => Right(ScienceRecord.Unrecorded))
        )
        .next(
          expressedByV1,
          _.deepMerge(
            Json.obj(
              "sharing" -> SharingOptions.complete.asJson,
              "science" -> (ScienceRecord.Unrecorded: ScienceRecord).asJson
            )
          )
        )(m => Right(CanonicalJson(writeV2(m))))(json =>
          read(
            json,
            _.get[SharingOptions]("sharing"),
            _.get[ScienceRecord]("science")
          )
        )
    }

  val codec: Either[CodecError, VersionedCodec[ProjectManifest]] = ladder.map(_.codec)
