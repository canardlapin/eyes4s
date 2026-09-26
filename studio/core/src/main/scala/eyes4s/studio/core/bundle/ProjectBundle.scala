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

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest, CodecError}
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.document.{JobHandle, ScienceContent, SourceRole, StudioDocument}
import io.circe.{Json, JsonObject}

import java.nio.charset.StandardCharsets.UTF_8

/** Why a bundle could not be read, written or shared. Every case names what
  * failed; `message` is a default English rendering, never an identity.
  */
enum BundleError derives CanEqual:
  case BadPath(path: String, problem: PathProblem)
  case BlankLockOwner
  case NegativeLength(path: String, length: Long)
  case PartOutsideArea(part: String, path: BundlePath, expected: BundleArea)
  case DuplicatePath(path: BundlePath)

  /** A document source whose bytes the bundle does not list as an input. */
  case UnlistedSource(dataset: DatasetRevision, role: SourceRole, sha256: ByteDigest)
  case Store(error: StoreError)
  case Codec(target: String, error: CodecError)
  case NotUtf8(target: String)
  case MalformedPart(path: BundlePath, reason: String)
  case PartLength(path: BundlePath, expected: Long, found: Long)
  case PartDigest(path: BundlePath, expected: ByteDigest, found: ByteDigest)

  /** An immutable entry already exists with other bytes. */
  case Collision(path: BundlePath, existing: ByteDigest, incoming: ByteDigest)

  /** The parts' science is not the science the manifest recorded. */
  case ScienceMismatch(
      recorded: CanonicalDigest[ScienceContent],
      found: CanonicalDigest[ScienceContent]
  )

  /** Sharing asked to include an input the source bundle withheld. */
  case WithheldInSource(entry: InputEntry)
  case InputUnavailable(entry: InputEntry, error: StoreError)
  case InputChanged(entry: InputEntry, found: ByteDigest)

  def message: String = this match
    case BadPath(path, problem)       => s"Bundle path '$path' is refused: ${problem.describe}."
    case BlankLockOwner               => "A lock owner needs a description."
    case NegativeLength(path, length) => s"$path declares a negative length, $length."
    case PartOutsideArea(part, path, a) =>
      s"The $part part $path lies outside the ${a.directory}/ area."
    case DuplicatePath(path)         => s"The manifest lists $path more than once."
    case UnlistedSource(ds, role, d) =>
      s"Dataset ${ds.label}'s ${role.label} source (${d.hex}) is not among the bundle's inputs."
    case Store(error)                => error.message
    case Codec(target, error)        => s"$target: ${error.message}"
    case NotUtf8(target)             => s"$target is not strict UTF-8."
    case MalformedPart(path, reason) => s"Part $path is malformed: $reason."
    case PartLength(path, e, f)      => s"Part $path has $f bytes; the manifest lists $e."
    case PartDigest(path, e, f)      =>
      s"Part $path has SHA-256 ${f.hex}; the manifest lists ${e.hex}."
    case Collision(path, existing, incoming) =>
      s"$path already holds ${existing.hex}; refusing to replace it with ${incoming.hex}."
    case ScienceMismatch(recorded, found) =>
      s"The parts' science is ${found.display}; the manifest recorded ${recorded.display}."
    case WithheldInSource(entry) =>
      s"Input ${entry.path} was withheld from the source bundle, so it cannot be included."
    case InputUnavailable(entry, error) => s"Input ${entry.path}: ${error.message}"
    case InputChanged(entry, found)     =>
      s"Input ${entry.path} has SHA-256 ${found.hex}; the manifest lists ${entry.sha256.hex}."

/** A document written as bundle files: the manifest, its exact bytes and
  * every part's bytes. `sessionOnly` returns, as data, the job handles that
  * were not written: a backend job belongs to the session that started it.
  */
final case class EncodedBundle(
    manifest: ProjectManifest,
    manifestBytes: IArray[Byte],
    parts: Vector[(BundlePath, IArray[Byte])],
    sessionOnly: Vector[JobHandle]
)

/** An opened bundle: its document (with no job handles), its manifest and
  * the digest of the manifest's bytes, which the next save swaps against.
  */
final case class OpenedProject(
    document: StudioDocument,
    manifest: ProjectManifest,
    manifestDigest: ByteDigest
) derives CanEqual

/** One listed input as the store holds it. */
enum InputStatus derives CanEqual:
  case Present(entry: InputEntry)
  case Withheld(entry: InputEntry)
  case Missing(entry: InputEntry)
  case Changed(entry: InputEntry, found: ByteDigest)
  case Unreadable(entry: InputEntry, error: StoreError)

/** The `.eyes` project bundle (ticket S2.3): a directory holding a
  * `project.json` manifest ([[ProjectManifest]]) and the areas of
  * [[BundleArea]], every path relative to the bundle root.
  *
  * A document is stored as parts, one per dataset revision (its column
  * mapping apart, under `mappings/`), analysis revision, draft, run (under
  * `runs/<id>/`), reporting spec and figure. Each part is the canonical JSON
  * of that fragment of the `studio.document` schema, and its name ends in the
  * first 16 hex digits of its SHA-256: a changed value is a new file, so
  * parts are immutable and a save only adds files before it swaps the
  * manifest (S2.4a).
  *
  * Where eyes4s's single `manifest@2` slots in: its roles are artifacts in
  * this layout, not a second project format. `SourceFile` entries are the
  * `inputs/<sha256>/` copies (the same `ByteDigest`), `ImportSpec` entries go
  * under `mappings/`, the admission ledger's `LedgerSource` beside its dataset
  * revision, a run's saved graph (the eyes4s manifest and its entries by
  * artifact name) under `runs/<id>/`, and the report roles under `reporting/`
  * and `figures/`. The document already binds them by CR3 digest
  * (`CoreBinding`); a part list for them is added by a later version of this
  * manifest through its `SchemaLadder`.
  */
object ProjectBundle:

  // -------------------------------------------------------------------------
  // Pure encoding and decoding
  // -------------------------------------------------------------------------

  /** Write `document` as bundle files. Every source of every dataset
    * revision must be among `inputs`; its bytes need not be in the store yet.
    */
  def encode(
      document: StudioDocument,
      sharing: SharingOptions,
      inputs: Vector[InputEntry]
  ): Either[BundleError, EncodedBundle] =
    for
      _        <- sourcesListed(document, inputs)
      envelope <- StudioDocument.encode(document).left.map(BundleError.Codec("the document", _))
      schema   <- definition(envelope)
      value    <- envelope.hcursor
        .get[JsonObject]("value")
        .left
        .map(f =>
          BundleError.Codec("the document", CodecError.Field("value", envelope, f.message))
        )
      datasetJson  <- array(value, "datasets")
      analysisJson <- array(value, "analyses")
      runJson      <- array(value, "runs")
      reportJson   <- array(value, "reporting")
      figureJson   <- array(value, "figures")
      datasets     <- document.datasets.zip(datasetJson).traverse { (d, json) =>
        val obj = json.asObject.getOrElse(JsonObject.empty)
        for
          dataset <- part(
            BundleArea.Datasets,
            s"r${d.id.number}",
            Json.fromJsonObject(obj.remove("mapping"))
          )
          mapping <- part(
            BundleArea.Mappings,
            s"r${d.id.number}",
            obj("mapping").getOrElse(Json.Null)
          )
        yield (dataset, mapping)
      }
      analyses <- document.analyses
        .zip(analysisJson)
        .traverse((a, json) => part(BundleArea.Analyses, s"rev${a.id.number}", json))
      draft <- document.draft.traverse(d =>
        part(
          BundleArea.Analyses,
          s"draft-rev${d.id.number}",
          value("draft").getOrElse(Json.Null)
        )
      )
      runs <- document.runs
        .zip(runJson)
        .traverse((r, json) => part(BundleArea.Runs, s"${r.id.number}/run", json))
      reporting <- reportJson.traverse(json => part(BundleArea.Reporting, "spec", json))
      figures   <- document.figures
        .zip(figureJson)
        .traverse((f, json) => part(BundleArea.Figures, s"figure${f.id.number}", json))
      science <- StudioDocument
        .scienceDigest(document)
        .left
        .map(BundleError.Codec("science", _))
      manifest <- ProjectManifest.of(
        schema,
        Some(science),
        sharing,
        inputs,
        DocumentParts(
          datasets.map((d, m) => DatasetParts(d._1, m._1)),
          analyses.map(_._1),
          draft.map(_._1),
          runs.map(_._1),
          reporting.map(_._1),
          figures.map(_._1)
        ),
        value("presentation").getOrElse(Json.Null)
      )
      manifestBytes <- manifestBytes(manifest)
      files = (datasets.flatMap((d, m) => Vector(d, m)) ++ analyses ++ draft ++ runs ++
        reporting ++ figures).map((entry, bytes) => entry.path -> bytes)
    yield EncodedBundle(manifest, manifestBytes, files, document.jobs)

  /** The exact bytes of `project.json`: the UTF-8 of its compact canonical
    * envelope (members in key order) and a final newline.
    */
  def manifestBytes(manifest: ProjectManifest): Either[BundleError, IArray[Byte]] =
    ProjectManifest.codec
      .flatMap(_.encode(manifest))
      .left
      .map(BundleError.Codec(ProjectStore.ManifestName, _))
      .flatMap(json => utf8(ProjectStore.ManifestName, PortableJson.print(json) + "\n"))

  /** Read `project.json` bytes of any listed manifest version. */
  def readManifest(bytes: IArray[Byte]): Either[BundleError, ProjectManifest] =
    for
      json  <- parseJson(ProjectStore.ManifestName, bytes)
      codec <- ProjectManifest.codec.left.map(BundleError.Codec(ProjectStore.ManifestName, _))
      manifest <- codec.decode(json).left.map(BundleError.Codec(ProjectStore.ManifestName, _))
    yield manifest

  /** Rebuild the document from its manifest and its parts' bytes: each part's
    * length and digest are checked, the document is read with the reader of
    * the version its manifest names, and its science must be the recorded
    * science. The document has no job handles.
    */
  def assemble(
      manifest: ProjectManifest,
      part: BundlePath => Either[BundleError, IArray[Byte]]
  ): Either[BundleError, StudioDocument] =
    def load(entry: PartEntry): Either[BundleError, Json] =
      for
        bytes <- part(entry.path)
        _     <- Either.cond(
          bytes.length.toLong == entry.length,
          (),
          BundleError.PartLength(entry.path, entry.length, bytes.length.toLong)
        )
        found = ByteDigest.sha256(bytes)
        _ <- Either.cond(
          found == entry.sha256,
          (),
          BundleError.PartDigest(entry.path, entry.sha256, found)
        )
        json <- parseJson(entry.path.value, bytes)
      yield json
    val parts = manifest.parts
    for
      datasets <- parts.datasets.traverse { d =>
        (load(d.dataset), load(d.mapping)).tupled.flatMap((dataset, mapping) =>
          dataset.asObject
            .toRight(
              BundleError.MalformedPart(d.dataset.path, "a dataset part is a JSON object")
            )
            .map(obj => Json.fromJsonObject(obj.add("mapping", mapping)))
        )
      }
      analyses  <- parts.analyses.traverse(load)
      draft     <- parts.draft.traverse(load)
      runs      <- parts.runs.traverse(load)
      reporting <- parts.reporting.traverse(load)
      figures   <- parts.figures.traverse(load)
      value = Json.obj(
        "datasets"     -> Json.fromValues(datasets),
        "analyses"     -> Json.fromValues(analyses),
        "draft"        -> draft.getOrElse(Json.Null),
        "runs"         -> Json.fromValues(runs),
        "reporting"    -> Json.fromValues(reporting),
        "figures"      -> Json.fromValues(figures),
        "presentation" -> manifest.presentation,
        "jobs"         -> Json.arr()
      )
      document <- StudioDocument.ladder
        .flatMap(_.readAt(manifest.document, value))
        .left
        .map(BundleError.Codec("the document", _))
      _ <- manifest.science.traverse_ { recorded =>
        StudioDocument
          .scienceDigest(document)
          .left
          .map(BundleError.Codec("science", _))
          .flatMap(found =>
            Either.cond(found == recorded, (), BundleError.ScienceMismatch(recorded, found))
          )
      }
    yield document

  // -------------------------------------------------------------------------
  // Through a store
  // -------------------------------------------------------------------------

  /** Open the bundle in `store`. Reads only the manifest and the parts it
    * lists; `cache/` and unlisted files are never read.
    */
  def open[F[_]: Monad](store: ProjectStore[F]): F[Either[BundleError, OpenedProject]] =
    (for
      bytes    <- EitherT(store.readManifest).leftMap(BundleError.Store(_))
      manifest <- EitherT.fromEither[F](readManifest(bytes))
      read     <- manifest.parts.all.traverse(entry =>
        EitherT(store.read(entry.path)).leftMap(BundleError.Store(_)).map(entry.path -> _)
      )
      document <- EitherT.fromEither[F](
        assemble(
          manifest,
          path =>
            read
              .find(_._1 == path)
              .map(_._2)
              .toRight(BundleError.Store(StoreError.Missing(path)))
        )
      )
    yield OpenedProject(document, manifest, ByteDigest.sha256(bytes))).value

  /** Write `encoded` to `store`: every part not already there, then the
    * manifest by compare-and-swap against `previous` (the digest of the
    * manifest this save replaces; `None` for a new bundle). An existing part
    * with other bytes is refused, never replaced. Returns the new manifest's
    * digest.
    */
  def save[F[_]: Monad](
      store: ProjectStore[F],
      lock: WriterLock,
      previous: Option[ByteDigest],
      encoded: EncodedBundle
  ): F[Either[BundleError, ByteDigest]] =
    (for
      _ <- encoded.parts.traverse_((path, bytes) =>
        EitherT(writeImmutable(store, lock, path, bytes))
      )
      _ <- EitherT(store.swapManifest(lock, previous, encoded.manifestBytes))
        .leftMap(BundleError.Store(_))
    yield ByteDigest.sha256(encoded.manifestBytes)).value

  /** Copy an input's bytes into `inputs/<sha256>/<name>` and return its
    * entry. Importing the same bytes again writes nothing.
    */
  def importInput[F[_]: Monad](
      store: ProjectStore[F],
      lock: WriterLock,
      kind: InputKind,
      name: String,
      bytes: IArray[Byte]
  ): F[Either[BundleError, InputEntry]] =
    (for
      entry <- EitherT.fromEither[F](
        InputEntry.of(kind, name, ByteDigest.sha256(bytes), bytes.length.toLong)
      )
      _ <- EitherT(writeImmutable(store, lock, entry.path, bytes))
    yield entry).value

  /** The state of every input the manifest lists. */
  def checkInputs[F[_]: Monad](
      store: ProjectStore[F],
      manifest: ProjectManifest
  ): F[Vector[InputStatus]] =
    manifest.inputs.traverse { entry =>
      if !manifest.sharing.includes(entry.kind) then Monad[F].pure(InputStatus.Withheld(entry))
      else
        store.read(entry.path).map {
          case Left(StoreError.Missing(_)) => InputStatus.Missing(entry)
          case Left(error)                 => InputStatus.Unreadable(entry, error)
          case Right(bytes)                =>
            val found = ByteDigest.sha256(bytes)
            if found == entry.sha256 then InputStatus.Present(entry)
            else InputStatus.Changed(entry, found)
        }
    }

  /** Write the bundle in `from` as a new bundle in `to` with `sharing`: the
    * same science, the inputs `sharing` includes, and the others listed as
    * withheld. An input cannot be included if `from` withheld it.
    */
  def share[F[_]: Monad](
      from: ProjectStore[F],
      to: ProjectStore[F],
      lock: WriterLock,
      sharing: SharingOptions
  ): F[Either[BundleError, ByteDigest]] =
    (for
      opened <- EitherT(open(from))
      inputs = opened.manifest.inputs
      _ <- EitherT.fromEither[F](
        inputs
          .find(e => sharing.includes(e.kind) && !opened.manifest.sharing.includes(e.kind))
          .map(BundleError.WithheldInSource(_))
          .toLeft(())
      )
      encoded <- EitherT.fromEither[F](encode(opened.document, sharing, inputs))
      _       <- inputs.filter(e => sharing.includes(e.kind)).traverse_ { entry =>
        for
          bytes <- EitherT(from.read(entry.path)).leftMap(
            BundleError.InputUnavailable(entry, _)
          )
          found = ByteDigest.sha256(bytes)
          _ <- EitherT.cond[F](
            found == entry.sha256,
            (),
            BundleError.InputChanged(entry, found)
          )
          _ <- EitherT(writeImmutable(to, lock, entry.path, bytes))
        yield ()
      }
      digest <- EitherT(save(to, lock, None, encoded))
    yield digest).value

  // -------------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------------

  private def writeImmutable[F[_]: Monad](
      store: ProjectStore[F],
      lock: WriterLock,
      path: BundlePath,
      bytes: IArray[Byte]
  ): F[Either[BundleError, Unit]] =
    store.read(path).flatMap {
      case Right(existing) =>
        val (was, now) = (ByteDigest.sha256(existing), ByteDigest.sha256(bytes))
        Monad[F].pure(Either.cond(was == now, (), BundleError.Collision(path, was, now)))
      case Left(StoreError.Missing(_)) =>
        store.write(lock, path, bytes).map(_.leftMap(BundleError.Store(_)))
      case Left(error) => Monad[F].pure(Left(BundleError.Store(error)))
    }

  private def sourcesListed(
      document: StudioDocument,
      inputs: Vector[InputEntry]
  ): Either[BundleError, Unit] =
    document.datasets.traverse_ { d =>
      d.sources.entries.traverse_ { s =>
        Either.cond(
          inputs.exists(i => i.kind == InputKind.Source(s.role) && i.sha256 == s.bytes),
          (),
          BundleError.UnlistedSource(d.id, s.role, s.bytes)
        )
      }
    }

  /** A part's file: `<area>/<stem>.<first 16 hex digits of its SHA-256>.json`. */
  private def part(
      area: BundleArea,
      stem: String,
      json: Json
  ): Either[BundleError, (PartEntry, IArray[Byte])] =
    for
      bytes <- utf8(s"${area.directory}/$stem", PortableJson.print(json))
      path  <- BundlePath.in(area, s"$stem.${ByteDigest.sha256(bytes).hex.take(16)}.json")
    yield (PartEntry.of(path, bytes), bytes)

  private def array(value: JsonObject, field: String): Either[BundleError, Vector[Json]] =
    value(field)
      .flatMap(_.asArray)
      .toRight(
        BundleError.Codec(
          "the document",
          CodecError.Field(field, Json.fromJsonObject(value), "expected an array")
        )
      )

  private def definition(envelope: Json): Either[BundleError, DefinitionId] =
    val c = envelope.hcursor.downField("schema")
    (c.get[String]("name"), c.get[Int]("version")).tupled.left
      .map(f => CodecError.Field("schema", envelope, f.message))
      .flatMap((name, version) =>
        DefinitionId.of(name, version).left.map(CodecError.Definition(_))
      )
      .left
      .map(BundleError.Codec("the document", _))

  /** Strict UTF-8: the text must survive the round trip, so a lone surrogate
    * or a malformed byte sequence is refused rather than replaced.
    */
  private def utf8(target: String, text: String): Either[BundleError, IArray[Byte]] =
    val bytes = text.getBytes(UTF_8)
    Either.cond(
      String(bytes, UTF_8) == text,
      IArray.unsafeFromArray(bytes),
      BundleError.NotUtf8(target)
    )

  private def parseJson(target: String, bytes: IArray[Byte]): Either[BundleError, Json] =
    val array = Array.from(bytes)
    val text  = String(array, UTF_8)
    for
      _ <- Either.cond(
        java.util.Arrays.equals(text.getBytes(UTF_8), array),
        (),
        BundleError.NotUtf8(target)
      )
      json <- io.circe.parser
        .parse(text)
        .left
        .map(e => BundleError.Codec(target, CodecError.InvalidJson(text.take(256), e.message)))
    yield json
