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

package eyes4s.studio.core.runs

import eyes4s.codec.{ArtifactName, ByteDigest, CodecError, VersionedCodec}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.bundle.{
  BundleArea,
  BundleError,
  BundlePath,
  PortableJson,
  ProjectBundle
}
import eyes4s.studio.core.document.{
  CanonicalJson,
  CoreBinding,
  ResultArchiveArtifact,
  RunLifecycle,
  RunRef,
  StudioSchemaIds
}
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

/** One file of a run's result archive: its eyes4s artifact name and the
  * exact length and SHA-256 of its bytes. Studio never reads inside it.
  */
final case class ArchiveEntry private (name: ArtifactName, length: Long, sha256: ByteDigest)
    derives CanEqual

object ArchiveEntry:
  def of(
      name: ArtifactName,
      length: Long,
      sha256: ByteDigest
  ): Either[RunStoreError, ArchiveEntry] =
    Either.cond(
      length >= 0L,
      new ArchiveEntry(name, length, sha256),
      RunStoreError.NegativeLength(name, length)
    )

  /** The entry describing `bytes`. */
  def describe(name: ArtifactName, bytes: IArray[Byte]): ArchiveEntry =
    new ArchiveEntry(name, bytes.length.toLong, ByteDigest.sha256(bytes))

/** The index of one run's stored result archive (S2.6): the run, the eyes4s
  * archive binding its [[RunRef]] carries, and its entries, at least one,
  * with distinct names, in name order.
  *
  * The index is what the run store reads to show an archive and its size;
  * an entry's bytes are read only when asked for.
  */
final case class ArchiveIndex private (
    run: RunId,
    archive: CoreBinding[ResultArchiveArtifact],
    entries: Vector[ArchiveEntry]
) derives CanEqual:
  def entry(name: ArtifactName): Option[ArchiveEntry] = entries.find(_.name == name)
  def names: Vector[ArtifactName]                     = entries.map(_.name)

  /** The bytes the entries occupy in the store: entries with the same
    * digest share one stored file, so each digest counts once.
    */
  def contentBytes: Long = entries.distinctBy(_.sha256.hex).map(_.length).sum

object ArchiveIndex:
  def of(
      run: RunId,
      archive: CoreBinding[ResultArchiveArtifact],
      entries: Vector[ArchiveEntry]
  ): Either[RunStoreError, ArchiveIndex] =
    val names    = entries.map(_.name)
    val repeated = names.diff(names.distinct).distinct
    if entries.isEmpty then Left(RunStoreError.EmptyArchive(run))
    else if repeated.nonEmpty then Left(RunStoreError.DuplicateEntries(run, repeated))
    else Right(new ArchiveIndex(run, archive, entries.sortBy(_.name.value)))

  private given Encoder[ArtifactName] = Encoder[String].contramap(_.value)
  private given Decoder[ArtifactName] =
    Decoder[String].emap(s => ArtifactName.of(s).left.map(_.message))
  private given Encoder[ByteDigest] = Encoder[String].contramap(_.hex)
  private given Decoder[ByteDigest] =
    Decoder[String].emap(s => ByteDigest.parse(s).left.map(_.message))

  private given Encoder.AsObject[ArchiveEntry] =
    Encoder.forProduct3("name", "length", "sha256")(e => (e.name, e.length, e.sha256))
  private given Decoder[ArchiveEntry] = Decoder.instance { (c: HCursor) =>
    for
      name   <- c.get[ArtifactName]("name")
      length <- c.get[Long]("length")
      sha256 <- c.get[ByteDigest]("sha256")
      entry  <- ArchiveEntry
        .of(name, length, sha256)
        .left
        .map(e => DecodingFailure(e.message, c.history))
    yield entry
  }

  given Encoder.AsObject[ArchiveIndex] =
    Encoder.forProduct3("run", "archive", "entries")(i => (i.run, i.archive, i.entries))
  given Decoder[ArchiveIndex] = Decoder.instance { (c: HCursor) =>
    for
      run     <- c.get[RunId]("run")
      archive <- c.get[CoreBinding[ResultArchiveArtifact]]("archive")
      entries <- c.get[Vector[ArchiveEntry]]("entries")
      index   <- of(run, archive, entries).left.map(e => DecodingFailure(e.message, c.history))
    yield index
  }

  /** The versioned canonical codec, schema `studio.run-archive@1`. Reading
    * validates exactly as [[of]] does.
    */
  val codec: Either[CodecError, VersionedCodec[ArchiveIndex]] =
    StudioSchemaIds.forCodec.map { ids =>
      VersionedCodec.checked[ArchiveIndex](ids.runArchive)(i => Right(CanonicalJson(i.asJson)))(
        json =>
          json
            .as[ArchiveIndex]
            .left
            .map(f => CodecError.Field("run archive", json, f.getMessage))
      )
    }

  def encode(index: ArchiveIndex): Either[CodecError, Json] = codec.flatMap(_.encode(index))
  def decode(json: Json): Either[CodecError, ArchiveIndex]  = codec.flatMap(_.decode(json))

/** A completed run's result archive, as eyes4s produced it: its index and
  * the bytes of every entry. Built from bytes ([[RunArchive.of]]) to be
  * stored, or read back whole by [[RunStore.load]].
  */
final class RunArchive private (val index: ArchiveIndex, contents: Map[String, IArray[Byte]]):
  def run: RunId = index.run

  /** The bytes of the entry `name`. */
  def bytes(name: ArtifactName): Option[IArray[Byte]] = contents.get(name.value)

  /** Every entry with its bytes, in name order. */
  def files: Vector[(ArchiveEntry, IArray[Byte])] =
    index.entries.map(e => e -> contents(e.name.value))

  override def toString: String =
    s"RunArchive(${index.run.label}, ${index.names.mkString(", ")})"

object RunArchive:
  /** The archive of `run`, which must have completed, holding `files`: at
    * least one, with distinct names.
    */
  def of(
      run: RunRef,
      files: Vector[(ArtifactName, IArray[Byte])]
  ): Either[RunStoreError, RunArchive] =
    for
      _ <- Either.cond(
        run.state == RunLifecycle.Completed,
        (),
        RunStoreError.NotCompleted(run.id, run.state)
      )
      index <- ArchiveIndex.of(
        run.id,
        run.archive,
        files.map((name, bytes) => ArchiveEntry.describe(name, bytes))
      )
    yield new RunArchive(index, files.map((n, b) => n.value -> b).toMap)

  /** An archive read back from a store, its entries already checked. */
  private[runs] def stored(
      index: ArchiveIndex,
      contents: Map[String, IArray[Byte]]
  ): RunArchive =
    new RunArchive(index, contents)

/** Where the run store keeps an archive inside a bundle: the area
  * `runs/<id>/archive/`, beside the run's document part `runs/<id>/run.*.json`.
  * The index is `index.json`; each entry's bytes are a file named by the full
  * hex SHA-256 of those bytes, so a stored file is never rewritten. The
  * manifest lists none of them: opening a project never reads an archive.
  */
object ArchivePaths:
  val IndexName: String = "index.json"

  def directory(run: RunId): String = s"${run.number}/archive"

  def index(run: RunId): Either[BundleError, BundlePath] =
    BundlePath.in(BundleArea.Runs, s"${directory(run)}/$IndexName")

  def content(run: RunId, sha256: ByteDigest): Either[BundleError, BundlePath] =
    BundlePath.in(BundleArea.Runs, s"${directory(run)}/${sha256.hex}")

  /** The run whose archive area holds `path`, if it lies in one. */
  def owner(path: BundlePath): Option[RunId] = path.segments match
    case Vector("runs", n, "archive", _) =>
      n.toIntOption.map(RunId(_)).filter(r => directory(r) == s"$n/archive")
    case _ => None

  def isIndex(path: BundlePath): Boolean = owner(path).isDefined && path.name == IndexName

private[runs] object ArchiveText:
  /** The index file's bytes. */
  def render(index: ArchiveIndex, target: String): Either[RunStoreError, IArray[Byte]] =
    ArchiveIndex
      .encode(index)
      .left
      .map(RunStoreError.Index(target, _))
      .flatMap(json =>
        PortableJson
          .print(target, json)
          .flatMap(ProjectBundle.utf8(target, _))
          .left
          .map(RunStoreError.Bundle(_))
      )

  /** The index in an index file's bytes. */
  def read(path: BundlePath, bytes: IArray[Byte]): Either[RunStoreError, ArchiveIndex] =
    ProjectBundle
      .parseJson(path.value, bytes)
      .left
      .map(RunStoreError.Bundle(_))
      .flatMap(json => ArchiveIndex.decode(json).left.map(RunStoreError.Index(path.value, _)))
