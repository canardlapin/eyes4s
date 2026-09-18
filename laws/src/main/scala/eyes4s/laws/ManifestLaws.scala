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

package eyes4s.laws

import cats.data.NonEmptyVector
import eyes4s.codec.*
import eyes4s.kernel.Unit2D
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import org.typelevel.discipline.Laws

/** What a manifest writer stores: the manifest and every entry's bytes by
  * name. The manifest's own bytes and address are its canonical encoding.
  */
final case class StoredGraph(
    manifest: ScientificManifest,
    entries: Map[ArtifactName, IArray[Byte]]
)

object StoredGraph:
  def of(saved: SavedManifest): StoredGraph =
    StoredGraph(saved.manifest, saved.artifacts.map(a => a.name -> a.bytes).toMap)

/** Published conformance of manifest writing and verified resolution, for
  * any scientific graph an application saves through its registered codecs.
  *
  * The rule set is parameterized by the writer, so a deliberately broken
  * writer (a dropped entry, an altered digest, a relation to the wrong
  * artifact) must falsify it; the decoders are the application's own
  * registrations, and the laws instrument them to observe decoding.
  */
trait ManifestLaws extends Laws:
  def verifiedResolution[G, K, U <: Unit2D](
      graphs: Gen[G],
      write: G => Either[CodecError, StoredGraph],
      decoders: ArtifactDecoders[K, U],
      reproduces: (G, ResolvedManifest[K, U]) => Boolean
  ): RuleSet =
    val stored: Gen[Either[CodecError, (StoredGraph, IArray[Byte])]] =
      graphs.map(g => write(g).flatMap(s => ScientificManifest.bytes(s.manifest).map(s -> _)))
    val corruptions = graphs.flatMap { g =>
      write(g).flatMap(s => ScientificManifest.bytes(s.manifest).map(s -> _)) match
        case Left(error)           => Gen.const(Left(error))
        case Right((graph, bytes)) =>
          // Index 0 is the manifest itself; entry i is at index i + 1.
          val blobs = (bytes +: graph.manifest.entries.map(e =>
            graph.entries.getOrElse(e.name, IArray.empty[Byte])
          )).zipWithIndex
            .filter(_._1.nonEmpty)
          for
            chosen <- Gen.oneOf(blobs)
            at     <- Gen.choose(0, chosen._1.length - 1)
            delta  <- Gen.choose(1, 255)
          yield Right((graph, bytes, chosen._2, at, delta))
    }
    new SimpleRuleSet(
      "verifiedManifest",
      "the manifest round-trips to its canonical bytes" -> forAll(stored) {
        case Right((graph, bytes)) =>
          ScientificManifest.codec
            .encode(graph.manifest)
            .flatMap(ScientificManifest.codec.decode) ==
            Right(graph.manifest) &&
            ScientificManifest.bytes(graph.manifest).map(_.toVector) == Right(bytes.toVector)
        case Left(_) => false
      },
      "a written graph resolves by address, reading each artifact once" -> forAll(graphs) { g =>
        write(g).flatMap(s => ScientificManifest.bytes(s.manifest).map(s -> _)) match
          case Right((graph, bytes)) =>
            val address = ByteDigest.sha256(bytes)
            var reads   = 0
            val source  = ByteSource { request =>
              reads += 1
              ManifestLaws.serve(graph, address, bytes)(request)
            }
            ArtifactResolver.resolve(address, source, decoders).exists(r => reproduces(g, r)) &&
            reads == graph.manifest.entries.size + 1
          case Left(_) => false
      },
      "any single corrupted byte is refused by its digest before anything is decoded" ->
        forAllNoShrink(corruptions) {
          case Right((graph, bytes, which, at, delta)) =>
            val address                                   = ByteDigest.sha256(bytes)
            def corrupt(blob: IArray[Byte]): IArray[Byte] =
              IArray.tabulate(blob.length)(i =>
                if i == at then (blob(i) ^ delta).toByte else blob(i)
              )
            val (manifestBytes, entries, expected) =
              if which == 0 then
                val changed = corrupt(bytes)
                (
                  changed,
                  graph.entries,
                  ResolveError.ManifestDigest(address, ByteDigest.sha256(changed))
                )
              else
                val entry   = graph.manifest.entries(which - 1)
                val changed = corrupt(graph.entries.getOrElse(entry.name, IArray.empty[Byte]))
                (
                  bytes,
                  graph.entries.updated(entry.name, changed),
                  ResolveError.Digest(entry.name, entry.sha256, ByteDigest.sha256(changed))
                )
            val counted = ManifestLaws.counting(decoders)
            val result  = ArtifactResolver.resolve(
              address,
              ByteSource.inMemory(Map(address -> manifestBytes), entries),
              counted
            )
            counted.calls == 0 && result == Left(NonEmptyVector.one(expected))
          case Left(_) => false
        }
    )

object ManifestLaws extends ManifestLaws:
  /** Serve a stored graph by manifest address and entry name. */
  def serve(graph: StoredGraph, address: ByteDigest, bytes: IArray[Byte])(
      request: ByteRequest
  ): Either[SourceFailure, IArray[Byte]] = request match
    case ByteRequest.Manifest(`address`) => Right(bytes)
    case ByteRequest.Manifest(_)         => Left(SourceFailure.Missing)
    case ByteRequest.Entry(entry)        =>
      graph.entries.get(entry.name).toRight(SourceFailure.Missing)

  /** Decoders that count their invocations and otherwise delegate. */
  final class Counting[K, U <: Unit2D](underlying: ArtifactDecoders[K, U])
      extends ArtifactDecoders[K, U]:
    private var count               = 0
    def calls: Int                  = count
    private def tick[A](a: => A): A =
      count += 1
      a
    def plan(document: Json)   = tick(underlying.plan(document))
    def input(document: Json)  = tick(underlying.input(document))
    def ledger(document: Json) = tick(underlying.ledger(document))
    def result(document: Json) = tick(underlying.result(document))
    def recording(document: Json, payloads: PayloadRef => Option[VerifiedPayload]) =
      tick(underlying.recording(document, payloads))
    def recordingInput(document: Json) = tick(underlying.recordingInput(document))
    def temporalInput(
        document: Json,
        base: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
    ) = tick(underlying.temporalInput(document, base))

  def counting[K, U <: Unit2D](decoders: ArtifactDecoders[K, U]): Counting[K, U] =
    new Counting(decoders)
