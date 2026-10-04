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

/** What a manifest writer stores: the manifest, the exact bytes it stored the
  * manifest as, and every entry's bytes by name. The address is the SHA-256
  * of the stored manifest bytes.
  */
final case class StoredGraph(
    manifest: ScientificManifest,
    manifestBytes: IArray[Byte],
    entries: Map[ArtifactName, IArray[Byte]]
):
  def address: ByteDigest = ByteDigest.sha256(manifestBytes)

object StoredGraph:
  def of(saved: SavedManifest): StoredGraph =
    StoredGraph(saved.manifest, saved.bytes, saved.artifacts.map(a => a.name -> a.bytes).toMap)

  /** A manifest stored in its canonical form beside these entries. */
  def canonical(
      manifest: ScientificManifest,
      entries: Map[ArtifactName, IArray[Byte]]
  ): Either[CodecError, StoredGraph] =
    ScientificManifest.bytes(manifest).map(StoredGraph(manifest, _, entries))

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
    val corruptions = graphs.flatMap { g =>
      write(g) match
        case Left(error)  => Gen.const(Left(error))
        case Right(graph) =>
          // Index 0 is the stored manifest itself; entry i is at index i + 1.
          val blobs = (graph.manifestBytes +: graph.manifest.entries.map(e =>
            graph.entries.getOrElse(e.name, IArray.empty[Byte])
          )).zipWithIndex
            .filter(_._1.nonEmpty)
          for
            chosen <- Gen.oneOf(blobs)
            at     <- Gen.choose(0, chosen._1.length - 1)
            delta  <- Gen.choose(1, 255)
          yield Right((graph, chosen._2, at, delta))
    }
    new SimpleRuleSet(
      "verifiedManifest",
      "the stored manifest bytes decode to the manifest and are its canonical form" ->
        forAll(graphs) { g =>
          write(g) match
            case Right(graph) =>
              ArtifactResolver
                .manifest(graph.address, ByteSource(_ => Right(graph.manifestBytes)))
                .contains(graph.manifest) &&
              ScientificManifest.bytes(graph.manifest).map(_.toVector) ==
                Right(graph.manifestBytes.toVector)
            case Left(_) => false
        },
      "a written graph resolves by address, reading each artifact once" -> forAll(graphs) { g =>
        write(g) match
          case Right(graph) =>
            var reads  = 0
            val source = ByteSource { request =>
              reads += 1
              ManifestLaws.serve(graph)(request)
            }
            ArtifactResolver
              .resolve(graph.address, source, decoders)
              .exists(r => reproduces(g, r)) &&
            reads == graph.manifest.entries.size + 1
          case Left(_) => false
      },
      "any single corrupted byte is refused by its digest before anything is decoded" ->
        forAllNoShrink(corruptions) {
          case Right((graph, which, at, delta)) =>
            val address                                   = graph.address
            def corrupt(blob: IArray[Byte]): IArray[Byte] =
              IArray.tabulate(blob.length)(i =>
                if i == at then (blob(i) ^ delta).toByte else blob(i)
              )
            val (manifestBytes, entries, expected) =
              if which == 0 then
                val changed = corrupt(graph.manifestBytes)
                (
                  changed,
                  graph.entries,
                  ResolveError.ManifestDigest(address, ByteDigest.sha256(changed))
                )
              else
                val entry   = graph.manifest.entries(which - 1)
                val changed = corrupt(graph.entries.getOrElse(entry.name, IArray.empty[Byte]))
                (
                  graph.manifestBytes,
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
  /** Serve a stored graph: the manifest under its address, entries by name. */
  def serve(graph: StoredGraph)(request: ByteRequest): Either[SourceFailure, IArray[Byte]] =
    request match
      case ByteRequest.Manifest(address) if address == graph.address =>
        Right(graph.manifestBytes)
      case ByteRequest.Manifest(_)  => Left(SourceFailure.Missing)
      case ByteRequest.Entry(entry) =>
        graph.entries.get(entry.name).toRight(SourceFailure.Missing)

  /** Decoders that count their invocations and otherwise delegate. */
  final class Counting[K, U <: Unit2D](underlying: ArtifactDecoders[K, U])
      extends ArtifactDecoders.Delegating[K, U](underlying):
    private var count               = 0
    def calls: Int                  = count
    private def tick[A](a: => A): A =
      count += 1
      a
    override def plan(document: Json)   = tick(super.plan(document))
    override def input(document: Json)  = tick(super.input(document))
    override def ledger(document: Json) = tick(super.ledger(document))
    override def result(document: Json) = tick(super.result(document))
    override def recording(document: Json, payloads: PayloadRef => Option[VerifiedPayload]) =
      tick(super.recording(document, payloads))
    override def recordingInput(document: Json) = tick(super.recordingInput(document))
    override def temporalInput(
        document: Json,
        base: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
    )                                            = tick(super.temporalInput(document, base))
    override def recordingPlan(document: Json)   = tick(super.recordingPlan(document))
    override def recordingResult(document: Json) = tick(super.recordingResult(document))
    override def temporalPlan(document: Json)    = tick(super.temporalPlan(document))
    override def temporalResult(document: Json)  = tick(super.temporalResult(document))
    override def report(document: Json)          = tick(super.report(document))
    override def importSpec(document: Json)      = tick(super.importSpec(document))

    /** The analysis registrations, each decoder counted. */
    override def analyses =
      AnalysisRegistry
        .of(
          super.analyses.entries.map(r =>
            r.copy(
              decodePlan = json => tick(r.decodePlan(json)),
              decodeResult = json => tick(r.decodeResult(json))
            )
          )
        )
        .getOrElse(super.analyses)

  def counting[K, U <: Unit2D](decoders: ArtifactDecoders[K, U]): Counting[K, U] =
    new Counting(decoders)
