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

package eyes4s.studio.core.session

import cats.effect.{IO, Ref}
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{InputCheck, SourceBlock, SourceCheck, SourceState}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.DocumentSamples.t1
import eyes4s.studio.core.document.SourceRole

import java.nio.charset.StandardCharsets.UTF_8

/** Source repair against a session (ticket S2.5): a stored source whose bytes
  * changed at its address, or went missing, is found by its digest; it is
  * re-copied only with its exact bytes, and is then present again.
  */
class SourceRepairSessionSuite extends SessionConformance:

  def bundle: cats.effect.Resource[IO, IO[ProjectStore[IO]]] =
    cats.effect.Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  private val fixations = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,1,2\n")
  private val trials    = bytes("participant,phase,trial\nP01,Encoding,enc_01\n")
  private val document  = BundleSamples.withSources(withoutJobs(t1), fixations, trials)
  private val revision  = document.datasets.last

  private def source(role: SourceRole) = revision.sources.entries.find(_.role == role).get

  /** A store whose reads of the paths in `swaps` give other bytes, as a
    * file edited outside the studio would; a write clears the swap.
    */
  private final class Tampered(
      underlying: ProjectStore[IO],
      swaps: Ref[IO, Map[BundlePath, IArray[Byte]]]
  ) extends ProjectStore[IO]:
    def read(path: BundlePath) =
      swaps.get.flatMap(_.get(path).fold(underlying.read(path))(b => IO.pure(Right(b))))
    def list                                                           = underlying.list
    def write(lock: WriterLock, path: BundlePath, bytes: IArray[Byte]) =
      swaps.update(_ - path) *> underlying.write(lock, path, bytes)
    def delete(lock: WriterLock, path: BundlePath) = underlying.delete(lock, path)
    def readManifest                               = underlying.readManifest
    def swapManifest(lock: WriterLock, expected: Option[ByteDigest], next: IArray[Byte]) =
      underlying.swapManifest(lock, expected, next)
    def acquire(owner: LockOwner)  = underlying.acquire(owner)
    def release(lock: WriterLock)  = underlying.release(lock)
    def readSidecar(file: Sidecar) = underlying.readSidecar(file)
    def appendSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
      underlying.appendSidecar(lock, file, bytes)
    def replaceSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
      underlying.replaceSidecar(lock, file, bytes)
    def removeSidecar(lock: WriterLock, file: Sidecar) = underlying.removeSidecar(lock, file)

  private def states(session: ProjectSession[IO]): IO[Map[SourceRole, SourceState]] =
    session.checkInputs.map(s =>
      SourceCheck
        .of(document, InputCheck.Checked(s))
        .of(revision.id)
        .map(f => f.source.role -> f.state)
        .toMap
    )

  test(
    "a source changed at its address is found by its digest, and blocks; re-copied, it is present"
  ) {
    for
      swaps  <- Ref.of[IO, Map[BundlePath, IArray[Byte]]](Map.empty)
      memory <- InMemoryProjectStore.create[IO]
      store = Tampered(memory, swaps)
      session <- created(store, document)
      _       <- ok(
        session.importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations)
      )
      _ <- ok(session.importInput(InputKind.Source(SourceRole.Trials), "trials.csv", trials))
      clean <- states(session)
      edited = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,9,9\n")
      path   = ProjectBundle.inputPath(source(SourceRole.Fixations)).toOption.get
      _        <- swaps.set(Map(path -> edited))
      changed  <- states(session)
      blocking <- session.checkInputs.map(s =>
        SourceCheck.of(document, InputCheck.Checked(s)).blocking(revision.id).map(_.name)
      )
      // Other bytes are refused, naming both digests; the exact bytes restore it.
      wrong    <- session.restoreInput(source(SourceRole.Fixations), edited)
      _        <- ok(session.restoreInput(source(SourceRole.Fixations), fixations))
      restored <- states(session)
      _        <- ok(session.close)
    yield
      assertEquals(clean.values.toSet, Set(SourceState.Present))
      assertEquals(
        changed(SourceRole.Fixations),
        SourceState.Changed(ByteDigest.sha256(edited))
      )
      assertEquals(changed(SourceRole.Trials), SourceState.Present)
      assertEquals(blocking, Vector("fixations.csv"))
      wrong match
        case Left(SessionError.Bundle(op, BundleError.RestoreMismatch(entry, found))) =>
          assertEquals(op, "restore inputs/fixations.csv")
          assertEquals(
            (entry.sha256, found),
            (ByteDigest.sha256(fixations), ByteDigest.sha256(edited))
          )
        case other => fail(s"expected a refused restore, got $other")
      assertEquals(restored.values.toSet, Set(SourceState.Present))
  }

  test(
    "a source whose bytes are not stored is missing; restoring writes them; unlisted is missing too"
  ) {
    for
      memory  <- InMemoryProjectStore.create[IO]
      session <- created(memory, document)
      _       <- ok(
        session.importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations)
      )
      before <- states(session)
      _      <- ok(session.restoreInput(source(SourceRole.Trials), trials))
      after  <- states(session)
      _      <- ok(session.close)
    yield
      assertEquals(before(SourceRole.Trials), SourceState.Missing)
      assertEquals(after(SourceRole.Trials), SourceState.Present)
      // A source no listed input records is missing.
      assertEquals(
        SourceCheck
          .of(document, InputCheck.Checked(Vector.empty))
          .of(revision.id)
          .map(_.state)
          .distinct,
        Vector(SourceState.Missing)
      )
      // Fail closed: unchecked, or a failed check, blocks; only no project does not.
      assertEquals(
        SourceCheck.of(document, InputCheck.Unchecked).block(revision.id),
        Some(SourceBlock.Unchecked)
      )
      assertEquals(
        SourceCheck.of(document, InputCheck.CheckFailed("store offline")).block(revision.id),
        Some(SourceBlock.CheckFailed("store offline"))
      )
      assertEquals(SourceCheck.of(document, InputCheck.NoProject).block(revision.id), None)
  }

  test(
    "two revisions with a fixations.csv each: only the damaged one is changed, matched by digest"
  ) {
    import eyes4s.studio.core.command.{Command, Reducer}
    val other    = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,3,4\n")
    val fix      = source(SourceRole.Fixations)
    val replaced = fix.copy(bytes = ByteDigest.sha256(other), semantic = None)
    val sources  = eyes4s.studio.core.document.Sources
      .of(
        revision.sources.entries
          .map(s => if s.role == SourceRole.Fixations then replaced else s)
      )
      .fold(e => fail(e.message), identity)
    val next = Reducer
      .step(
        document,
        Command.ImportSources(
          Some(revision.id),
          sources,
          revision.mapping,
          revision.units,
          revision.geometry,
          revision.attributes,
          Some(revision.admission),
          revision.inventory
        )
      )
      .fold(e => fail(e.toString), _._1)
    val r4                                                     = next.datasets.last.id
    def entry(kind: InputKind, name: String, of: IArray[Byte]) =
      InputEntry
        .of(kind, name, ByteDigest.sha256(of), of.length.toLong)
        .fold(e => fail(e.message), identity)
    val fixKind = InputKind.Source(SourceRole.Fixations)
    // The newer revision's fixations.csv is listed first, and is the damaged one.
    val statuses = Vector(
      InputStatus
        .Changed(entry(fixKind, "fixations.csv", other), ByteDigest.sha256(bytes("x"))),
      InputStatus.Present(entry(fixKind, "fixations.csv", fixations)),
      InputStatus.Present(entry(InputKind.Source(SourceRole.Trials), "trials.csv", trials))
    )
    val check = SourceCheck.of(next, InputCheck.Checked(statuses))
    assertEquals(check.block(revision.id), None)
    assertEquals(
      check.block(r4).map {
        case SourceBlock.Damaged(fs) => fs.map(f => (f.dataset, f.name))
        case other                   => fail(s"$other")
      },
      Some(Vector((r4, "fixations.csv")))
    )
    // A withheld image is not counted as changed or missing; a missing one is, by digest.
    val img        = entry(InputKind.StimulusImage, "a.png", bytes("a"))
    val gone       = entry(InputKind.StimulusImage, "b.png", bytes("b"))
    val withImages = SourceCheck.of(
      next,
      InputCheck.Checked(
        statuses ++ Vector(InputStatus.Withheld(img.withheld), InputStatus.Missing(gone))
      )
    )
    assertEquals(
      withImages.imagesUnstored(Set(img.sha256, gone.sha256)),
      Vector(InputStatus.Missing(gone))
    )
    assertEquals(withImages.imagesUnstored(Set(img.sha256)), Vector.empty)
  }
