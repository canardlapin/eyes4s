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

package eyes4s.studio.core.fixture

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{AssetFile, AssetRef, AssetRegistry}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{SourceRole, StudioDocument}

/** One file the example is written from: its name and exact bytes. */
final case class ExampleFile(name: String, bytes: IArray[Byte])

/** The files of fixtures/studio-golden the example is written from. */
final case class ExampleSources(
    fixations: ExampleFile,
    trials: ExampleFile,
    stimuli: Vector[ExampleFile]
)

/** The bundled example opened as a new project: its name and the copy. */
final case class ExampleCopy(name: String, project: OpenedProject) derives CanEqual

/** The bundled example study, fixtures/studio-bundles/example.eyes (ticket
  * S0.8): the golden fixture's study at story moment t2 (data r3 admitted,
  * rev 4 · run 7 current, draft rev 5 ready, Figures 1 and 2), with its
  * fixation and trial sources and every stimulus image stored in the bundle.
  *
  * The example is never opened in place: [[openCopy]] writes it into a new,
  * empty bundle named [[copyName]], "memory-study (copy)".
  */
object ExampleProject:

  /** The example study's name. */
  val name: String = "memory-study"

  /** The name of a copy of the project `of`. */
  def copyNameOf(of: String): String = s"$of (copy)"

  /** "memory-study (copy)". */
  val copyName: String = copyNameOf(name)

  /** The example's document: story moment t2. */
  def document: Either[String, StudioDocument] = StoryMoments.t2

  /** Write `document` as a new bundle into the empty `store`: its sources
    * and stimulus images as stored inputs, then its parts and manifest. Every
    * byte is determined by `document` and `sources`, so writing twice gives
    * the same bundle.
    */
  def write[F[_]: Monad](
      store: ProjectStore[F],
      lock: WriterLock,
      document: StudioDocument,
      sources: ExampleSources
  ): F[Either[BundleError, ByteDigest]] =
    val files =
      Vector(
        InputKind.Source(SourceRole.Fixations) -> sources.fixations,
        InputKind.Source(SourceRole.Trials)    -> sources.trials
      ) ++ sources.stimuli.sortBy(_.name).map(InputKind.StimulusImage -> _)
    (for
      inputs <- files.traverse((kind, file) =>
        EitherT(ProjectBundle.importInput(store, lock, kind, file.name, file.bytes))
      )
      encoded <- EitherT.fromEither[F](
        ProjectBundle.encode(document, SharingOptions.complete, inputs)
      )
      digest <- EitherT(ProjectBundle.save(store, lock, None, encoded))
    yield digest).value

  /** Open the example as a new project: copy the bundle in `example`, with
    * every input, into the empty `target` and open the copy, named
    * [[copyName]]. The example itself is only read.
    */
  def openCopy[F[_]: Monad](
      example: ProjectStore[F],
      target: ProjectStore[F],
      lock: WriterLock
  ): F[Either[BundleError, ExampleCopy]] =
    (for
      _      <- EitherT(ProjectBundle.share(example, target, lock, SharingOptions.complete))
      opened <- EitherT(ProjectBundle.open(target))
    yield ExampleCopy(copyName, opened)).value

  /** The stimulus images a manifest stores, as assets. */
  def storedStimuli(manifest: ProjectManifest): Either[String, Vector[AssetRef]] =
    manifest.inputs
      .filter(e => e.kind == InputKind.StimulusImage)
      .flatMap(e => e.name.map(_ -> e.sha256))
      .traverse((n, digest) => AssetFile.of(n).bimap(_.message, AssetRef(_, digest)))

  /** The asset registry of an opened golden project's latest dataset
    * revision, from the stimulus images its bundle stores.
    */
  def registry(project: OpenedProject): Either[String, AssetRegistry] =
    for
      dataset  <- project.document.datasets.lastOption.toRight("the project has no dataset")
      stored   <- storedStimuli(project.manifest)
      rows     <- GoldenAssets.rows
      registry <- AssetRegistry.fromRows(dataset, rows, stored).leftMap(_.message)
    yield registry
