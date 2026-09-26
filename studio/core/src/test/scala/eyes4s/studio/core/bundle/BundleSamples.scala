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

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.*

import java.nio.charset.StandardCharsets.UTF_8

/** Bundle inputs for the sample documents, and the golden t2 bundle's pins. */
object BundleSamples:

  def right[A](e: Either[BundleError, A]): A =
    e.fold(l => throw new AssertionError(l.message), identity)

  def utf8(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  /** Byte lengths of fixtures/studio-golden/fixations.csv and trials.csv;
    * the golden-bundle suite in studio-desktop checks them against the files.
    */
  val fixationsLength: Long = 572152L
  val trialsLength: Long    = 57968L

  /** One input entry per distinct source of `document`, named after the
    * source path's file name. Lengths are those of the golden files for the
    * golden digests and zero otherwise; no bytes are stored.
    */
  def inputsFor(document: StudioDocument): Vector[InputEntry] =
    document.datasets
      .flatMap(_.sources.entries)
      .distinctBy(s => (s.role, s.bytes))
      .map { s =>
        val length = s.role match
          case SourceRole.Fixations if s.bytes.hex.startsWith("19342ece") => fixationsLength
          case SourceRole.Trials if s.bytes.hex.startsWith("0668bccf")    => trialsLength
          case _                                                          => 0L
        right(
          InputEntry.of(InputKind.Source(s.role), s.path.value.split('/').last, s.bytes, length)
        )
      }

  /** `document` with every dataset revision's sources replaced by these
    * bytes' digests, so a test can hold the bytes.
    */
  def withSources(
      document: StudioDocument,
      fixations: IArray[Byte],
      trials: IArray[Byte]
  ): StudioDocument =
    def doc[A](e: Either[DocumentError, A]): A =
      e.fold(l => throw new AssertionError(l.message), identity)
    val sources = doc(
      for
        fp <- SourcePath.of("inputs/fixations.csv")
        tp <- SourcePath.of("inputs/trials.csv")
        s  <- Sources.of(
          Vector(
            Source(SourceRole.Fixations, fp, ByteDigest.sha256(fixations), None),
            Source(SourceRole.Trials, tp, ByteDigest.sha256(trials), None)
          )
        )
      yield s
    )
    doc(
      StudioDocument.of(
        document.datasets.map(_.copy(sources = sources)),
        document.analyses,
        document.draft,
        document.runs,
        document.reporting,
        document.figures,
        document.presentation,
        document.jobs
      )
    )
