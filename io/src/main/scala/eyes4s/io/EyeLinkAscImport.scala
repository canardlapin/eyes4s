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

package eyes4s.io

import cats.effect.kernel.Concurrent
import cats.syntax.functor.*

import _root_.fs2.io.file.Files
import _root_.fs2.io.file.Path

/** Task-level entrance to the lossless EyeLink ASC pipeline.
  *
  * This facade has no second parser semantics: it runs [[EyeLinkAscFraming]]
  * and passes every emission to [[EyeLinkAscSessions.materialize]]. Byte input
  * can derive its own unidentified origin exactly. Path input requires an
  * explicit origin because release use must retain the independently observed
  * or declared EDF2ASC conversion receipt rather than inventing provenance
  * after reading.
  */
object EyeLinkAscImport:

  /** Import already available ASC bytes for exploratory analysis.
    *
    * The complete byte content becomes the source digest. The resulting
    * origin remains `Unidentified`, so a release evidence policy will reject
    * it even though the digest is exact.
    */
  def fromBytes(
      bytes: IArray[Byte],
      settings: AscStreamSettings,
      config: EyeLinkAscSessionConfig
  ): EyeLinkAscSessionMaterialization =
    fromBytes(
      bytes,
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(bytes)),
      settings,
      config
    )

  /** Import already available ASC bytes with explicit conversion provenance.
    *
    * A receipt whose ASC digest disagrees with `bytes` remains visible as a
    * `SourceDigestMismatch` and prevents trusted assembly.
    */
  def fromBytes(
      bytes: IArray[Byte],
      origin: EyeLinkAscOrigin,
      settings: AscStreamSettings,
      config: EyeLinkAscSessionConfig
  ): EyeLinkAscSessionMaterialization =
    EyeLinkAscSessions.materialize(
      origin,
      config,
      EyeLinkAscFraming.whole(settings, bytes)
    )

  /** Read and materialize one ASC path through the bounded FS2 framer.
    *
    * Materializing a trusted session necessarily retains its parsed records
    * and samples. Call [[EyeLinkAscStreaming.readPath]] directly when a caller
    * only needs bounded-memory framing or a streaming fold.
    */
  def readPath[F[_]: Concurrent](
      path: Path,
      origin: EyeLinkAscOrigin,
      settings: AscStreamSettings,
      config: EyeLinkAscSessionConfig
  )(using Files[F]): F[EyeLinkAscSessionMaterialization] =
    EyeLinkAscStreaming
      .readPath[F](path, settings)
      .compile
      .toVector
      .map(framing => EyeLinkAscSessions.materialize(origin, config, framing))

end EyeLinkAscImport
