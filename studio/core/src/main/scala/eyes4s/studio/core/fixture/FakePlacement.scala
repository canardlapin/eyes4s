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

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.DatasetRevisionSpec
import eyes4s.studio.core.geometry.PlacementPreviewAdapter

import java.nio.charset.StandardCharsets.UTF_8

/** The fake's placement preview (S5.5): the backend places every record of a
  * revision's fixation source with eyes4s, under the revision's geometry and
  * recorded corrections, whether the revision is stored or a draft. The fake
  * holds one fixation file, the fixture's fixations.csv ([[GoldenFixationsCsv]]);
  * a revision whose source is another file is refused, naming both digests.
  */
object FakePlacement:

  private lazy val bytes: IArray[Byte] = IArray.from(GoldenFixationsCsv.text.getBytes(UTF_8))
  private lazy val digest: ByteDigest  = ByteDigest.sha256(bytes)

  def of(spec: DatasetRevisionSpec): Either[BackendError, PlacementPreview] =
    def refused(reason: String) = BackendError.PlacementRefused(spec.id, reason)
    for
      source <- spec.sources.fixations.toRight(refused("it has no fixation source"))
      _      <- Either.cond(
        source.bytes == digest,
        (),
        refused(
          s"its fixation source has sha256:${source.bytes.hex}; this backend holds " +
            s"sha256:${digest.hex} only"
        )
      )
      preview <- place(spec, bytes)
    yield preview

  /** `spec`'s records read from `source` (its fixation file's bytes) and
    * placed with eyes4s: what a backend holding that file answers. Tests
    * stand for a backend with another file through it.
    */
  def place(
      spec: DatasetRevisionSpec,
      source: IArray[Byte]
  ): Either[BackendError, PlacementPreview] =
    PlacementPreviewAdapter.place(spec, source)
