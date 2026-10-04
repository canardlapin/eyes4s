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

package eyes4s.studio.core.document

import eyes4s.studio.core.assets.{AssetFile, AssetRef}
import eyes4s.studio.core.backend.DatasetRevision
import io.circe.{Codec, Decoder, Encoder}

/** One repaired display asset (S2.5, S5.7): dataset revision `dataset`'s
  * trial inventory names `file`, and its bytes are the stored input `asset`
  * (its own name and the SHA-256 of the bytes the repair imported). The
  * inventory's name is kept, so a reopened project resolves it again.
  */
final case class AssetRelink(dataset: DatasetRevision, file: AssetFile, asset: AssetRef)
    derives CanEqual

object AssetRelink:
  given Encoder.AsObject[AssetRelink] =
    Encoder.forProduct3("dataset", "file", "asset")(r => (r.dataset, r.file, r.asset))
  given Decoder[AssetRelink] =
    Decoder.forProduct3("dataset", "file", "asset")(AssetRelink.apply)

/** A document's repaired display assets, in dataset and file order, one per
  * dataset revision and inventory file. They are not science: a repair
  * changes which bytes a display shows, never gaze, admission or a score.
  */
final case class AssetRelinks private (entries: Vector[AssetRelink]) derives CanEqual:
  def isEmpty: Boolean = entries.isEmpty

  /** `dataset`'s repairs. */
  def of(dataset: DatasetRevision): Vector[AssetRelink] = entries.filter(_.dataset == dataset)

  /** `dataset`'s repair of `file`, if any. */
  def find(dataset: DatasetRevision, file: AssetFile): Option[AssetRelink] =
    entries.find(r => r.dataset == dataset && r.file == file)

  /** With `dataset`'s `file` relinked to `asset`, or unlinked (`None`). */
  def set(dataset: DatasetRevision, file: AssetFile, asset: Option[AssetRef]): AssetRelinks =
    AssetRelinks.sorted(
      entries.filterNot(r => r.dataset == dataset && r.file == file) ++
        asset.map(AssetRelink(dataset, file, _))
    )

object AssetRelinks:
  val empty: AssetRelinks = AssetRelinks(Vector.empty)

  private def sorted(entries: Vector[AssetRelink]): AssetRelinks =
    AssetRelinks(entries.sortBy(r => (r.dataset.number, r.file.value)))

  /** `entries` in order, each dataset revision's file once. */
  def of(entries: Vector[AssetRelink]): Either[DocumentError, AssetRelinks] =
    entries
      .groupBy(r => (r.dataset, r.file))
      .collectFirst {
        case ((d, f), rs) if rs.size > 1 => DocumentError.DuplicateRelink(d, f.value)
      }
      .toLeft(sorted(entries))

  given Codec[AssetRelinks] = DocumentCodecs.validated(of, _.entries)
