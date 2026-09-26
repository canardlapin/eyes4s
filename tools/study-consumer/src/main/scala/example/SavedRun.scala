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

package example

import cats.data.NonEmptyVector
import eyes4s.codec.*
import eyes4s.plan.*

import java.nio.charset.StandardCharsets

/** The application's storage layout for any saved run, whatever its route:
  * the manifest in `manifest.json`, every entry under its manifest name, and
  * the manifest's address in `manifest.sha256`. eyes4s owns the manifest and
  * its verification; where the files live is the application's choice.
  */
object SavedRun:
  val manifestFile = "manifest.json"
  val addressFile  = "manifest.sha256"

  /** Every file of a saved run under this layout. Entry names must differ
    * from the two layout files.
    */
  def files(saved: SavedManifest): Vector[(String, IArray[Byte])] =
    (manifestFile  -> saved.bytes) +:
      (addressFile -> IArray.from(saved.address.hex.getBytes(StandardCharsets.UTF_8))) +:
      saved.artifacts.map(a => a.name.value -> a.bytes)

  /** Serve a stored run from whatever holds its files, by this layout. */
  def source(files: String => Option[IArray[Byte]]): ByteSource = ByteSource {
    case ByteRequest.Manifest(_)  => files(manifestFile).toRight(SourceFailure.Missing)
    case ByteRequest.Entry(entry) => files(entry.name.value).toRight(SourceFailure.Missing)
  }

  /** The one artifact of `role` a resolved manifest holds. */
  def single[A](
      role: ArtifactRole,
      values: Vector[(ArtifactName, A)]
  ): Either[JourneyError, A] =
    values match
      case Vector((_, value)) => Right(value)
      case other              => Left(JourneyError.Entries(role, other.size))

/** Why a saved run could not be reloaded or rerun, kept typed for every
  * route. Each case wraps the library's own refusal.
  */
enum JourneyError derives CanEqual:
  case Codec(error: CodecError)
  case Resolve(errors: NonEmptyVector[ResolveError])

  /** The resolved manifest did not hold exactly one artifact of `role`. */
  case Entries(role: ArtifactRole, count: Int)
  case Preflight(error: PreflightError[Any])
  case Descriptor(error: DescriptorError)

  /** A recording input whose channels a recording plan cannot run on. */
  case Recording(error: RecordingInputError)

  /** A temporal plan that could not be prepared on its reloaded input. */
  case Temporal(error: TemporalStudyError)
