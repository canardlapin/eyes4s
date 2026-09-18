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

import eyes4s.codec.*

import java.nio.charset.StandardCharsets

/** Perturbations of a stored run, as a damaged disk or a hand-edited file
  * would make them. Each works on a private copy of the stored files.
  */
object Forgery:
  import JourneySetup.get

  /** One byte of `bytes` changed at `at`. */
  def flip(bytes: IArray[Byte], at: Int): IArray[Byte] =
    IArray.tabulate(bytes.length)(i => if i == at then (bytes(i) ^ 1).toByte else bytes(i))

  /** Replace entry `name` with `bytes` and re-declare its length and SHA-256
    * consistently in a new manifest, keeping its declared semantic identity:
    * the files then pass every integrity check, so only what the reader
    * re-derives can refuse them.
    */
  def redeclare(
      files: Map[String, IArray[Byte]],
      saved: SavedManifest,
      name: String,
      bytes: IArray[Byte]
  ): Map[String, IArray[Byte]] =
    val entries = saved.manifest.entries.map { e =>
      if e.name.value == name then
        get(
          ManifestEntry.of(
            e.name,
            e.role,
            e.schema,
            e.media,
            bytes.length.toLong,
            ByteDigest.sha256(bytes),
            e.identity,
            e.layout
          )
        )
      else e
    }
    val manifest = get(ScientificManifest.of(entries, saved.manifest.relations))
    val encoded  = get(ScientificManifest.bytes(manifest))
    files ++ Map(
      name                  -> bytes,
      SavedRun.manifestFile -> encoded,
      SavedRun.addressFile  -> IArray.from(
        ByteDigest.sha256(encoded).hex.getBytes(StandardCharsets.UTF_8)
      )
    )

  /** The innermost cause of a codec error and the entry path that locates it. */
  def leaf(error: CodecError, path: Vector[String] = Vector.empty): (String, CodecError) =
    error match
      case CodecError.Entry(at, inner) => leaf(inner, path :+ at)
      case other                       => (path.mkString("."), other)
