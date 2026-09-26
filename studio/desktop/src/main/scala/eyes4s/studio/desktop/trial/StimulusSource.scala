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

package eyes4s.studio.desktop.trial

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{AssetFile, AssetRef}
import eyes4s.studio.core.bundle.{BundleArea, BundlePath, ProjectStore}
import eyes4s.studio.viz.trial.StimulusRaster
import intaglio.{RasterDimensions, RasterImage, Rgba32}
import javafx.scene.image.Image

import java.io.ByteArrayInputStream
import java.nio.file.{Files, NoSuchFileException, Path}
import scala.util.control.NonFatal

/** Why a stored stimulus could not be shown. Every case names the file. */
enum StimulusError derives CanEqual:

  /** The source holds no bytes for the file; `where` says where it looked. */
  case NotStored(file: AssetFile, where: String)

  /** The bytes are not the ones the asset registry recorded. */
  case DigestMismatch(file: AssetFile, expected: ByteDigest, actual: ByteDigest)

  /** The bytes could not be read. */
  case Unreadable(file: AssetFile, reason: String)

  /** The bytes are not an image JavaFX can decode. */
  case Undecodable(file: AssetFile, reason: String)

  def message: String = this match
    case NotStored(f, where)     => s"${f.value} is not stored in $where."
    case DigestMismatch(f, e, a) =>
      s"${f.value} has sha256 ${a.hex.take(12)}…, not the registered ${e.hex.take(12)}…."
    case Unreadable(f, reason)  => s"${f.value} could not be read: $reason."
    case Undecodable(f, reason) => s"${f.value} is not a decodable image: $reason."

/** Where a trial view reads a stored stimulus's bytes (a platform service,
  * DESIGN_SPEC section 13). A source is read off the FX thread.
  */
trait StimulusSource:

  /** The exact bytes stored for `asset`. */
  def bytes(asset: AssetRef): Either[StimulusError, IArray[Byte]]

object StimulusSource:

  /** A directory of stimulus files by name, such as the golden fixture's
    * `fixtures/studio-golden/stimuli`.
    */
  def directory(dir: Path): StimulusSource = asset =>
    val file = dir.resolve(asset.file.value)
    try Right(IArray.unsafeFromArray(Files.readAllBytes(file)))
    catch
      case _: NoSuchFileException => Left(StimulusError.NotStored(asset.file, dir.toString))
      case NonFatal(e)            => Left(StimulusError.Unreadable(asset.file, e.toString))

  /** A project bundle's stored inputs, `inputs/<sha256>/<file>` (S2.3). */
  def bundle(store: ProjectStore[IO])(using runtime: IORuntime): StimulusSource = asset =>
    BundlePath.in(BundleArea.Inputs, s"${asset.sha256.hex}/${asset.file.value}") match
      case Left(e)     => Left(StimulusError.NotStored(asset.file, e.message))
      case Right(path) =>
        try
          store.read(path).unsafeRunSync() match
            case Right(bytes) => Right(bytes)
            case Left(e)      => Left(StimulusError.NotStored(asset.file, e.message))
        catch case NonFatal(e) => Left(StimulusError.Unreadable(asset.file, e.toString))

/** Reads, checks and decodes stored stimuli for the trial view. */
object Stimuli:

  /** `asset`'s bytes from `source`, checked against its registered digest. */
  def verified(source: StimulusSource, asset: AssetRef): Either[StimulusError, IArray[Byte]] =
    source.bytes(asset).flatMap { bytes =>
      val actual = ByteDigest.sha256(bytes)
      Either.cond(
        actual == asset.sha256,
        bytes,
        StimulusError.DigestMismatch(asset.file, asset.sha256, actual)
      )
    }

  /** Decodes image bytes into an Intaglio raster, row zero at the top. */
  def decode(file: AssetFile, bytes: IArray[Byte]): Either[StimulusError, RasterImage] =
    try
      val image = Image(ByteArrayInputStream(IArray.genericWrapArray(bytes).toArray))
      val w     = image.getWidth.toInt
      val h     = image.getHeight.toInt
      Option(image.getException) match
        case Some(e)               => Left(StimulusError.Undecodable(file, e.toString))
        case None if image.isError => Left(StimulusError.Undecodable(file, "decoding failed"))
        case None if w <= 0 || h <= 0 =>
          Left(StimulusError.Undecodable(file, s"it decodes to ${w}x$h pixels"))
        case None =>
          val reader = image.getPixelReader
          RasterDimensions(w, h).left
            .map(e => StimulusError.Undecodable(file, e.message))
            .map(dims =>
              RasterImage.tabulate(dims) { (x, y) =>
                val argb = reader.getArgb(x, y)
                Rgba32
                  .unsafe((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff, (argb >>> 24))
              }
            )
    catch case NonFatal(e) => Left(StimulusError.Undecodable(file, e.toString))

  /** What the trial scene shows for `asset`: its raster, or why it has none. */
  def load(source: StimulusSource, asset: AssetRef): StimulusRaster =
    verified(source, asset).flatMap(decode(asset.file, _)) match
      case Right(image) => StimulusRaster.Loaded(image)
      case Left(error)  => StimulusRaster.Unreadable(error.message)
