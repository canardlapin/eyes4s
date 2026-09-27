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

package eyes4s.studio.core.importing

import cats.data.NonEmptyVector
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.{
  ColumnBinding,
  DocumentError,
  Geometry,
  Source,
  SourcePath,
  SourceRole,
  TimeUnit
}
import io.circe.syntax.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

import java.nio.charset.{CodingErrorAction, StandardCharsets}
import java.nio.{ByteBuffer, CharBuffer}

/** Why a preset value was refused; each case names its operand. */
enum PresetError derives CanEqual:
  case BlankName
  case RepeatedName(name: String)
  case UnknownPreset(name: String, known: Vector[String])
  case RepeatedRole(preset: String, role: String, columns: Vector[String])
  case RepeatedColumn(preset: String, column: String, roles: Vector[String])

  def message: String = this match
    case BlankName          => "A preset name is blank."
    case RepeatedName(name) => s"A preset named $name already exists; choose another name."
    case RepeatedRole(p, role, cs) =>
      s"Preset $p maps the $role role to more than one column: ${cs.mkString(", ")}."
    case RepeatedColumn(p, column, rs) =>
      s"Preset $p maps column $column to more than one role: ${rs.mkString(", ")}."
    case UnknownPreset(n, ks) =>
      s"No preset is named $n; the presets are ${
          if ks.isEmpty then "none" else ks.mkString(", ")
        }."

/** A preset's name as the analyst typed it, trimmed. */
final case class PresetName private (value: String) derives CanEqual

object PresetName:
  def of(value: String): Either[PresetError, PresetName] =
    Either.cond(value.trim.nonEmpty, new PresetName(value.trim), PresetError.BlankName)

  given Encoder[PresetName] = Encoder[String].contramap(_.value)
  given Decoder[PresetName] = Decoder[String].emap(of(_).left.map(_.message))

/** A saved import mapping (ticket S5.2): the column each role is read from
  * and the declared time unit, re-applied to another file by column name.
  * It records declarations only; nothing in it is inferred from a file.
  */
final case class ImportPreset private (
    name: PresetName,
    bindings: Vector[ColumnBinding],
    time: Option[TimeUnit]
) derives CanEqual:

  /** This preset re-applied to `preview`; every bound column must exist. */
  def applyTo(preview: CsvPreview): Either[NonEmptyVector[MappingError], MappingDraft] =
    MappingDraft.reapply(preview, MappingOrigin.Preset(name), bindings, time)

object ImportPreset:

  /** A preset binds each role to at most one column and each column to at
    * most one role; anything else would lose a binding when re-applied.
    */
  def of(
      name: PresetName,
      bindings: Vector[ColumnBinding],
      time: Option[TimeUnit]
  ): Either[PresetError, ImportPreset] =
    // The first repeat in binding order, so the error is deterministic.
    val roles = bindings.map(_.role).distinct.collectFirst {
      case r if bindings.count(_.role == r) > 1 => (r, bindings.filter(_.role == r))
    }
    val columns = bindings.map(_.column).distinct.collectFirst {
      case c if bindings.count(_.column == c) > 1 => (c, bindings.filter(_.column == c))
    }
    (roles, columns) match
      case (Some((role, bs)), _) =>
        Left(PresetError.RepeatedRole(name.value, role.label, bs.map(_.column.value)))
      case (_, Some((column, bs))) =>
        Left(PresetError.RepeatedColumn(name.value, column.value, bs.map(_.role.label)))
      case _ => Right(new ImportPreset(name, bindings, time))

  /** The stored format's name and version. */
  val format: String = "studio.import-preset"
  val version: Int   = 1

  given Encoder[ImportPreset] = Encoder.instance(p =>
    Json.obj(
      "format"   -> format.asJson,
      "version"  -> version.asJson,
      "name"     -> p.name.asJson,
      "bindings" -> p.bindings.asJson,
      "time"     -> p.time.asJson
    )
  )

  given Decoder[ImportPreset] = Decoder.instance { c =>
    for
      f <- c.get[String]("format")
      v <- c.get[Int]("version")
      _ <- Either.cond(
        f == format && v == version,
        (),
        DecodingFailure(s"expected $format version $version, got $f version $v", c.history)
      )
      name     <- c.get[PresetName]("name")
      bindings <- c.get[Vector[ColumnBinding]]("bindings")
      time     <- c.get[Option[TimeUnit]]("time")
      preset   <- ImportPreset
        .of(name, bindings, time)
        .left
        .map(e => DecodingFailure(e.message, c.history))
    yield preset
  }

/** The presets the analyst has saved, by distinct name, in save order. */
final case class ImportPresets private (all: Vector[ImportPreset]) derives CanEqual:
  def names: Vector[PresetName] = all.map(_.name)

  def find(name: String): Either[PresetError, ImportPreset] =
    all
      .find(_.name.value == name.trim)
      .toRight(PresetError.UnknownPreset(name, names.map(_.value)))

  /** Add `preset`; a name already saved is refused, never overwritten. */
  def add(preset: ImportPreset): Either[PresetError, ImportPresets] =
    Either.cond(
      !all.exists(_.name == preset.name),
      new ImportPresets(all :+ preset),
      PresetError.RepeatedName(preset.name.value)
    )

object ImportPresets:
  val empty: ImportPresets = new ImportPresets(Vector.empty)

  def of(presets: Vector[ImportPreset]): Either[PresetError, ImportPresets] =
    presets.foldLeft[Either[PresetError, ImportPresets]](Right(empty))((acc, p) =>
      acc.flatMap(_.add(p))
    )

/** A source file read for import: where it was imported from, the SHA-256
  * of its exact bytes and its preview. The bytes are read by the platform;
  * this is where they become a document [[Source]].
  */
final case class SniffedSource(
    role: SourceRole,
    path: SourcePath,
    bytes: ByteDigest,
    preview: CsvPreview
) derives CanEqual:
  def source: Source = Source(role, path, bytes, None)

/** Why a file could not be read for import. */
enum SourceReadError derives CanEqual:
  case BadPath(error: DocumentError)
  case Unpreviewable(error: SniffError)

  /** The platform could not read the file's bytes. */
  case Unreadable(path: String, reason: String)

  def message: String = this match
    case BadPath(e)            => e.message
    case Unpreviewable(e)      => e.message
    case Unreadable(path, why) => s"$path could not be read: $why"

object SniffedSource:

  /** Digest and preview `bytes` imported as `path`. The text must be UTF-8:
    * malformed bytes are refused with their offset, never replaced, and a
    * UTF-16 byte-order mark is refused.
    */
  def read(
      role: SourceRole,
      path: String,
      bytes: IArray[Byte]
  ): Either[SourceReadError, SniffedSource] =
    for
      p <- SourcePath.of(path).left.map(SourceReadError.BadPath(_))
      file = p.value.split('/').last
      text    <- decodeUtf8(file, bytes).left.map(SourceReadError.Unpreviewable(_))
      preview <- CsvSniffer.sniff(file, text).left.map(SourceReadError.Unpreviewable(_))
    yield SniffedSource(role, p, ByteDigest.sha256(bytes), preview)

  /** `bytes` as UTF-8 text, or where they stop being UTF-8. */
  def decodeUtf8(file: String, bytes: IArray[Byte]): Either[SniffError, CharSequence] =
    val raw        = IArray.genericWrapArray(bytes).toArray
    def at(i: Int) = if raw.length > i then raw(i) & 0xff else -1
    if (at(0) == 0xfe && at(1) == 0xff) || (at(0) == 0xff && at(1) == 0xfe) then
      Left(SniffError.Utf16(file))
    else
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      val in     = ByteBuffer.wrap(raw)
      val out    = CharBuffer.allocate(raw.length + 1)
      val result = decoder.decode(in, out, true)
      if result.isError then Left(SniffError.NotUtf8(file, in.position().toLong))
      else
        val flushed = decoder.flush(out)
        if flushed.isError then Left(SniffError.NotUtf8(file, in.position().toLong))
        else
          out.flip()
          Right(out)

/** A declared display geometry as typed into the wizard's Geometry tab:
  * seven fields, parsed together. A field that is not a number names itself.
  */
final case class GeometryFields(
    screenWidth: String,
    screenHeight: String,
    imageLeft: String,
    imageTop: String,
    imageWidth: String,
    imageHeight: String,
    pixelsPerDegree: String
) derives CanEqual:

  def parse: Either[GeometryInputError, Geometry] =
    import eyes4s.studio.core.document.{DeclaredPixelsPerDegree, ImagePlacement, ScreenSize}
    import GeometryField.*
    def int(f: GeometryField) =
      field(f).trim.toIntOption.toRight(GeometryInputError.NotAWholeNumber(f, field(f)))
    val ppd = field(PixelsPerDegree).trim.toDoubleOption
      .filter(_.isFinite)
      .toRight(GeometryInputError.NotANumber(PixelsPerDegree, field(PixelsPerDegree)))
    for
      sw   <- int(ScreenWidth)
      sh   <- int(ScreenHeight)
      il   <- int(ImageLeft)
      it   <- int(ImageTop)
      iw   <- int(ImageWidth)
      ih   <- int(ImageHeight)
      rate <- ppd
      g    <- (for
        screen <- ScreenSize.of(sw, sh)
        image  <- ImagePlacement.of(il, it, iw, ih)
        r      <- DeclaredPixelsPerDegree.of(rate)
        g      <- Geometry.of(screen, image, r)
      yield g).left.map(GeometryInputError.Refused(_))
    yield g

  def field(which: GeometryField): String = which match
    case GeometryField.ScreenWidth     => screenWidth
    case GeometryField.ScreenHeight    => screenHeight
    case GeometryField.ImageLeft       => imageLeft
    case GeometryField.ImageTop        => imageTop
    case GeometryField.ImageWidth      => imageWidth
    case GeometryField.ImageHeight     => imageHeight
    case GeometryField.PixelsPerDegree => pixelsPerDegree

  def set(which: GeometryField, value: String): GeometryFields = which match
    case GeometryField.ScreenWidth     => copy(screenWidth = value)
    case GeometryField.ScreenHeight    => copy(screenHeight = value)
    case GeometryField.ImageLeft       => copy(imageLeft = value)
    case GeometryField.ImageTop        => copy(imageTop = value)
    case GeometryField.ImageWidth      => copy(imageWidth = value)
    case GeometryField.ImageHeight     => copy(imageHeight = value)
    case GeometryField.PixelsPerDegree => copy(pixelsPerDegree = value)

/** One field of [[GeometryFields]]. */
enum GeometryField derives CanEqual:
  case ScreenWidth, ScreenHeight, ImageLeft, ImageTop, ImageWidth, ImageHeight, PixelsPerDegree

  def label: String = this match
    case ScreenWidth     => "screen width"
    case ScreenHeight    => "screen height"
    case ImageLeft       => "image left"
    case ImageTop        => "image top"
    case ImageWidth      => "image width"
    case ImageHeight     => "image height"
    case PixelsPerDegree => "pixels per degree"

/** Why the typed geometry was refused; each case names its field. */
enum GeometryInputError derives CanEqual:
  case NotAWholeNumber(field: GeometryField, raw: String)
  case NotANumber(field: GeometryField, raw: String)
  case Refused(error: DocumentError)

  def message: String = this match
    case NotAWholeNumber(f, raw) => s"The ${f.label} '$raw' is not a whole number of pixels."
    case NotANumber(f, raw)      => s"The ${f.label} '$raw' is not a finite number."
    case Refused(e)              => e.message

object GeometryFields:
  val blank: GeometryFields = GeometryFields("", "", "", "", "", "", "")

  def of(g: Geometry): GeometryFields =
    GeometryFields(
      g.screen.width.toString,
      g.screen.height.toString,
      g.image.left.toString,
      g.image.top.toString,
      g.image.width.toString,
      g.image.height.toString,
      Format.decimal(g.pixelsPerDegree.value)
    )

  private object Format:
    /** 35.0 → "35", 35.5 → "35.5". */
    def decimal(d: Double): String =
      if d.isWhole && d.abs < 1e15 then d.toLong.toString else d.toString
