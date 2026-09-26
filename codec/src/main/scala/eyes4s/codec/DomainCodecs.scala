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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.core.ObservedCoverage
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.plan.DefinitionId
import io.circe.Json

/** Scientific values use the same raw wire vocabulary as saved plans.
  * Time is always a decimal string of signed 64-bit microseconds.
  */
object DomainCodecs:
  def frame[U <: Unit2D: UnitLabel](schema: DefinitionId): VersionedCodec[Frame[U]] =
    VersionedCodec.of(schema)(DomainWire.frame[U])(DomainWire.readFrame[U])

  def grid[U <: Unit2D: UnitLabel](schema: DefinitionId): VersionedCodec[Grid[U]] =
    VersionedCodec.of[Grid[U]](schema)(g =>
      Json.obj(
        "id"    -> Json.fromString(g.id.name),
        "frame" -> DomainWire.frame(g.frame),
        "nx"    -> Json.fromInt(g.nx),
        "ny"    -> Json.fromInt(g.ny)
      )
    )(j =>
      for
        f <- Wire.field[Json](j, "frame").flatMap(DomainWire.readFrame[U])
        g <- DomainWire.readGrid(j, f)
      yield g
    )

  /** The witness carries units, never a conversion. Display names are derived. */
  def unit[U <: Unit2D](
      schema: DefinitionId
  )(using u: UnitLabel[U]): VersionedCodec[UnitLabel[U]] =
    VersionedCodec.checked[UnitLabel[U]](schema)(value =>
      Either.cond(
        value.symbol == u.symbol,
        Json.fromString(u.symbol),
        CodecError.Field("unit", Json.fromString(value.symbol), s"expected ${u.symbol}")
      )
    )(j =>
      Either.cond(
        j.asString.contains(u.symbol),
        u,
        CodecError.Field("unit", j, s"expected ${u.symbol}")
      )
    )

  def clock(schema: DefinitionId): VersionedCodec[ClockId] =
    VersionedCodec.of[ClockId](schema)(c => Json.fromString(c.name))(j =>
      j.asString
        .map(ClockId.apply)
        .toRight(CodecError.Field("clock", j, "expected nominal clock ID"))
    )

  def instant(schema: DefinitionId): VersionedCodec[Instant] =
    VersionedCodec.of[Instant](schema)(i => DomainWire.time(i.toMicros))(j =>
      DomainWire.readTime(j, "instantMicros").map(Instant.micros)
    )
  def span(schema: DefinitionId): VersionedCodec[Span] =
    VersionedCodec.of[Span](schema)(s => DomainWire.time(s.toMicros))(j =>
      DomainWire.readTime(j, "spanMicros").map(Span.micros)
    )

  def interval(schema: DefinitionId): VersionedCodec[Interval] =
    VersionedCodec.of(schema)(DomainWire.interval)(DomainWire.readInterval)

  def window(schema: DefinitionId): VersionedCodec[Window] =
    VersionedCodec.of[Window](schema)(w =>
      Json.obj(
        "fromMicros"  -> DomainWire.time(w.from.toMicros),
        "untilMicros" -> DomainWire.time(w.until.toMicros)
      )
    )(j =>
      for
        a <- DomainWire.micros(j, "fromMicros")
        b <- DomainWire.micros(j, "untilMicros")
        w <- Window
          .of(Span.micros(a), Span.micros(b))
          .left
          .map(e => CodecError.Field("window", j, e.message))
      yield w
    )

  def coverage(schema: DefinitionId): VersionedCodec[ObservedCoverage] =
    VersionedCodec.of[ObservedCoverage](schema)(c =>
      Json.obj(
        "clock"     -> Json.fromString(c.clock.name),
        "intervals" -> Json.arr(c.intervals.map(DomainWire.interval)*)
      )
    )(j =>
      for
        c         <- Wire.field[String](j, "clock")
        xs        <- Wire.field[Vector[Json]](j, "intervals")
        intervals <- xs.traverse(DomainWire.readInterval)
        _         <- Wire.ascending("intervals", intervals.map(_.onset.toMicros).zip(xs))
        result    <- ObservedCoverage
          .of(ClockId(c), intervals)
          .left
          .map(e => CodecError.Field("coverage", j, e.message))
      yield result
    )

  def perspective(schema: DefinitionId): VersionedCodec[Perspective] =
    VersionedCodec.of(schema)(DomainWire.perspective)(DomainWire.readPerspective)

  def syncMark(schema: DefinitionId): VersionedCodec[SyncMark] =
    VersionedCodec.of(schema)(DomainWire.mark)(DomainWire.readMark)

  /** Encodes the checked affine mapping; fitted evidence remains a result payload. */
  def synchronization(schema: DefinitionId): VersionedCodec[Sync] =
    VersionedCodec.of[Sync](schema)(s =>
      Json.obj(
        "from"         -> Json.fromString(s.from.name),
        "to"           -> Json.fromString(s.to.name),
        "offsetMicros" -> DomainWire.time(s.offset.toMicros),
        "drift"        -> Json.fromDoubleOrNull(s.drift)
      )
    )(j =>
      for
        a      <- Wire.field[String](j, "from")
        b      <- Wire.field[String](j, "to")
        offset <- DomainWire.micros(j, "offsetMicros")
        drift  <- DomainWire.finite(j, "drift")
        s      <- Sync
          .affine(ClockId(a), ClockId(b), Span.micros(offset), drift)
          .left
          .map(e => CodecError.Field("synchronization", j, e.message))
      yield s
    )

  def identities(schema: DefinitionId): VersionedCodec[DocumentIdentities] =
    VersionedCodec.of[DocumentIdentities](schema)(_.json)(DocumentIdentities.read)

/** Immutable, document-local nominal identities. Different IDs never coalesce.
  * Typed lookups reconstruct values, preserving nominal equality rather than
  * JVM reference identity. No global interning or erased spatial cast is used.
  */
final class DocumentIdentities private (
    private val frames: Vector[DocumentIdentities.FrameEntry],
    private val grids: Vector[DocumentIdentities.GridEntry],
    private val clocks: Vector[ClockId]
):
  import DocumentIdentities.*

  def addFrame[U <: Unit2D: UnitLabel](
      frame: Frame[U]
  ): Either[CodecError, DocumentIdentities] =
    addFrameEntry(FrameEntry(frame.id, frame.spec, DomainWire.frame(frame)))

  private def addFrameEntry(entry: FrameEntry): Either[CodecError, DocumentIdentities] =
    frames.find(_.id == entry.id) match
      case None      => Right(new DocumentIdentities(frames :+ entry, grids, clocks))
      case Some(old) =>
        Agreement
          .frames(old.id, old.spec, entry.id, entry.spec)
          .left
          .map(e => CodecError.Field("frames", entry.json, e.message))
          .flatMap(_ =>
            Either.cond(
              old.json.hcursor.get[String]("unit") ==
                entry.json.hcursor.get[String]("unit"),
              this,
              CodecError.IdentityConflict("frame unit", entry.id.name, old.json, entry.json)
            )
          )

  def addGrid[U <: Unit2D: UnitLabel](grid: Grid[U]): Either[CodecError, DocumentIdentities] =
    addFrame(grid.frame).flatMap(
      _.addGridEntry(
        GridEntry(
          grid.id,
          grid.spec,
          Json.obj(
            "id"    -> Json.fromString(grid.id.name),
            "frame" -> Json.fromString(grid.frame.id.name),
            "nx"    -> Json.fromInt(grid.nx),
            "ny"    -> Json.fromInt(grid.ny)
          )
        )
      )
    )

  private def addGridEntry(entry: GridEntry): Either[CodecError, DocumentIdentities] =
    grids.find(_.id == entry.id) match
      case None      => Right(new DocumentIdentities(frames, grids :+ entry, clocks))
      case Some(old) =>
        Agreement
          .grids(old.id, old.spec, entry.id, entry.spec)
          .left
          .map(e => CodecError.Field("grids", entry.json, e.message))
          .map(_ => this)

  def addClock(clock: ClockId): DocumentIdentities =
    if clocks.contains(clock) then this
    else new DocumentIdentities(frames, grids, clocks :+ clock)

  def frame[U <: Unit2D: UnitLabel](id: FrameId): Either[CodecError, Frame[U]] =
    frames
      .find(_.id == id)
      .toRight(CodecError.MissingIdentity("frame", id.name))
      .flatMap(e => DomainWire.readFrame[U](e.json))

  def grid[U <: Unit2D: UnitLabel](id: GridId): Either[CodecError, Grid[U]] =
    grids
      .find(_.id == id)
      .toRight(CodecError.MissingIdentity("grid", id.name))
      .flatMap(e =>
        for
          f     <- Wire.field[String](e.json, "frame")
          value <- frame[U](FrameId(f))
          g     <- DomainWire.readGrid(e.json, value)
        yield g
      )

  def clock(id: ClockId): Either[CodecError, ClockId] =
    clocks.find(_ == id).toRight(CodecError.MissingIdentity("clock", id.name))

  private[codec] def json: Json = Json.obj(
    "frames" -> Json.arr(frames.map(_.json)*),
    "grids"  -> Json.arr(grids.map(_.json)*),
    "clocks" -> Json.arr(clocks.map(c => Json.fromString(c.name))*)
  )

object DocumentIdentities:
  private final case class FrameEntry(id: FrameId, spec: FrameSpec, json: Json)
  private final case class GridEntry(id: GridId, spec: GridSpec, json: Json)
  val empty: DocumentIdentities =
    new DocumentIdentities(Vector.empty, Vector.empty, Vector.empty)

  private def frameEntry[U <: Unit2D: UnitLabel](j: Json): Either[CodecError, FrameEntry] =
    DomainWire.readFrame[U](j).map(f => FrameEntry(f.id, f.spec, DomainWire.frame(f)))

  private def gridEntry[U <: Unit2D: UnitLabel](
      j: Json,
      f: Json
  ): Either[CodecError, GridEntry] =
    for
      frame <- DomainWire.readFrame[U](f)
      grid  <- DomainWire.readGrid(j, frame)
    yield GridEntry(
      grid.id,
      grid.spec,
      Json.obj(
        "id"    -> Json.fromString(grid.id.name),
        "frame" -> Json.fromString(frame.id.name),
        "nx"    -> Json.fromInt(grid.nx),
        "ny"    -> Json.fromInt(grid.ny)
      )
    )

  /** Refuse a wire table that declares one identity twice: the writer
    * declares each once, so a repetition is a second spelling of the table.
    */
  private def once(
      member: String,
      table: Json,
      declared: Vector[Json],
      id: Json => Option[String]
  ): Either[CodecError, Unit] =
    val ids = declared.map(id)
    Either.cond(
      ids.distinct.size == ids.size,
      (),
      CodecError.NonCanonical(
        member,
        Json.arr(declared*),
        Json.arr(declared.zip(ids).distinctBy(_._2).map(_._1)*),
        "each identity is declared once"
      )
    )

  private[codec] def read(j: Json): Either[CodecError, DocumentIdentities] = for
    fs         <- Wire.field[Vector[Json]](j, "frames")
    gs         <- Wire.field[Vector[Json]](j, "grids")
    cs         <- Wire.field[Vector[String]](j, "clocks")
    _          <- once("frames", j, fs, _.hcursor.get[String]("id").toOption)
    _          <- once("grids", j, gs, _.hcursor.get[String]("id").toOption)
    _          <- once("clocks", j, cs.map(Json.fromString), _.asString)
    withFrames <- fs.foldLeft[Either[CodecError, DocumentIdentities]](Right(empty)) {
      (acc, f) =>
        for
          table <- acc
          unit  <- Wire.field[String](f, "unit")
          entry <- unit match
            case "px"   => frameEntry[Px](f)
            case "deg"  => frameEntry[Deg](f)
            case "norm" => frameEntry[Norm](f)
            case "mm"   => frameEntry[Mm](f)
            case _      => Left(CodecError.Field("unit", f, s"unknown spatial unit '$unit'"))
          next <- table.addFrameEntry(entry)
        yield next
    }
    withGrids <- gs.foldLeft[Either[CodecError, DocumentIdentities]](Right(withFrames)) {
      (acc, g) =>
        for
          table <- acc
          id    <- Wire.field[String](g, "frame")
          f     <- table.frames
            .find(_.id == FrameId(id))
            .toRight(CodecError.MissingIdentity("frame", id))
          unit  <- Wire.field[String](f.json, "unit")
          entry <- unit match
            case "px"   => gridEntry[Px](g, f.json)
            case "deg"  => gridEntry[Deg](g, f.json)
            case "norm" => gridEntry[Norm](g, f.json)
            case "mm"   => gridEntry[Mm](g, f.json)
            case _ => Left(CodecError.Field("unit", f.json, s"unknown spatial unit '$unit'"))
          next <- table.addGridEntry(entry)
        yield next
    }
  yield cs.foldLeft(withGrids)((table, c) => table.addClock(ClockId(c)))

private[codec] object DomainWire:
  def time(micros: Long): Json = Json.fromString(micros.toString)
  def readTime(j: Json, path: String): Either[CodecError, Long] =
    j.asString
      .flatMap(raw => raw.toLongOption.map(raw -> _))
      .toRight(
        CodecError
          .Field(path, j, "expected signed 64-bit integer microseconds as a decimal string")
      )
      .flatMap((raw, value) =>
        Either.cond(
          raw == value.toString,
          value,
          CodecError.NonCanonical(
            path,
            j,
            Json.fromString(value.toString),
            "microseconds are written without a sign or leading zeros they do not need"
          )
        )
      )
  def micros(j: Json, field: String): Either[CodecError, Long] =
    Wire.field[Json](j, field).flatMap(readTime(_, field))
  def finite(j: Json, field: String): Either[CodecError, Double] =
    Wire
      .field[Double](j, field)
      .flatMap(x =>
        Either.cond(x.isFinite, x, CodecError.Field(field, j, "expected a finite number"))
      )

  def frame[U <: Unit2D](f: Frame[U])(using u: UnitLabel[U]): Json = Json.obj(
    "id"    -> Json.fromString(f.id.name),
    "unit"  -> Json.fromString(u.symbol),
    "xMin"  -> Json.fromDoubleOrNull(f.spec.xMin),
    "yMin"  -> Json.fromDoubleOrNull(f.spec.yMin),
    "xMax"  -> Json.fromDoubleOrNull(f.spec.xMax),
    "yMax"  -> Json.fromDoubleOrNull(f.spec.yMax),
    "yAxis" -> Json.fromString(f.yAxis.toString)
  )

  def readFrame[U <: Unit2D](j: Json)(using u: UnitLabel[U]): Either[CodecError, Frame[U]] = for
    id   <- Wire.field[String](j, "id")
    unit <- Wire.field[String](j, "unit")
    _    <- Either.cond(
      unit == u.symbol,
      (),
      CodecError.Field("unit", j, s"expected ${u.symbol}, got $unit")
    )
    a        <- finite(j, "xMin")
    b        <- finite(j, "yMin")
    c        <- finite(j, "xMax")
    d        <- finite(j, "yMax")
    axisName <- Wire.field[String](j, "yAxis")
    axis     <- YAxis.values
      .find(_.toString == axisName)
      .toRight(CodecError.Field("yAxis", j, s"unknown axis $axisName"))
    bounds <- Bounds.of[U](a, b, c, d).left.map(e => CodecError.Field("bounds", j, e.message))
  yield Frame.of(FrameId(id), bounds, axis)

  def readGrid[U <: Unit2D](j: Json, frame: Frame[U]): Either[CodecError, Grid[U]] = for
    id   <- Wire.field[String](j, "id")
    nx   <- Wire.field[Int](j, "nx")
    ny   <- Wire.field[Int](j, "ny")
    grid <- Grid
      .of(GridId(id), frame, nx, ny)
      .left
      .map(e => CodecError.Field("grid", j, e.message))
  yield grid

  def interval(i: Interval): Json = Json.obj(
    "clock"        -> Json.fromString(i.clock.name),
    "onsetMicros"  -> time(i.onset.toMicros),
    "offsetMicros" -> time(i.offset.toMicros)
  )
  def readInterval(j: Json): Either[CodecError, Interval] = for
    c <- Wire.field[String](j, "clock")
    a <- micros(j, "onsetMicros")
    b <- micros(j, "offsetMicros")
    i <- Interval
      .of(ClockId(c), Instant.micros(a), Instant.micros(b))
      .left
      .map(e => CodecError.Field("interval", j, e.message))
  yield i

  def perspective(p: Perspective): Json = Json.obj(
    "distanceMm" -> Json.fromDoubleOrNull(p.distance.toMm),
    "widthMm"    -> Json.fromDoubleOrNull(p.surfaceWidth.toMm),
    "heightMm"   -> Json.fromDoubleOrNull(p.surfaceHeight.toMm)
  )
  def readPerspective(j: Json): Either[CodecError, Perspective] = for
    d <- finite(j, "distanceMm")
    w <- finite(j, "widthMm")
    h <- finite(j, "heightMm")
    p <- Perspective
      .millimetres(d, w, h)
      .left
      .map(e => CodecError.Field("perspective", j, e.message))
  yield p

  def mark(m: SyncMark): Json = Json.obj(
    "id"           -> Json.fromString(m.id),
    "sourceMicros" -> time(m.onSource.toMicros),
    "targetMicros" -> time(m.onTarget.toMicros)
  )
  def readMark(j: Json): Either[CodecError, SyncMark] = for
    id   <- Wire.field[String](j, "id")
    a    <- micros(j, "sourceMicros")
    b    <- micros(j, "targetMicros")
    mark <- SyncMark
      .of(id, Instant.micros(a), Instant.micros(b))
      .left
      .map(e => CodecError.Field("mark", j, e.message))
  yield mark
