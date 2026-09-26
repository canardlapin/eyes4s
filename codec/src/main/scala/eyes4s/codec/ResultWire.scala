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

import cats.data.NonEmptyVector
import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import eyes4s.surface.{EstimateError, SmoothingAxis}
import io.circe.Json

/** Wire forms for the typed parts of a completed result: provenance, every
  * failure family a fixation study can record, pairing and reduction reports,
  * evaluation metadata and contrast errors. Each enum case is written with an
  * explicit `kind` tag and its operands, never a rendered message alone, so an
  * archived failure still names the trial, pair, frame, grid or component it
  * concerns. Unknown tags and malformed operands are located field errors.
  */
private[codec] object ResultWire:

  def kind(json: Json): Either[CodecError, String] = Wire.field[String](json, "kind")

  def unknown(json: Json, what: String, found: String): CodecError =
    CodecError.Field("kind", json, s"unknown $what $found")

  def tagged(name: String, fields: (String, Json)*): Json =
    Json.obj(("kind" -> Json.fromString(name)) +: fields*)

  /** 64-bit integers travel as decimal strings, like microsecond times. */
  def long(value: Long): Json = Json.fromString(value.toString)

  def readLong(json: Json, field: String): Either[CodecError, Long] =
    DomainWire.micros(json, field)

  /** Doubles that may legitimately be non-finite, as error operands are. */
  def double(value: Double): Json =
    if value.isFinite then Json.fromDoubleOrNull(value)
    else if value.isNaN then Json.fromString("NaN")
    else if value > 0 then Json.fromString("Infinity")
    else Json.fromString("-Infinity")

  def readDouble(json: Json, field: String): Either[CodecError, Double] =
    Wire.field[Json](json, field).flatMap { value =>
      value.asNumber.map(_.toDouble) match
        case Some(number) => Right(number)
        case None         =>
          value.asString match
            case Some("NaN")       => Right(Double.NaN)
            case Some("Infinity")  => Right(Double.PositiveInfinity)
            case Some("-Infinity") => Right(Double.NegativeInfinity)
            case _                 =>
              Left(CodecError.Field(field, json, "expected a number or NaN/Infinity/-Infinity"))
    }

  def optional[A](json: Json, field: String)(
      read: Json => Either[CodecError, A]
  ): Either[CodecError, Option[A]] =
    Wire
      .field[Json](json, field)
      .flatMap(value => if value.isNull then Right(None) else read(value).map(Some(_)))

  def strings(values: Vector[String]): Json = Json.arr(values.map(Json.fromString)*)

  /** A codec's payload without its envelope; the schema is declared once per document. */
  def payload[A](codec: VersionedCodec[A], value: A): Either[CodecError, Json] =
    codec.encode(value).flatMap(Wire.field[Json](_, "value"))

  def unwrap[A](codec: VersionedCodec[A], json: Json): Either[CodecError, A] =
    codec.decode(Json.obj("schema" -> Wire.id(codec.schema), "value" -> json))

  def keyed[K](keys: VersionedCodec[K], json: Json, field: String): Either[CodecError, K] =
    Wire.field[Json](json, field).flatMap(keys.decode)

  def keyVector[K](
      keys: VersionedCodec[K],
      json: Json,
      field: String
  ): Either[CodecError, Vector[K]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.zipWithIndex.traverse { case (entry, index) =>
        keys.decode(entry).left.map(Wire.at(s"$field[$index]"))
      })

  // ---- provenance --------------------------------------------------------

  def param(value: Provenance.Param): Json = value match
    case Provenance.Param.Num(v)  => tagged("num", "value" -> double(v))
    case Provenance.Param.Text(v) => tagged("text", "value" -> Json.fromString(v))
    case Provenance.Param.Flag(v) => tagged("flag", "value" -> Json.fromBoolean(v))

  def readParam(json: Json): Either[CodecError, Provenance.Param] = kind(json).flatMap {
    case "num"  => readDouble(json, "value").map(Provenance.Param.Num.apply)
    case "text" => Wire.field[String](json, "value").map(Provenance.Param.Text.apply)
    case "flag" => Wire.field[Boolean](json, "value").map(Provenance.Param.Flag.apply)
    case other  => Left(unknown(json, "parameter kind", other))
  }

  def params(values: Vector[(String, Provenance.Param)]): Json =
    Json.arr(values.map { case (name, value) =>
      Json.fromFields(
        ("name" -> Json.fromString(name)) +: param(value).asObject.toVector.flatMap(_.toVector)
      )
    }*)

  def readParams(
      json: Json,
      field: String
  ): Either[CodecError, Vector[(String, Provenance.Param)]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.zipWithIndex.traverse { case (entry, index) =>
        (for
          name  <- Wire.field[String](entry, "name")
          value <- readParam(entry)
        yield name -> value).left.map(Wire.at(s"$field[$index]"))
      })

  def values(values: Vector[Provenance.Param]): Json = Json.arr(values.map(param)*)

  def readValues(json: Json, field: String): Either[CodecError, Vector[Provenance.Param]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.zipWithIndex.traverse { case (entry, index) =>
        readParam(entry).left.map(Wire.at(s"$field[$index]"))
      })

  def provenance(value: Provenance): Json = Json.obj(
    "inputs" -> Json.fromString(value.inputs.render),
    "steps"  -> Json.arr(
      value.steps.map(step =>
        Json.obj(
          "operation" -> Json.fromString(step.operation),
          "params"    -> params(step.params)
        )
      )*
    )
  )

  def hash(json: Json, field: String): Either[CodecError, ContentHash] =
    Wire
      .field[String](json, field)
      .flatMap(rendered =>
        ContentHash
          .parse(rendered)
          .toRight(CodecError.Field(field, json, "expected 16 lowercase hexadecimal digits"))
      )

  def readProvenance(json: Json): Either[CodecError, Provenance] = for
    inputs  <- hash(json, "inputs")
    entries <- Wire.field[Vector[Json]](json, "steps")
    steps   <- entries.zipWithIndex.traverse { case (entry, index) =>
      (for
        operation <- Wire.field[String](entry, "operation")
        values    <- readParams(entry, "params")
      yield Provenance.Step(operation, values)).left.map(Wire.at(s"steps[$index]"))
    }
  yield Provenance(inputs, steps)

  // ---- geometry metadata ------------------------------------------------

  def frameId(id: FrameId): Json = Json.fromString(id.name)
  def gridId(id: GridId): Json   = Json.fromString(id.name)
  def clockId(id: ClockId): Json = Json.fromString(id.name)

  def readFrameId(json: Json, field: String): Either[CodecError, FrameId] =
    Wire.field[String](json, field).map(FrameId.apply)
  def readGridId(json: Json, field: String): Either[CodecError, GridId] =
    Wire.field[String](json, field).map(GridId.apply)
  def readClockId(json: Json, field: String): Either[CodecError, ClockId] =
    Wire.field[String](json, field).map(ClockId.apply)

  def frameSpec(id: FrameId, spec: FrameSpec): Json = Json.obj(
    "id"    -> frameId(id),
    "xMin"  -> Json.fromDoubleOrNull(spec.xMin),
    "yMin"  -> Json.fromDoubleOrNull(spec.yMin),
    "xMax"  -> Json.fromDoubleOrNull(spec.xMax),
    "yMax"  -> Json.fromDoubleOrNull(spec.yMax),
    "yAxis" -> Json.fromString(spec.yAxis.toString)
  )

  /** A frame specification is unit-free structural metadata; the pixel frame
    * that rebuilds it is returned so callers take its identity and spec.
    */
  def readFrameSpec(json: Json): Either[CodecError, Frame[Unit2D.Px]] = for
    id       <- Wire.field[String](json, "id")
    a        <- DomainWire.finite(json, "xMin")
    b        <- DomainWire.finite(json, "yMin")
    c        <- DomainWire.finite(json, "xMax")
    d        <- DomainWire.finite(json, "yMax")
    axisName <- Wire.field[String](json, "yAxis")
    axis     <- YAxis.values
      .find(_.toString == axisName)
      .toRight(CodecError.Field("yAxis", json, s"unknown axis $axisName"))
    bounds <- Bounds
      .of[Unit2D.Px](a, b, c, d)
      .left
      .map(e => CodecError.Field("bounds", json, e.message))
  yield Frame.of(FrameId(id), bounds, axis)

  def gridSpec(id: GridId, spec: GridSpec): Json = Json.obj(
    "id"    -> gridId(id),
    "frame" -> frameSpec(spec.frameId, spec.frame),
    "nx"    -> Json.fromInt(spec.nx),
    "ny"    -> Json.fromInt(spec.ny)
  )

  def readGridSpec(json: Json): Either[CodecError, (GridId, GridSpec)] = for
    id    <- Wire.field[String](json, "id")
    frame <- Wire.field[Json](json, "frame").flatMap(readFrameSpec)
    nx    <- Wire.field[Int](json, "nx")
    ny    <- Wire.field[Int](json, "ny")
    grid  <- Grid
      .of(GridId(id), frame, nx, ny)
      .left
      .map(e => CodecError.Field("grid", json, e.message))
  yield GridId(id) -> grid.spec

  def lengthUnit(unit: LengthUnit): Json = Json.fromString(unit.symbol)

  def readLengthUnit(json: Json, field: String): Either[CodecError, LengthUnit] =
    Wire
      .field[String](json, field)
      .flatMap(symbol =>
        LengthUnit.values
          .find(_.symbol == symbol)
          .toRight(CodecError.Field(field, json, s"unknown length unit $symbol"))
      )

  // ---- error families ---------------------------------------------------

  def geometryError(error: GeometryError): Json =
    import GeometryError.*
    error match
      case DegenerateBounds(x0, y0, x1, y1) =>
        tagged(
          "degenerateBounds",
          "xMin" -> double(x0),
          "yMin" -> double(y0),
          "xMax" -> double(x1),
          "yMax" -> double(y1)
        )
      case BoundsExtentOverflow(x0, y0, x1, y1) =>
        tagged(
          "boundsExtentOverflow",
          "xMin" -> double(x0),
          "yMin" -> double(y0),
          "xMax" -> double(x1),
          "yMax" -> double(y1)
        )
      case NonFiniteBounds(x0, y0, x1, y1) =>
        tagged(
          "nonFiniteBounds",
          "xMin" -> double(x0),
          "yMin" -> double(y0),
          "xMax" -> double(x1),
          "yMax" -> double(y1)
        )
      case FrameMismatch(l, r) =>
        tagged("frameMismatch", "left" -> frameId(l), "right" -> frameId(r))
      case FrameIdentityConflict(id, l, r) =>
        tagged(
          "frameIdentityConflict",
          "id"    -> frameId(id),
          "left"  -> frameSpec(id, l),
          "right" -> frameSpec(id, r)
        )
      case NonFiniteLength(v, u) =>
        tagged("nonFiniteLength", "value" -> double(v), "unit" -> lengthUnit(u))
      case NegativeLength(v, u) =>
        tagged("negativeLength", "value" -> double(v), "unit" -> lengthUnit(u))
      case NonPositivePerspective(d, w, h) =>
        tagged(
          "nonPositivePerspective",
          "distanceMm" -> double(d),
          "widthMm"    -> double(w),
          "heightMm"   -> double(h)
        )
      case NotAffine(m)           => tagged("notAffine", "matrix" -> Json.fromString(m))
      case NonFiniteSigma(v)      => tagged("nonFiniteSigma", "value" -> double(v))
      case NonPositiveSigma(v)    => tagged("nonPositiveSigma", "value" -> double(v))
      case DegenerateGrid(nx, ny) =>
        tagged("degenerateGrid", "nx" -> Json.fromInt(nx), "ny" -> Json.fromInt(ny))
      case GridCellCountOverflow(nx, ny, cells) =>
        tagged(
          "gridCellCountOverflow",
          "nx"    -> Json.fromInt(nx),
          "ny"    -> Json.fromInt(ny),
          "cells" -> long(cells)
        )
      case DegenerateEllipse(rx, ry) =>
        tagged("degenerateEllipse", "rx" -> double(rx), "ry" -> double(ry))
      case DegeneratePolygon(n) => tagged("degeneratePolygon", "vertices" -> Json.fromInt(n))
      case NonFiniteRegion(s)   => tagged("nonFiniteRegion", "shape" -> Json.fromString(s))
      case NonFiniteVelocity(v) => tagged("nonFiniteVelocity", "value" -> double(v))
      case NegativeVelocity(v)  => tagged("negativeVelocity", "value" -> double(v))
      case NonFiniteDistance(v) => tagged("nonFiniteDistance", "value" -> double(v))
      case NegativeDistance(v)  => tagged("negativeDistance", "value" -> double(v))
      case SubframeOutsideParent(window, x0, y0, x1, y1, parent, spec) =>
        tagged(
          "subframeOutsideParent",
          "window" -> frameId(window),
          "xMin"   -> double(x0),
          "yMin"   -> double(y0),
          "xMax"   -> double(x1),
          "yMax"   -> double(y1),
          "parent" -> frameSpec(parent, spec)
        )
      case SubframeIdentity(window) => tagged("subframeIdentity", "window" -> frameId(window))
      case NonPositiveAngularScale(frame, value) =>
        tagged("nonPositiveAngularScale", "frame" -> frameId(frame), "value" -> double(value))
      case NonFiniteTranslation(dx, dy) =>
        tagged("nonFiniteTranslation", "dx" -> double(dx), "dy" -> double(dy))

  def readGeometryError(json: Json): Either[CodecError, GeometryError] =
    import GeometryError.*
    def bounds(build: (Double, Double, Double, Double) => GeometryError) = for
      a <- readDouble(json, "xMin")
      b <- readDouble(json, "yMin")
      c <- readDouble(json, "xMax")
      d <- readDouble(json, "yMax")
    yield build(a, b, c, d)
    def value(build: Double => GeometryError) = readDouble(json, "value").map(build)
    def length(build: (Double, LengthUnit) => GeometryError) = for
      v <- readDouble(json, "value")
      u <- readLengthUnit(json, "unit")
    yield build(v, u)
    kind(json).flatMap {
      case "degenerateBounds"     => bounds(DegenerateBounds.apply)
      case "nonFiniteBounds"      => bounds(NonFiniteBounds.apply)
      case "boundsExtentOverflow" => bounds(BoundsExtentOverflow.apply)
      case "frameMismatch"        =>
        for
          l <- readFrameId(json, "left")
          r <- readFrameId(json, "right")
        yield FrameMismatch(l, r)
      case "frameIdentityConflict" =>
        for
          id <- readFrameId(json, "id")
          l  <- Wire.field[Json](json, "left").flatMap(readFrameSpec)
          r  <- Wire.field[Json](json, "right").flatMap(readFrameSpec)
        yield FrameIdentityConflict(id, l.spec, r.spec)
      case "nonFiniteLength"        => length(NonFiniteLength.apply)
      case "negativeLength"         => length(NegativeLength.apply)
      case "nonPositivePerspective" =>
        for
          d <- readDouble(json, "distanceMm")
          w <- readDouble(json, "widthMm")
          h <- readDouble(json, "heightMm")
        yield NonPositivePerspective(d, w, h)
      case "notAffine"        => Wire.field[String](json, "matrix").map(NotAffine.apply)
      case "nonFiniteSigma"   => value(NonFiniteSigma.apply)
      case "nonPositiveSigma" => value(NonPositiveSigma.apply)
      case "degenerateGrid"   =>
        for
          nx <- Wire.field[Int](json, "nx")
          ny <- Wire.field[Int](json, "ny")
        yield DegenerateGrid(nx, ny)
      case "gridCellCountOverflow" =>
        for
          nx    <- Wire.field[Int](json, "nx")
          ny    <- Wire.field[Int](json, "ny")
          cells <- readLong(json, "cells")
        yield GridCellCountOverflow(nx, ny, cells)
      case "degenerateEllipse" =>
        for
          rx <- readDouble(json, "rx")
          ry <- readDouble(json, "ry")
        yield DegenerateEllipse(rx, ry)
      case "degeneratePolygon" => Wire.field[Int](json, "vertices").map(DegeneratePolygon.apply)
      case "nonFiniteRegion"   => Wire.field[String](json, "shape").map(NonFiniteRegion.apply)
      case "nonFiniteVelocity" => value(NonFiniteVelocity.apply)
      case "negativeVelocity"  => value(NegativeVelocity.apply)
      case "nonFiniteDistance" => value(NonFiniteDistance.apply)
      case "negativeDistance"  => value(NegativeDistance.apply)
      case "subframeOutsideParent" =>
        for
          window <- readFrameId(json, "window")
          x0     <- readDouble(json, "xMin")
          y0     <- readDouble(json, "yMin")
          x1     <- readDouble(json, "xMax")
          y1     <- readDouble(json, "yMax")
          parent <- Wire.field[Json](json, "parent").flatMap(readFrameSpec)
        yield SubframeOutsideParent(window, x0, y0, x1, y1, parent.id, parent.spec)
      case "subframeIdentity"        => readFrameId(json, "window").map(SubframeIdentity.apply)
      case "nonPositiveAngularScale" =>
        for
          frame <- readFrameId(json, "frame")
          v     <- readDouble(json, "value")
        yield NonPositiveAngularScale(frame, v)
      case "nonFiniteTranslation" =>
        for
          dx <- readDouble(json, "dx")
          dy <- readDouble(json, "dy")
        yield NonFiniteTranslation(dx, dy)
      case other => Left(unknown(json, "geometry error", other))
    }

  def surfaceError(error: SurfaceError): Json =
    import SurfaceError.*
    error match
      case LengthMismatch(e, a) =>
        tagged("lengthMismatch", "expected" -> Json.fromInt(e), "actual" -> Json.fromInt(a))
      case NegativeWeight(i, v) =>
        tagged("negativeWeight", "index" -> Json.fromInt(i), "value" -> double(v))
      case NegativeValue(i, v) =>
        tagged("negativeValue", "index" -> Json.fromInt(i), "value" -> double(v))
      case NonFiniteValue(i, v) =>
        tagged("nonFiniteValue", "index" -> Json.fromInt(i), "value" -> double(v))
      case DegenerateTotal(t) => tagged("degenerateTotal", "total" -> double(t))
      case GridMismatch(l, r) =>
        tagged("gridMismatch", "left" -> gridId(l), "right" -> gridId(r))
      case GridIdentityConflict(id, l, r) =>
        tagged(
          "gridIdentityConflict",
          "id"    -> gridId(id),
          "left"  -> gridSpec(id, l),
          "right" -> gridSpec(id, r)
        )
      case EmptyCollection(op) => tagged("emptyCollection", "operation" -> Json.fromString(op))

  def readSurfaceError(json: Json): Either[CodecError, SurfaceError] =
    import SurfaceError.*
    def indexed(build: (Int, Double) => SurfaceError) = for
      i <- Wire.field[Int](json, "index")
      v <- readDouble(json, "value")
    yield build(i, v)
    kind(json).flatMap {
      case "lengthMismatch" =>
        for
          e <- Wire.field[Int](json, "expected")
          a <- Wire.field[Int](json, "actual")
        yield LengthMismatch(e, a)
      case "negativeWeight"  => indexed(NegativeWeight.apply)
      case "negativeValue"   => indexed(NegativeValue.apply)
      case "nonFiniteValue"  => indexed(NonFiniteValue.apply)
      case "degenerateTotal" => readDouble(json, "total").map(DegenerateTotal.apply)
      case "gridMismatch"    =>
        for
          l <- readGridId(json, "left")
          r <- readGridId(json, "right")
        yield GridMismatch(l, r)
      case "gridIdentityConflict" =>
        for
          id <- readGridId(json, "id")
          l  <- Wire.field[Json](json, "left").flatMap(readGridSpec)
          r  <- Wire.field[Json](json, "right").flatMap(readGridSpec)
        yield GridIdentityConflict(id, l._2, r._2)
      case "emptyCollection" =>
        Wire.field[String](json, "operation").map(EmptyCollection.apply)
      case other => Left(unknown(json, "surface error", other))
    }

  def estimateError(error: EstimateError): Json = error match
    case EstimateError.DegenerateAxisBandwidth(axis, s, c) =>
      tagged(
        "degenerateAxisBandwidth",
        "axis"     -> Json.fromString(axis.toString),
        "sigma"    -> double(s),
        "cellSize" -> double(c)
      )
    case EstimateError.KernelSupportOverflow(axis, s, c) =>
      tagged(
        "kernelSupportOverflow",
        "axis"     -> Json.fromString(axis.toString),
        "sigma"    -> double(s),
        "cellSize" -> double(c)
      )
    case EstimateError.FrameMismatch(m, g) =>
      tagged("frameMismatch", "measure" -> frameId(m), "grid" -> frameId(g))
    case EstimateError.NoMass                    => tagged("noMass")
    case EstimateError.DegenerateBandwidth(s, c) =>
      tagged("degenerateBandwidth", "sigma" -> double(s), "cellSize" -> double(c))
    case EstimateError.Surface(e) => tagged("surface", "error" -> surfaceError(e))

  def readEstimateError(json: Json): Either[CodecError, EstimateError] = kind(json).flatMap {
    case tag @ ("degenerateAxisBandwidth" | "kernelSupportOverflow") =>
      for
        name <- Wire.field[String](json, "axis")
        axis <- SmoothingAxis.values
          .find(_.toString == name)
          .toRight(CodecError.Field("axis", json, s"unknown smoothing axis $name"))
        s <- readDouble(json, "sigma")
        c <- readDouble(json, "cellSize")
      yield
        if tag == "degenerateAxisBandwidth" then
          EstimateError.DegenerateAxisBandwidth(axis, s, c)
        else EstimateError.KernelSupportOverflow(axis, s, c)
    case "frameMismatch" =>
      for
        m <- readFrameId(json, "measure")
        g <- readFrameId(json, "grid")
      yield EstimateError.FrameMismatch(m, g)
    case "noMass"              => Right(EstimateError.NoMass)
    case "degenerateBandwidth" =>
      for
        s <- readDouble(json, "sigma")
        c <- readDouble(json, "cellSize")
      yield EstimateError.DegenerateBandwidth(s, c)
    case "surface" =>
      Wire.field[Json](json, "error").flatMap(readSurfaceError).map(EstimateError.Surface.apply)
    case other => Left(unknown(json, "estimate error", other))
  }

  def comparisonValueError(error: ComparisonValueError): Json = error match
    case ComparisonValueError.NonFiniteMeasureDistance(v) =>
      tagged("nonFiniteMeasureDistance", "value" -> double(v))
    case ComparisonValueError.NegativeMeasureDistance(v) =>
      tagged("negativeMeasureDistance", "value" -> double(v))
    case ComparisonValueError.NonFiniteSimilarity(v) =>
      tagged("nonFiniteSimilarity", "value" -> double(v))
    case ComparisonValueError.InvalidUnitSimilarity(c, v) =>
      tagged("invalidUnitSimilarity", "component" -> Json.fromString(c), "value" -> double(v))

  def readComparisonValueError(json: Json): Either[CodecError, ComparisonValueError] =
    kind(json).flatMap {
      case "nonFiniteMeasureDistance" =>
        readDouble(json, "value").map(ComparisonValueError.NonFiniteMeasureDistance.apply)
      case "negativeMeasureDistance" =>
        readDouble(json, "value").map(ComparisonValueError.NegativeMeasureDistance.apply)
      case "nonFiniteSimilarity" =>
        readDouble(json, "value").map(ComparisonValueError.NonFiniteSimilarity.apply)
      case "invalidUnitSimilarity" =>
        for
          c <- Wire.field[String](json, "component")
          v <- readDouble(json, "value")
        yield ComparisonValueError.InvalidUnitSimilarity(c, v)
      case other => Left(unknown(json, "comparison value error", other))
    }

  val compareOperands: Vector[(CompareOperand, String)] = Vector(
    CompareOperand.Left     -> "left",
    CompareOperand.Right    -> "right",
    CompareOperand.Both     -> "both",
    CompareOperand.Model    -> "model",
    CompareOperand.Observed -> "observed"
  )

  def compareOperand(operand: CompareOperand): Json =
    Json.fromString(compareOperands.toMap.apply(operand))

  def readCompareOperand(json: Json, field: String): Either[CodecError, CompareOperand] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        compareOperands
          .collectFirst { case (o, n) if n == name => o }
          .toRight(CodecError.Field(field, json, s"unknown comparison operand $name"))
      )

  def compareError(error: CompareError): Json =
    import CompareError.*
    error match
      case Grids(e)            => tagged("grids", "error" -> surfaceError(e))
      case Frames(e)           => tagged("frames", "error" -> geometryError(e))
      case Estimation(e)       => tagged("estimation", "error" -> estimateError(e))
      case ConstantInput(m, o) =>
        tagged("constantInput", "measure" -> Json.fromString(m), "operand" -> compareOperand(o))
      case EmptyInput(m, o, t) =>
        tagged(
          "emptyInput",
          "measure" -> Json.fromString(m),
          "operand" -> compareOperand(o),
          "total"   -> double(t)
        )
      case ZeroNorm(m, l, r) =>
        tagged(
          "zeroNorm",
          "measure"   -> Json.fromString(m),
          "leftNorm"  -> double(l),
          "rightNorm" -> double(r)
        )
      case RelativeEntropySupport(m, i, l, r) =>
        tagged(
          "relativeEntropySupport",
          "measure"   -> Json.fromString(m),
          "cellIndex" -> Json.fromInt(i),
          "leftMass"  -> double(l),
          "rightMass" -> double(r)
        )
      case CostMatrixLimitExceeded(m, c, l) =>
        tagged(
          "costMatrixLimitExceeded",
          "measure" -> Json.fromString(m),
          "cells"   -> Json.fromInt(c),
          "limit"   -> Json.fromInt(l)
        )
      case WorkLimitExceeded(m, c, p, l) =>
        tagged(
          "workLimitExceeded",
          "measure" -> Json.fromString(m),
          "cells"   -> Json.fromInt(c),
          "pairs"   -> long(p),
          "limit"   -> long(l)
        )
      case InvalidSubstitutionCost(m, l, r, v) =>
        tagged(
          "invalidSubstitutionCost",
          "measure"    -> Json.fromString(m),
          "leftIndex"  -> Json.fromInt(l),
          "rightIndex" -> Json.fromInt(r),
          "value"      -> double(v)
        )
      case InvalidScore(m, e) =>
        tagged(
          "invalidScore",
          "measure" -> Json.fromString(m),
          "error"   -> comparisonValueError(e)
        )
      case TooShort(w, g, n) =>
        tagged(
          "tooShort",
          "what"   -> Json.fromString(w),
          "got"    -> Json.fromInt(g),
          "needed" -> Json.fromInt(n)
        )

  def readCompareError(json: Json): Either[CodecError, CompareError] =
    import CompareError.*
    def measure = Wire.field[String](json, "measure")
    kind(json).flatMap {
      case "grids" => Wire.field[Json](json, "error").flatMap(readSurfaceError).map(Grids.apply)
      case "frames" =>
        Wire.field[Json](json, "error").flatMap(readGeometryError).map(Frames.apply)
      case "estimation" =>
        Wire.field[Json](json, "error").flatMap(readEstimateError).map(Estimation.apply)
      case "constantInput" =>
        for
          m <- measure
          o <- readCompareOperand(json, "operand")
        yield ConstantInput(m, o)
      case "emptyInput" =>
        for
          m <- measure
          o <- readCompareOperand(json, "operand")
          t <- readDouble(json, "total")
        yield EmptyInput(m, o, t)
      case "zeroNorm" =>
        for
          m <- measure
          l <- readDouble(json, "leftNorm")
          r <- readDouble(json, "rightNorm")
        yield ZeroNorm(m, l, r)
      case "relativeEntropySupport" =>
        for
          m <- measure
          i <- Wire.field[Int](json, "cellIndex")
          l <- readDouble(json, "leftMass")
          r <- readDouble(json, "rightMass")
        yield RelativeEntropySupport(m, i, l, r)
      case "costMatrixLimitExceeded" =>
        for
          m <- measure
          c <- Wire.field[Int](json, "cells")
          l <- Wire.field[Int](json, "limit")
        yield CostMatrixLimitExceeded(m, c, l)
      case "workLimitExceeded" =>
        for
          m <- measure
          c <- Wire.field[Int](json, "cells")
          p <- readLong(json, "pairs")
          l <- readLong(json, "limit")
        yield WorkLimitExceeded(m, c, p, l)
      case "invalidSubstitutionCost" =>
        for
          m <- measure
          l <- Wire.field[Int](json, "leftIndex")
          r <- Wire.field[Int](json, "rightIndex")
          v <- readDouble(json, "value")
        yield InvalidSubstitutionCost(m, l, r, v)
      case "invalidScore" =>
        for
          m <- measure
          e <- Wire.field[Json](json, "error").flatMap(readComparisonValueError)
        yield InvalidScore(m, e)
      case "tooShort" =>
        for
          w <- Wire.field[String](json, "what")
          g <- Wire.field[Int](json, "got")
          n <- Wire.field[Int](json, "needed")
        yield TooShort(w, g, n)
      case other => Left(unknown(json, "comparison error", other))
    }

  def timeError(error: TimeError): Json =
    import TimeError.*
    error match
      case ReversedInterval(c, on, off) =>
        tagged(
          "reversedInterval",
          "clock"        -> clockId(c),
          "onsetMicros"  -> long(on),
          "offsetMicros" -> long(off)
        )
      case ReversedWindow(f, u) =>
        tagged("reversedWindow", "fromMicros" -> long(f), "untilMicros" -> long(u))
      case ClockMismatch(l, r) =>
        tagged("clockMismatch", "left" -> clockId(l), "right" -> clockId(r))
      case WrongSourceClock(e, a) =>
        tagged("wrongSourceClock", "expected" -> clockId(e), "actual" -> clockId(a))
      case NonPositiveRate(v)      => tagged("nonPositiveRate", "value" -> double(v))
      case NonFiniteDrift(f, t, d) =>
        tagged("nonFiniteDrift", "from" -> clockId(f), "to" -> clockId(t), "drift" -> double(d))
      case NonPositiveClockScale(f, t, d) =>
        tagged(
          "nonPositiveClockScale",
          "from"  -> clockId(f),
          "to"    -> clockId(t),
          "drift" -> double(d)
        )

  def readTimeError(json: Json): Either[CodecError, TimeError] =
    import TimeError.*
    def drift(build: (ClockId, ClockId, Double) => TimeError) = for
      f <- readClockId(json, "from")
      t <- readClockId(json, "to")
      d <- readDouble(json, "drift")
    yield build(f, t, d)
    kind(json).flatMap {
      case "reversedInterval" =>
        for
          c   <- readClockId(json, "clock")
          on  <- readLong(json, "onsetMicros")
          off <- readLong(json, "offsetMicros")
        yield ReversedInterval(c, on, off)
      case "reversedWindow" =>
        for
          f <- readLong(json, "fromMicros")
          u <- readLong(json, "untilMicros")
        yield ReversedWindow(f, u)
      case "clockMismatch" =>
        for
          l <- readClockId(json, "left")
          r <- readClockId(json, "right")
        yield ClockMismatch(l, r)
      case "wrongSourceClock" =>
        for
          e <- readClockId(json, "expected")
          a <- readClockId(json, "actual")
        yield WrongSourceClock(e, a)
      case "nonPositiveRate"       => readDouble(json, "value").map(NonPositiveRate.apply)
      case "nonFiniteDrift"        => drift(NonFiniteDrift.apply)
      case "nonPositiveClockScale" => drift(NonPositiveClockScale.apply)
      case other                   => Left(unknown(json, "time error", other))
    }

  def scoreMeanError(error: ScoreMeanError): Json = error match
    case ScoreMeanError.EmptyValues(o) => tagged("emptyValues", "operand" -> Json.fromString(o))
    case ScoreMeanError.NonFiniteValue(c, i, v) =>
      tagged(
        "nonFiniteValue",
        "component" -> Json.fromString(c),
        "index"     -> Json.fromInt(i),
        "value"     -> double(v)
      )
    case ScoreMeanError.NonFiniteMean(c, v) =>
      tagged("nonFiniteMean", "component" -> Json.fromString(c), "value" -> double(v))
    case ScoreMeanError.InvalidComparisonValue(c, e) =>
      tagged(
        "invalidComparisonValue",
        "component" -> Json.fromString(c),
        "error"     -> comparisonValueError(e)
      )

  def readScoreMeanError(json: Json): Either[CodecError, ScoreMeanError] = kind(json).flatMap {
    case "emptyValues" =>
      Wire.field[String](json, "operand").map(ScoreMeanError.EmptyValues.apply)
    case "nonFiniteValue" =>
      for
        c <- Wire.field[String](json, "component")
        i <- Wire.field[Int](json, "index")
        v <- readDouble(json, "value")
      yield ScoreMeanError.NonFiniteValue(c, i, v)
    case "nonFiniteMean" =>
      for
        c <- Wire.field[String](json, "component")
        v <- readDouble(json, "value")
      yield ScoreMeanError.NonFiniteMean(c, v)
    case "invalidComparisonValue" =>
      for
        c <- Wire.field[String](json, "component")
        e <- Wire.field[Json](json, "error").flatMap(readComparisonValueError)
      yield ScoreMeanError.InvalidComparisonValue(c, e)
    case other => Left(unknown(json, "score mean error", other))
  }

  def differenceError(error: DifferenceError): Json = error match
    case DifferenceError.NonFiniteOperands(c, m, k) =>
      tagged(
        "nonFiniteOperands",
        "component" -> Json.fromString(c),
        "matched"   -> double(m),
        "control"   -> double(k)
      )
    case DifferenceError.NonFiniteDifference(c, m, k) =>
      tagged(
        "nonFiniteDifference",
        "component" -> Json.fromString(c),
        "matched"   -> double(m),
        "control"   -> double(k)
      )

  def readDifferenceError(json: Json): Either[CodecError, DifferenceError] =
    def operands(build: (String, Double, Double) => DifferenceError) = for
      c <- Wire.field[String](json, "component")
      m <- readDouble(json, "matched")
      k <- readDouble(json, "control")
    yield build(c, m, k)
    kind(json).flatMap {
      case "nonFiniteOperands"   => operands(DifferenceError.NonFiniteOperands.apply)
      case "nonFiniteDifference" => operands(DifferenceError.NonFiniteDifference.apply)
      case other                 => Left(unknown(json, "difference error", other))
    }

  def reductionError[K](keys: VersionedCodec[K])(
      error: ReductionError[K]
  ): Either[CodecError, Json] =
    import ReductionError.*
    error match
      case NoSelectedScores(k) =>
        keys.encode(k).map(key => tagged("noSelectedScores", "key" -> key))
      case AmbiguousKey(k, indices) =>
        keys
          .encode(k)
          .map(key =>
            tagged(
              "ambiguousKey",
              "key"           -> key,
              "sourceIndices" -> Json.arr(indices.map(Json.fromInt)*)
            )
          )
      case FailedScores(k, s, f) =>
        keys
          .encode(k)
          .map(key =>
            tagged(
              "failedScores",
              "key"        -> key,
              "successful" -> Json.fromInt(s),
              "failed"     -> Json.fromInt(f)
            )
          )
      case InsufficientSuccessful(k, r, s, f) =>
        keys
          .encode(k)
          .map(key =>
            tagged(
              "insufficientSuccessful",
              "key"        -> key,
              "required"   -> Json.fromInt(r),
              "successful" -> Json.fromInt(s),
              "failed"     -> Json.fromInt(f)
            )
          )
      case MeanFailure(k, e) =>
        keys
          .encode(k)
          .map(key => tagged("meanFailure", "key" -> key, "error" -> scoreMeanError(e)))

  def readReductionError[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, ReductionError[K]] =
    import ReductionError.*
    def key = keyed(keys, json, "key")
    kind(json).flatMap {
      case "noSelectedScores" => key.map(NoSelectedScores.apply)
      case "ambiguousKey"     =>
        for
          k       <- key
          indices <- Wire.field[Vector[Int]](json, "sourceIndices")
        yield AmbiguousKey(k, indices)
      case "failedScores" =>
        for
          k <- key
          s <- Wire.field[Int](json, "successful")
          f <- Wire.field[Int](json, "failed")
        yield FailedScores(k, s, f)
      case "insufficientSuccessful" =>
        for
          k <- key
          r <- Wire.field[Int](json, "required")
          s <- Wire.field[Int](json, "successful")
          f <- Wire.field[Int](json, "failed")
        yield InsufficientSuccessful(k, r, s, f)
      case "meanFailure" =>
        for
          k <- key
          e <- Wire.field[Json](json, "error").flatMap(readScoreMeanError)
        yield MeanFailure(k, e)
      case other => Left(unknown(json, "reduction error", other))
    }

  /** Every failure family, the temporal one included: a temporal failure
    * carries its trial and the typed `TemporalStudyError` its occupancy
    * produced, which names the window and epoch it concerns.
    */
  def studyFailure[K](keys: VersionedCodec[K])(
      failure: StudyFailure[K]
  ): Either[CodecError, Json] = failure match
    case StudyFailure.Frame(k, e) =>
      keys.encode(k).map(key => tagged("frame", "key" -> key, "error" -> geometryError(e)))
    case StudyFailure.Occupancy(k, e) =>
      keys.encode(k).map(key => tagged("occupancy", "key" -> key, "error" -> surfaceError(e)))
    case StudyFailure.Estimation(k, e) =>
      keys.encode(k).map(key => tagged("estimation", "key" -> key, "error" -> estimateError(e)))
    case StudyFailure.Comparison(l, r, e) =>
      for
        left  <- keys.encode(l)
        right <- keys.encode(r)
      yield tagged("comparison", "left" -> left, "right" -> right, "error" -> compareError(e))
    case StudyFailure.Temporal(k, e) =>
      keys
        .encode(k)
        .map(key =>
          tagged("temporal", "key" -> key, "error" -> TemporalWire.temporalStudyError(e))
        )
    case StudyFailure.OffWindow(k, tally) =>
      keys
        .encode(k)
        .map(key => tagged("offWindow", "key" -> key, "tally" -> windowTally(tally)))

  def readStudyFailure[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, StudyFailure[K]] =
    def key   = keyed(keys, json, "key")
    def error = Wire.field[Json](json, "error")
    kind(json).flatMap {
      case "frame" =>
        for
          k <- key
          e <- error.flatMap(readGeometryError)
        yield StudyFailure.Frame(k, e)
      case "occupancy" =>
        for
          k <- key
          e <- error.flatMap(readSurfaceError)
        yield StudyFailure.Occupancy(k, e)
      case "estimation" =>
        for
          k <- key
          e <- error.flatMap(readEstimateError)
        yield StudyFailure.Estimation(k, e)
      case "comparison" =>
        for
          l <- keyed(keys, json, "left")
          r <- keyed(keys, json, "right")
          e <- error.flatMap(readCompareError)
        yield StudyFailure.Comparison(l, r, e)
      case "temporal" =>
        for
          k <- key
          e <- error.flatMap(TemporalWire.readTemporalStudyError)
        yield StudyFailure.Temporal(k, e)
      case "offWindow" =>
        for
          k <- key
          t <- Wire.field[Json](json, "tally").flatMap(readWindowTally)
        yield StudyFailure.OffWindow(k, t)
      case other => Left(unknown(json, "study failure", other))
    }

  /** A trial's window tally: counts and signed-microsecond duration strings. */
  def windowTally(tally: WindowTally): Json = Json.obj(
    "outsideScreen"         -> Json.fromInt(tally.outsideScreen),
    "outsideWindow"         -> Json.fromInt(tally.outsideWindow),
    "total"                 -> Json.fromInt(tally.total),
    "outsideScreenDuration" -> DomainWire.time(tally.outsideScreenDuration.toMicros),
    "outsideWindowDuration" -> DomainWire.time(tally.outsideWindowDuration.toMicros),
    "totalDuration"         -> DomainWire.time(tally.totalDuration.toMicros)
  )

  def readWindowTally(json: Json): Either[CodecError, WindowTally] = for
    screen         <- Wire.field[Int](json, "outsideScreen")
    window         <- Wire.field[Int](json, "outsideWindow")
    total          <- Wire.field[Int](json, "total")
    screenDuration <- DomainWire.micros(json, "outsideScreenDuration")
    windowDuration <- DomainWire.micros(json, "outsideWindowDuration")
    totalDuration  <- DomainWire.micros(json, "totalDuration")
    tally          <- WindowTally
      .of(
        screen,
        window,
        total,
        Span.micros(screenDuration),
        Span.micros(windowDuration),
        Span.micros(totalDuration)
      )
      .left
      .map(CodecError.Definition.apply)
  yield tally

  // ---- evaluation metadata ---------------------------------------------

  def measureScale(scale: MeasureScale): Json = scale match
    case MeasureScale.Correlation         => tagged("correlation")
    case MeasureScale.FisherZ             => tagged("fisherZ")
    case MeasureScale.UnboundedSimilarity => tagged("unboundedSimilarity")
    case MeasureScale.Probability         => tagged("probability")
    case MeasureScale.Bounded(lo, hi)     =>
      tagged("bounded", "lo" -> double(lo), "hi" -> double(hi))
    case MeasureScale.DistanceLike => tagged("distanceLike")

  def readMeasureScale(json: Json): Either[CodecError, MeasureScale] = kind(json).flatMap {
    case "correlation"         => Right(MeasureScale.Correlation)
    case "fisherZ"             => Right(MeasureScale.FisherZ)
    case "unboundedSimilarity" => Right(MeasureScale.UnboundedSimilarity)
    case "probability"         => Right(MeasureScale.Probability)
    case "bounded"             =>
      for
        lo <- readDouble(json, "lo")
        hi <- readDouble(json, "hi")
      yield MeasureScale.Bounded(lo, hi)
    case "distanceLike" => Right(MeasureScale.DistanceLike)
    case other          => Left(unknown(json, "measure scale", other))
  }

  def evaluationScale(scale: EvaluationScale): Json = scale match
    case EvaluationScale.Measure(m) => tagged("measure", "scale" -> measureScale(m))
    case EvaluationScale.Count      => tagged("count")
    case EvaluationScale.Duration   => tagged("duration")
    case EvaluationScale.Unitless   => tagged("unitless")

  def readEvaluationScale(json: Json): Either[CodecError, EvaluationScale] =
    kind(json).flatMap {
      case "measure" =>
        Wire
          .field[Json](json, "scale")
          .flatMap(readMeasureScale)
          .map(EvaluationScale.Measure.apply)
      case "count"    => Right(EvaluationScale.Count)
      case "duration" => Right(EvaluationScale.Duration)
      case "unitless" => Right(EvaluationScale.Unitless)
      case other      => Left(unknown(json, "evaluation scale", other))
    }

  def evaluationTime(time: EvaluationTime): Json = time match
    case EvaluationTime.OrderFree            => tagged("orderFree")
    case EvaluationTime.Ordered              => tagged("ordered")
    case EvaluationTime.RelativeMicroseconds => tagged("relativeMicroseconds")
    case EvaluationTime.SharedClock(c)       => tagged("sharedClock", "clock" -> clockId(c))

  def readEvaluationTime(json: Json): Either[CodecError, EvaluationTime] = kind(json).flatMap {
    case "orderFree"            => Right(EvaluationTime.OrderFree)
    case "ordered"              => Right(EvaluationTime.Ordered)
    case "relativeMicroseconds" => Right(EvaluationTime.RelativeMicroseconds)
    case "sharedClock" => readClockId(json, "clock").map(EvaluationTime.SharedClock.apply)
    case other         => Left(unknown(json, "evaluation time", other))
  }

  /** Geometry is written from the specification's own structural metadata,
    * so a result whose every estimate failed still records the method's domain.
    */
  def evaluationGeometry[U <: Unit2D](
      geometry: EvaluationGeometry
  )(using unit: UnitLabel[U]): Either[CodecError, Json] =
    (geometry.unit, geometry.frame, geometry.grid) match
      case (None, None, None) => Right(tagged("independent"))
      case (Some((symbol, _)), Some((fid, fs)), None) if symbol == unit.symbol =>
        Right(tagged("frame", "unit" -> Json.fromString(symbol), "frame" -> frameSpec(fid, fs)))
      case (Some((symbol, _)), Some((fid, fs)), Some((gid, gs))) if symbol == unit.symbol =>
        Right(
          tagged(
            "grid",
            "unit"  -> Json.fromString(symbol),
            "frame" -> frameSpec(fid, fs),
            "grid"  -> Json.obj(
              "id" -> gridId(gid),
              "nx" -> Json.fromInt(gs.nx),
              "ny" -> Json.fromInt(gs.ny)
            )
          )
        )
      case (u, _, _) =>
        Left(
          CodecError.Unsupported(
            "geometry",
            s"evaluation geometry in unit ${u.map(_._1)} cannot be written for ${unit.symbol}"
          )
        )

  def readEvaluationGeometry[U <: Unit2D](
      json: Json
  )(using unit: UnitLabel[U]): Either[CodecError, EvaluationGeometry] =
    def frame = for
      symbol <- Wire.field[String](json, "unit")
      _      <- Either.cond(
        symbol == unit.symbol,
        (),
        CodecError.Field("unit", json, s"expected ${unit.symbol}, got $symbol")
      )
      pixel  <- Wire.field[Json](json, "frame").flatMap(readFrameSpec)
      bounds <- Bounds
        .of[U](pixel.spec.xMin, pixel.spec.yMin, pixel.spec.xMax, pixel.spec.yMax)
        .left
        .map(e => CodecError.Field("frame", json, e.message))
    yield Frame.of(pixel.id, bounds, pixel.yAxis)
    kind(json).flatMap {
      case "independent" => Right(EvaluationGeometry.Independent)
      case "frame"       => frame.map(f => EvaluationGeometry.inFrame(f))
      case "grid"        =>
        for
          f    <- frame
          g    <- Wire.field[Json](json, "grid")
          id   <- Wire.field[String](g, "id")
          nx   <- Wire.field[Int](g, "nx")
          ny   <- Wire.field[Int](g, "ny")
          grid <- Grid
            .of(GridId(id), f, nx, ny)
            .left
            .map(e => CodecError.Field("grid", json, e.message))
        yield EvaluationGeometry.onGrid(grid)
      case other => Left(unknown(json, "evaluation geometry", other))
    }

  def evaluationSpec[U <: Unit2D: UnitLabel](spec: EvaluationSpec): Either[CodecError, Json] =
    evaluationGeometry[U](spec.geometry).map(geometry =>
      Json.obj(
        "method"     -> Json.fromString(spec.method),
        "revision"   -> Json.fromString(spec.revision),
        "parameters" -> params(spec.parameters),
        "components" -> strings(spec.components),
        "geometry"   -> geometry,
        "time"       -> evaluationTime(spec.time)
      )
    )

  def readEvaluationSpec[U <: Unit2D: UnitLabel](
      json: Json
  ): Either[CodecError, EvaluationSpec] =
    for
      method     <- Wire.field[String](json, "method")
      revision   <- Wire.field[String](json, "revision")
      parameters <- readParams(json, "parameters")
      components <- Wire.field[Vector[String]](json, "components")
      geometry   <- Wire.field[Json](json, "geometry").flatMap(readEvaluationGeometry[U])
      time       <- Wire.field[Json](json, "time").flatMap(readEvaluationTime)
      spec       <- EvaluationSpec
        .of(method, revision, parameters, components, geometry, time)
        .left
        .map(e => CodecError.Field("specification", json, e.message))
    yield spec

  def evaluationInfo[U <: Unit2D: UnitLabel](info: EvaluationInfo): Either[CodecError, Json] =
    info.specification
      .traverse(evaluationSpec[U])
      .map(spec =>
        Json.obj(
          "name"          -> Json.fromString(info.name),
          "scale"         -> evaluationScale(info.scale),
          "specification" -> spec.getOrElse(Json.Null)
        )
      )

  def readEvaluationInfo[U <: Unit2D: UnitLabel](
      json: Json
  ): Either[CodecError, EvaluationInfo] =
    for
      name  <- Wire.field[String](json, "name")
      scale <- Wire.field[Json](json, "scale").flatMap(readEvaluationScale)
      spec  <- optional(json, "specification")(readEvaluationSpec[U])
    yield EvaluationInfo(name, scale, spec)

  val orientations: Vector[(ReductionOrientation, String)] = Vector(
    ReductionOrientation.ByLeft            -> "byLeft",
    ReductionOrientation.ByRight           -> "byRight",
    ReductionOrientation.EdgesOnce         -> "edgesOnce",
    ReductionOrientation.MirroredEndpoints -> "mirroredEndpoints"
  )

  def orientation(value: ReductionOrientation): Json =
    Json.fromString(orientations.toMap.apply(value))

  def readOrientation(json: Json, field: String): Either[CodecError, ReductionOrientation] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        orientations
          .collectFirst { case (o, n) if n == name => o }
          .toRight(CodecError.Field(field, json, s"unknown reduction orientation $name"))
      )

  val contrastOperands: Vector[(ContrastOperand, String)] =
    Vector(ContrastOperand.Matched -> "matched", ContrastOperand.Control -> "control")

  def contrastOperand(value: ContrastOperand): Json =
    Json.fromString(contrastOperands.toMap.apply(value))

  def readContrastOperand(json: Json): Either[CodecError, ContrastOperand] =
    json.asString
      .flatMap(name => contrastOperands.collectFirst { case (o, n) if n == name => o })
      .toRight(CodecError.Field("operand", json, "unknown contrast operand"))

  def contrastCompatibilityError[U <: Unit2D: UnitLabel](
      error: ContrastCompatibilityError
  ): Either[CodecError, Json] =
    import ContrastCompatibilityError.*
    error match
      case Orientation(m, c) =>
        Right(tagged("orientation", "matched" -> orientation(m), "control" -> orientation(c)))
      case Policy(m, c) =>
        Right(
          tagged("policy", "matched" -> StudyWire.policy(m), "control" -> StudyWire.policy(c))
        )
      case Scale(m, c) =>
        Right(tagged("scale", "matched" -> evaluationScale(m), "control" -> evaluationScale(c)))
      case MissingSpecification(o, info) =>
        evaluationInfo[U](info).map(i =>
          tagged("missingSpecification", "operand" -> contrastOperand(o), "evaluation" -> i)
        )
      case Method(m, c) =>
        for
          a <- evaluationSpec[U](m)
          b <- evaluationSpec[U](c)
        yield tagged("method", "matched" -> a, "control" -> b)
      case Components(o, d, r) =>
        Right(
          tagged(
            "components",
            "operand"  -> contrastOperand(o),
            "declared" -> strings(d),
            "required" -> strings(r)
          )
        )
      case SpatialConvention(m, c) =>
        for
          a <- evaluationGeometry[U](m)
          b <- evaluationGeometry[U](c)
        yield tagged("spatialConvention", "matched" -> a, "control" -> b)
      case Frames(e)  => Right(tagged("frames", "error" -> geometryError(e)))
      case Grids(e)   => Right(tagged("grids", "error" -> surfaceError(e)))
      case Time(m, c) =>
        Right(tagged("time", "matched" -> evaluationTime(m), "control" -> evaluationTime(c)))
      case Clocks(e) => Right(tagged("clocks", "error" -> timeError(e)))

  def readContrastCompatibilityError[U <: Unit2D: UnitLabel](
      json: Json
  ): Either[CodecError, ContrastCompatibilityError] =
    import ContrastCompatibilityError.*
    def both[A](read: Json => Either[CodecError, A]) = for
      m <- Wire.field[Json](json, "matched").flatMap(read)
      c <- Wire.field[Json](json, "control").flatMap(read)
    yield (m, c)
    kind(json).flatMap {
      case "orientation" =>
        for
          m <- readOrientation(json, "matched")
          c <- readOrientation(json, "control")
        yield Orientation(m, c)
      case "policy"               => both(StudyWire.readPolicy).map(Policy.apply.tupled)
      case "scale"                => both(readEvaluationScale).map(Scale.apply.tupled)
      case "missingSpecification" =>
        for
          o <- Wire.field[Json](json, "operand").flatMap(readContrastOperand)
          i <- Wire.field[Json](json, "evaluation").flatMap(readEvaluationInfo[U])
        yield MissingSpecification(o, i)
      case "method"     => both(readEvaluationSpec[U]).map(Method.apply.tupled)
      case "components" =>
        for
          o <- Wire.field[Json](json, "operand").flatMap(readContrastOperand)
          d <- Wire.field[Vector[String]](json, "declared")
          r <- Wire.field[Vector[String]](json, "required")
        yield Components(o, d, r)
      case "spatialConvention" =>
        both(readEvaluationGeometry[U]).map(SpatialConvention.apply.tupled)
      case "frames" =>
        Wire.field[Json](json, "error").flatMap(readGeometryError).map(Frames.apply)
      case "grids" => Wire.field[Json](json, "error").flatMap(readSurfaceError).map(Grids.apply)
      case "time"  => both(readEvaluationTime).map(Time.apply.tupled)
      case "clocks" => Wire.field[Json](json, "error").flatMap(readTimeError).map(Clocks.apply)
      case other    => Left(unknown(json, "contrast compatibility error", other))
    }

  def contrastError[K, U <: Unit2D: UnitLabel](keys: VersionedCodec[K])(
      error: ContrastError[K]
  ): Either[CodecError, Json] = error match
    case ContrastError.Incompatible(issues) =>
      issues.toVector
        .traverse(contrastCompatibilityError[U])
        .map(list => tagged("incompatible", "issues" -> Json.arr(list*)))
    case ContrastError.EmptyDomain(m, c) =>
      Right(
        tagged(
          "emptyDomain",
          "matchedKeys" -> Json.fromInt(m),
          "controlKeys" -> Json.fromInt(c)
        )
      )
    case ContrastError.IndistinguishableOrdering(a, b) =>
      for
        first  <- keys.encode(a)
        second <- keys.encode(b)
      yield tagged("indistinguishableOrdering", "first" -> first, "second" -> second)

  def readContrastError[K, U <: Unit2D: UnitLabel](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, ContrastError[K]] = kind(json).flatMap {
    case "incompatible" =>
      for
        entries <- Wire.field[Vector[Json]](json, "issues")
        issues  <- entries.zipWithIndex.traverse { case (entry, index) =>
          readContrastCompatibilityError[U](entry).left.map(Wire.at(s"issues[$index]"))
        }
        result <- NonEmptyVector
          .fromVector(issues)
          .toRight(
            CodecError.Field("issues", json, "an incompatible contrast names its issues")
          )
      yield ContrastError.Incompatible(result)
    case "emptyDomain" =>
      for
        m <- Wire.field[Int](json, "matchedKeys")
        c <- Wire.field[Int](json, "controlKeys")
      yield ContrastError.EmptyDomain(m, c)
    case "indistinguishableOrdering" =>
      for
        a <- keyed(keys, json, "first")
        b <- keyed(keys, json, "second")
      yield ContrastError.IndistinguishableOrdering(a, b)
    case other => Left(unknown(json, "contrast error", other))
  }

  def contrastRowError[K](keys: VersionedCodec[K])(
      error: ContrastRowError[K]
  ): Either[CodecError, Json] = error match
    case ContrastRowError.MissingOperands(k, missing) =>
      keys
        .encode(k)
        .map(key =>
          tagged(
            "missingOperands",
            "key"     -> key,
            "missing" -> Json.arr(missing.map(contrastOperand)*)
          )
        )
    case ContrastRowError.ReductionFailures(k, m, c) =>
      for
        key     <- keys.encode(k)
        matched <- m.traverse(reductionError(keys))
        control <- c.traverse(reductionError(keys))
      yield tagged(
        "reductionFailures",
        "key"     -> key,
        "matched" -> matched.getOrElse(Json.Null),
        "control" -> control.getOrElse(Json.Null)
      )
    case ContrastRowError.Arithmetic(k, e) =>
      keys
        .encode(k)
        .map(key => tagged("arithmetic", "key" -> key, "error" -> differenceError(e)))

  def readContrastRowError[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, ContrastRowError[K]] = kind(json).flatMap {
    case "missingOperands" =>
      for
        k       <- keyed(keys, json, "key")
        entries <- Wire.field[Vector[Json]](json, "missing")
        missing <- entries.traverse(readContrastOperand)
      yield ContrastRowError.MissingOperands(k, missing)
    case "reductionFailures" =>
      for
        k <- keyed(keys, json, "key")
        m <- optional(json, "matched")(readReductionError(keys))
        c <- optional(json, "control")(readReductionError(keys))
      yield ContrastRowError.ReductionFailures(k, m, c)
    case "arithmetic" =>
      for
        k <- keyed(keys, json, "key")
        e <- Wire.field[Json](json, "error").flatMap(readDifferenceError)
      yield ContrastRowError.Arithmetic(k, e)
    case other => Left(unknown(json, "contrast row error", other))
  }

  // ---- pairing ----------------------------------------------------------

  def selection(value: Selection): Json = value match
    case Selection.All                          => tagged("all")
    case Selection.BottomK(cap, seed, sampleId) =>
      tagged(
        "bottomK",
        "cap"      -> Json.fromInt(cap.value),
        "seed"     -> long(seed.value),
        "sampleId" -> Json.fromString(sampleId.value)
      )

  def readSelection(json: Json): Either[CodecError, Selection] = kind(json).flatMap {
    case "all"     => Right(Selection.All)
    case "bottomK" =>
      for
        cap <- Wire
          .field[Int](json, "cap")
          .flatMap(n => PairLimit.of(n).left.map(e => CodecError.Field("cap", json, e.message)))
        seed     <- readLong(json, "seed")
        sampleId <- Wire.field[String](json, "sampleId")
      yield Selection.BottomK(cap, Seed(seed), SampleId(sampleId))
    case other => Left(unknown(json, "selection", other))
  }

  val selfPolicies: Vector[(SelfPolicy, String)] =
    Vector(SelfPolicy.Exclude -> "exclude", SelfPolicy.Include -> "include")

  def readSelfPolicy(json: Json, field: String): Either[CodecError, SelfPolicy] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        selfPolicies
          .collectFirst { case (p, n) if n == name => p }
          .toRight(CodecError.Field(field, json, s"unknown self policy $name"))
      )

  def pairSpace(space: PairSpace): Json = space match
    case PairSpace.BetweenDirected(relation, sel) =>
      tagged(
        "betweenDirected",
        "relation"  -> Json.fromString(relation),
        "selection" -> selection(sel)
      )
    case PairSpace.WithinDirected(relation, self, sel) =>
      tagged(
        "withinDirected",
        "relation"  -> Json.fromString(relation),
        "self"      -> Json.fromString(selfPolicies.toMap.apply(self)),
        "selection" -> selection(sel)
      )
    case PairSpace.WithinUndirected(relation, self) =>
      tagged(
        "withinUndirected",
        "relation" -> Json.fromString(relation),
        "self"     -> Json.fromString(selfPolicies.toMap.apply(self))
      )

  def readPairSpace(json: Json): Either[CodecError, PairSpace] =
    def relation = Wire.field[String](json, "relation")
    def selected = Wire.field[Json](json, "selection").flatMap(readSelection)
    kind(json).flatMap {
      case "betweenDirected" =>
        for
          r <- relation
          s <- selected
        yield PairSpace.BetweenDirected(r, s)
      case "withinDirected" =>
        for
          r    <- relation
          self <- readSelfPolicy(json, "self")
          s    <- selected
        yield PairSpace.WithinDirected(r, self, s)
      case "withinUndirected" =>
        for
          r    <- relation
          self <- readSelfPolicy(json, "self")
        yield PairSpace.WithinUndirected(r, self)
      case other => Left(unknown(json, "pair space", other))
    }

  def ambiguity[K](keys: VersionedCodec[K])(
      value: PairingAmbiguity[K, K]
  ): Either[CodecError, Json] = value match
    case PairingAmbiguity.DuplicateLeft(k, indices) =>
      keys
        .encode(k)
        .map(key =>
          tagged(
            "duplicateLeft",
            "key"     -> key,
            "indices" -> Json.arr(indices.map(Json.fromInt)*)
          )
        )
    case PairingAmbiguity.DuplicateRight(k, indices) =>
      keys
        .encode(k)
        .map(key =>
          tagged(
            "duplicateRight",
            "key"     -> key,
            "indices" -> Json.arr(indices.map(Json.fromInt)*)
          )
        )

  def readAmbiguity[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, PairingAmbiguity[K, K]] =
    for
      tag     <- kind(json)
      k       <- keyed(keys, json, "key")
      indices <- Wire.field[Vector[Int]](json, "indices")
      result  <- tag match
        case "duplicateLeft"  => Right(PairingAmbiguity.DuplicateLeft[K, K](k, indices))
        case "duplicateRight" => Right(PairingAmbiguity.DuplicateRight[K, K](k, indices))
        case other            => Left(unknown(json, "pairing ambiguity", other))
    yield result

  def pairingReport[K](keys: VersionedCodec[K])(
      report: PairingReport[K, K]
  ): Either[CodecError, Json] =
    for
      left      <- report.unmatchedLeft.traverse(keys.encode)
      right     <- report.unmatchedRight.traverse(keys.encode)
      ambiguous <- report.ambiguous.traverse(ambiguity(keys))
    yield Json.obj(
      "pairSpace"         -> pairSpace(report.pairSpace),
      "eligiblePairCount" -> long(report.eligiblePairCount),
      "selectedPairCount" -> Json.fromInt(report.selectedPairCount),
      "unmatchedLeft"     -> Json.arr(left*),
      "unmatchedRight"    -> Json.arr(right*),
      "ambiguous"         -> Json.arr(ambiguous*)
    )

  def readPairingReport[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, PairingReport[K, K]] =
    for
      space     <- Wire.field[Json](json, "pairSpace").flatMap(readPairSpace)
      eligible  <- readLong(json, "eligiblePairCount")
      selected  <- Wire.field[Int](json, "selectedPairCount")
      left      <- keyVector(keys, json, "unmatchedLeft")
      right     <- keyVector(keys, json, "unmatchedRight")
      entries   <- Wire.field[Vector[Json]](json, "ambiguous")
      ambiguous <- entries.zipWithIndex.traverse { case (entry, index) =>
        readAmbiguity(keys)(entry).left.map(Wire.at(s"ambiguous[$index]"))
      }
      report <- PairingReport
        .reconstruct(space, eligible, selected, left, right, ambiguous)
        .left
        .map(e => CodecError.Reconstruction(e))
    yield report

  def reductionReport[K](keys: VersionedCodec[K])(
      report: ReductionReport[K]
  ): Either[CodecError, Json] =
    report.failedKeys
      .traverse(keys.encode)
      .map(failed =>
        Json.obj(
          "orientation"         -> orientation(report.orientation),
          "policy"              -> StudyWire.policy(report.policy),
          "eligiblePairCount"   -> long(report.eligiblePairCount),
          "selectedPairCount"   -> Json.fromInt(report.selectedPairCount),
          "successfulPairCount" -> Json.fromInt(report.successfulPairCount),
          "failedPairCount"     -> Json.fromInt(report.failedPairCount),
          "contributionCount"   -> Json.fromInt(report.contributionCount),
          "reducedKeyCount"     -> Json.fromInt(report.reducedKeyCount),
          "failedKeys"          -> Json.arr(failed*)
        )
      )

  def readReductionReport[K](keys: VersionedCodec[K])(
      json: Json
  ): Either[CodecError, ReductionReport[K]] =
    for
      orientation   <- readOrientation(json, "orientation")
      policy        <- Wire.field[Json](json, "policy").flatMap(StudyWire.readPolicy)
      eligible      <- readLong(json, "eligiblePairCount")
      selected      <- Wire.field[Int](json, "selectedPairCount")
      successful    <- Wire.field[Int](json, "successfulPairCount")
      failed        <- Wire.field[Int](json, "failedPairCount")
      contributions <- Wire.field[Int](json, "contributionCount")
      reduced       <- Wire.field[Int](json, "reducedKeyCount")
      failedKeys    <- keyVector(keys, json, "failedKeys")
      report        <- ReductionReport
        .reconstruct(
          orientation,
          policy,
          eligible,
          selected,
          successful,
          failed,
          contributions,
          reduced,
          failedKeys
        )
        .left
        .map(e => CodecError.Reconstruction(e))
    yield report
