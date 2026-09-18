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

import io.circe.Json

/** The element type of a packed payload. `UInt8` values are carried as
  * `Byte` and read unsigned (`b & 0xff`).
  */
enum ElementKind(val wire: String, val width: Int) derives CanEqual:
  case UInt8   extends ElementKind("uint8", 1)
  case Int32   extends ElementKind("int32", 4)
  case Int64   extends ElementKind("int64", 8)
  case Float64 extends ElementKind("float64", 8)

/** How a multi-dimensional payload is laid out: `RowMajor` varies the last
  * axis fastest, `ColumnMajor` the first. A one-dimensional payload is the
  * same sequence either way.
  */
enum ArrayOrder(val wire: String) derives CanEqual:
  case RowMajor    extends ArrayOrder("row-major")
  case ColumnMajor extends ArrayOrder("column-major")

/** The byte order of every multi-byte element. Pinned: only little-endian is
  * written or read, and it is declared on the wire so that a payload never
  * relies on the platform's order.
  */
enum ByteOrder(val wire: String) derives CanEqual:
  case LittleEndian extends ByteOrder("little-endian")

/** A typed payload's declared shape: element kind, extents, storage order and
  * byte order. The byte length is `product(shape) * width` and is bounded by
  * [[PayloadLayout.maximumBytes]], so a layout never describes an array that
  * cannot be allocated on either platform.
  */
final case class PayloadLayout private (
    element: ElementKind,
    shape: Vector[Int],
    order: ArrayOrder,
    byteOrder: ByteOrder
) derives CanEqual:
  /** The number of elements. */
  def count: Int = shape.foldLeft(1)(_ * _)

  /** The exact byte length of a payload with this layout. */
  def byteLength: Int = count * element.width

object PayloadLayout:
  /** The largest payload, in bytes: the largest array JVM runtimes allocate. */
  val maximumBytes: Int = Int.MaxValue - 8

  def of(
      element: ElementKind,
      shape: Vector[Int],
      order: ArrayOrder
  ): Either[PayloadError, PayloadLayout] =
    if shape.isEmpty then Left(PayloadError.EmptyShape)
    else
      shape.zipWithIndex.collectFirst {
        case (extent, axis) if extent < 0 => PayloadError.NegativeExtent(axis, extent)
      } match
        case Some(error) => Left(error)
        case None        =>
          val bytes = shape.foldLeft(BigInt(element.width))(_ * _)
          Either.cond(
            bytes <= maximumBytes,
            new PayloadLayout(element, shape, order, ByteOrder.LittleEndian),
            PayloadError.TooLarge(element, shape, maximumBytes)
          )

  /** A one-dimensional column of `count` elements. */
  def column(element: ElementKind, count: Int): Either[PayloadError, PayloadLayout] =
    of(element, Vector(count), ArrayOrder.RowMajor)

  private[codec] def json(layout: PayloadLayout): Json = Json.obj(
    "element"   -> Json.fromString(layout.element.wire),
    "shape"     -> Json.arr(layout.shape.map(Json.fromInt)*),
    "order"     -> Json.fromString(layout.order.wire),
    "byteOrder" -> Json.fromString(layout.byteOrder.wire)
  )

  private[codec] def read(json: Json): Either[CodecError, PayloadLayout] = for
    elementName <- Wire.field[String](json, "element")
    element     <- ElementKind.values
      .find(_.wire == elementName)
      .toRight(CodecError.Field("element", json, s"unknown element kind '$elementName'"))
    shape     <- Wire.field[Vector[Int]](json, "shape")
    orderName <- Wire.field[String](json, "order")
    order     <- ArrayOrder.values
      .find(_.wire == orderName)
      .toRight(CodecError.Field("order", json, s"unknown array order '$orderName'"))
    byteOrderName <- Wire.field[String](json, "byteOrder")
    _             <- Either.cond(
      byteOrderName == ByteOrder.LittleEndian.wire,
      (),
      CodecError.Field(
        "byteOrder",
        json,
        s"only little-endian payloads are supported, found '$byteOrderName'"
      )
    )
    layout <- of(element, shape, order).left.map(CodecError.Payload("layout", _))
  yield layout

/** A typed reference to a separately stored payload: the SHA-256 of its exact
  * bytes and its layout. The reference is content-addressed, so the payload
  * is found by digest rather than by a storage location.
  */
final case class PayloadRef(sha256: ByteDigest, layout: PayloadLayout) derives CanEqual:
  def length: Int = layout.byteLength

object PayloadRef:
  private[codec] def json(ref: PayloadRef): Json = Json.obj(
    "sha256" -> Json.fromString(ref.sha256.hex),
    "layout" -> PayloadLayout.json(ref.layout)
  )

  private[codec] def read(json: Json): Either[CodecError, PayloadRef] = for
    hex    <- Wire.field[String](json, "sha256")
    digest <- ByteDigest.parse(hex).left.map(e => CodecError.Field("sha256", json, e.message))
    layout <- Wire.field[Json](json, "layout").flatMap(PayloadLayout.read)
  yield PayloadRef(digest, layout)

/** Payload bytes whose length and SHA-256 were checked against a reference.
  *
  * Ownership: the bytes are a private copy made when the payload was verified
  * (or produced when it was packed), so mutating the array a caller supplied
  * cannot change a payload after it was accepted.
  */
final class VerifiedPayload private (val ref: PayloadRef, private val data: IArray[Byte]):
  def bytes: IArray[Byte] = data

object VerifiedPayload:
  /** Copy the bytes, then check their length and digest against `ref`. */
  def verify(ref: PayloadRef, bytes: IArray[Byte]): Either[PayloadError, VerifiedPayload] =
    if bytes.length != ref.length then Left(PayloadError.Length(ref.length, bytes.length))
    else
      val copy   = Bytes.copy(bytes)
      val actual = ByteDigest.sha256(copy)
      Either.cond(
        actual == ref.sha256,
        new VerifiedPayload(ref, copy),
        PayloadError.Digest(ref.sha256, actual)
      )

  /** Bytes the caller already owns and verified; no further copy is made. */
  private[codec] def trusted(ref: PayloadRef, owned: IArray[Byte]): VerifiedPayload =
    new VerifiedPayload(ref, owned)

/** Why a payload could not be laid out, packed, verified or unpacked. */
enum PayloadError derives CanEqual:
  case EmptyShape
  case NegativeExtent(axis: Int, extent: Int)
  case TooLarge(element: ElementKind, shape: Vector[Int], maximumBytes: Int)
  case Count(layout: PayloadLayout, values: Int)
  case Element(declared: ElementKind, requested: ElementKind)
  case Length(declared: Int, actual: Int)
  case Digest(declared: ByteDigest, actual: ByteDigest)
  case NotANumber(index: Int)

  def message: String = this match
    case EmptyShape                        => "A payload layout needs at least one axis."
    case NegativeExtent(axis, extent)      => s"Payload axis $axis has negative extent $extent."
    case TooLarge(element, shape, maximum) =>
      s"A ${element.wire} payload of shape $shape exceeds the $maximum-byte payload bound."
    case Count(layout, values) =>
      s"Layout ${layout.element.wire}${layout.shape} holds ${layout.count} elements, supplied $values."
    case Element(declared, requested) =>
      s"Payload holds ${declared.wire} elements; ${requested.wire} were requested."
    case Length(declared, actual) =>
      s"Payload declares $declared bytes but has $actual."
    case Digest(declared, actual) =>
      s"Payload declares SHA-256 ${declared.hex} but its bytes digest to ${actual.hex}."
    case NotANumber(index) =>
      s"Float64 element $index is NaN; payloads carry no NaN because its bits are not portable."

/** A little-endian element type. Reads assemble every value from its bytes
  * with integer shifts, so a decoded payload is exact on the JVM and Scala.js:
  * 64-bit integers keep all 64 bits, and doubles come from their IEEE bits
  * without any platform number formatting.
  */
sealed trait PackedElement[A]:
  def kind: ElementKind
  private[codec] def write(values: IArray[A], out: Array[Byte]): Either[PayloadError, Unit]
  private[codec] def read(bytes: IArray[Byte], count: Int): Either[PayloadError, IArray[A]]

object PackedElement:
  given uint8: PackedElement[Byte] with
    def kind                                                         = ElementKind.UInt8
    private[codec] def write(values: IArray[Byte], out: Array[Byte]) =
      var i = 0
      while i < values.length do
        out(i) = values(i)
        i += 1
      Right(())
    private[codec] def read(bytes: IArray[Byte], count: Int) = Right(Bytes.copy(bytes))

  given int32: PackedElement[Int] with
    def kind                                                        = ElementKind.Int32
    private[codec] def write(values: IArray[Int], out: Array[Byte]) =
      var i = 0
      while i < values.length do
        Bytes.putInt(out, i * 4, values(i))
        i += 1
      Right(())
    private[codec] def read(bytes: IArray[Byte], count: Int) =
      Right(IArray.tabulate(count)(i => Bytes.getInt(bytes, i * 4)))

  given int64: PackedElement[Long] with
    def kind                                                         = ElementKind.Int64
    private[codec] def write(values: IArray[Long], out: Array[Byte]) =
      var i = 0
      while i < values.length do
        Bytes.putLong(out, i * 8, values(i))
        i += 1
      Right(())
    private[codec] def read(bytes: IArray[Byte], count: Int) =
      Right(IArray.tabulate(count)(i => Bytes.getLong(bytes, i * 8)))

  /** Every double except NaN, including signed zeros, subnormals and
    * infinities, round-trips through its exact IEEE 754 bits.
    */
  given float64: PackedElement[Double] with
    def kind                                                           = ElementKind.Float64
    private[codec] def write(values: IArray[Double], out: Array[Byte]) =
      values.indexWhere(_.isNaN) match
        case -1 =>
          var i = 0
          while i < values.length do
            Bytes.putLong(out, i * 8, java.lang.Double.doubleToLongBits(values(i)))
            i += 1
          Right(())
        case index => Left(PayloadError.NotANumber(index))
    private[codec] def read(bytes: IArray[Byte], count: Int) =
      val out   = Array.ofDim[Double](count)
      var i     = 0
      var error = -1
      while i < count && error < 0 do
        val bits = Bytes.getLong(bytes, i * 8)
        if (bits & 0x7ff0000000000000L) == 0x7ff0000000000000L &&
          (bits & 0x000fffffffffffffL) != 0L
        then error = i
        else out(i) = java.lang.Double.longBitsToDouble(bits)
        i += 1
      if error >= 0 then Left(PayloadError.NotANumber(error))
      else Right(IArray.unsafeFromArray(out))

/** Pack typed values into the pinned little-endian layout and unpack them. */
object PackedArrays:
  /** Pack `values` in the layout's storage order; the result is verified by
    * construction, since its digest is computed from the bytes it holds.
    */
  def pack[A](layout: PayloadLayout, values: IArray[A])(using
      element: PackedElement[A]
  ): Either[PayloadError, VerifiedPayload] =
    for
      _ <- Either.cond(
        layout.element == element.kind,
        (),
        PayloadError.Element(layout.element, element.kind)
      )
      _ <- Either.cond(
        values.length == layout.count,
        (),
        PayloadError.Count(layout, values.length)
      )
      out = Array.ofDim[Byte](layout.byteLength)
      _ <- element.write(values, out)
      bytes = IArray.unsafeFromArray(out)
    yield VerifiedPayload.trusted(PayloadRef(ByteDigest.sha256(bytes), layout), bytes)

  /** The payload's elements in storage order, exactly as packed. */
  def unpack[A](payload: VerifiedPayload)(using
      element: PackedElement[A]
  ): Either[PayloadError, IArray[A]] =
    val layout = payload.ref.layout
    for
      _ <- Either.cond(
        layout.element == element.kind,
        (),
        PayloadError.Element(layout.element, element.kind)
      )
      _ <- Either.cond(
        payload.bytes.length == layout.byteLength,
        (),
        PayloadError.Length(layout.byteLength, payload.bytes.length)
      )
      values <- element.read(payload.bytes, layout.count)
    yield values

/** Little-endian primitives over plain `Int` and `Long` arithmetic. */
private[codec] object Bytes:
  def copy(bytes: IArray[Byte]): IArray[Byte] =
    val out = Array.ofDim[Byte](bytes.length)
    var i   = 0
    while i < bytes.length do
      out(i) = bytes(i)
      i += 1
    IArray.unsafeFromArray(out)

  def putInt(out: Array[Byte], at: Int, value: Int): Unit =
    var i = 0
    while i < 4 do
      out(at + i) = (value >>> (8 * i)).toByte
      i += 1

  def putLong(out: Array[Byte], at: Int, value: Long): Unit =
    var i = 0
    while i < 8 do
      out(at + i) = (value >>> (8 * i)).toByte
      i += 1

  def getInt(bytes: IArray[Byte], at: Int): Int =
    (bytes(at) & 0xff) |
      ((bytes(at + 1) & 0xff) << 8) |
      ((bytes(at + 2) & 0xff) << 16) |
      ((bytes(at + 3) & 0xff) << 24)

  def getLong(bytes: IArray[Byte], at: Int): Long =
    var value = 0L
    var i     = 0
    while i < 8 do
      value = value | ((bytes(at + i) & 0xffL) << (8 * i))
      i += 1
    value
