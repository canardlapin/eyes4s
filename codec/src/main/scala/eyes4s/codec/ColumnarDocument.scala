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

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.{Label, break}

/** Private storage experiment. Native archives do not accept this format yet.
  * Objects with the same ordered keys are stored as reference columns, and
  * identical values are interned without normalizing numbers or strings.
  */
private[codec] object ColumnarDocument:
  final case class Limits(
      bytes: Int,
      nodes: Int,
      references: Int,
      textUnits: Int,
      expandedNodes: Long,
      depth: Int
  )

  final case class Statistics(
      nodes: Int,
      objectShapes: Int,
      arrayRecords: Int,
      shapeFields: Long,
      references: Long,
      textUnits: Long,
      expandedNodes: Long,
      depth: Int
  )

  private enum Node:
    case Empty, False, True
    case Number(token: String, negativeZero: Boolean, value: Json)
    case Text(value: String)
    case Arr(items: Vector[Int])
    case Obj(shape: Int, values: Vector[Int])

  final class Document private[ColumnarDocument] (
      private[ColumnarDocument] val nodes: Vector[Node],
      private[ColumnarDocument] val shapes: Vector[Vector[String]],
      private[ColumnarDocument] val root: Int,
      val statistics: Statistics
  ):
    /** No array item is expanded until the caller requests it. */
    def canonical: CanonicalDoc = at(root)

    private def at(index: Int): CanonicalDoc = nodes(index) match
      case Node.Empty               => CanonicalDoc.Leaf(Json.Null)
      case Node.False               => CanonicalDoc.Leaf(Json.False)
      case Node.True                => CanonicalDoc.Leaf(Json.True)
      case Node.Number(_, _, value) => CanonicalDoc.Leaf(value)
      case Node.Text(value)         => CanonicalDoc.Leaf(Json.fromString(value))
      case Node.Arr(items)          => CanonicalDoc.Arr(items.size, i => Right(at(items(i))))
      case Node.Obj(s, items)       =>
        CanonicalDoc.obj(
          shapes(s).zip(items).map((key, id) => key -> CanonicalDoc.Deferred(() => at(id)))*
        )

  private val Magic = Vector[Byte](69, 52, 67, 68, 1)

  private def fail[A](reason: String)(using Label[Either[CodecError, A]]): Nothing =
    break(Left(CodecError.Unsupported("columnar document", reason)))

  private def take[A, B](value: Either[CodecError, B])(using
      Label[Either[CodecError, A]]
  ): B = value match
    case Right(v) => v
    case Left(e)  => break(Left(e))

  private def valid[A](limits: Limits)(using Label[Either[CodecError, A]]): Unit =
    if limits.bytes < Magic.size || limits.nodes < 1 || limits.references < 0 ||
      limits.textUnits < 0 || limits.expandedNodes < 1 || limits.depth < 1
    then fail(s"invalid limits $limits")

  /** Check all resource and graph bounds before exposing an immutable view. */
  private def checked(
      nodes: Vector[Node],
      shapes: Vector[Vector[String]],
      root: Int,
      limits: Limits
  ): Either[CodecError, Document] = boundary:
    valid(limits)
    if nodes.isEmpty || nodes.size > limits.nodes || root != nodes.size - 1 then
      fail(s"root $root and ${nodes.size} nodes exceed limits or are not a final root")
    val costs  = new Array[Long](nodes.size)
    val depths = new Array[Int](nodes.size)
    var refs   = shapes.iterator.map(_.size.toLong).sum
    var text   = shapes.iterator.flatMap(_.iterator).map(_.length.toLong).sum
    shapes.zipWithIndex.foreach { (keys, s) =>
      if keys.distinct.size != keys.size then fail(s"shape $s contains duplicate keys")
    }
    nodes.zipWithIndex.foreach { (node, index) =>
      val children = node match
        case Node.Arr(items)     => items
        case Node.Obj(s, values) =>
          if s < 0 || s >= shapes.size || values.size != shapes(s).size then
            fail(s"node $index has invalid shape $s or column count ${values.size}")
          values
        case Node.Number(token, _, _) =>
          text += token.length
          Vector.empty
        case Node.Text(value) =>
          text += value.length
          Vector.empty
        case _ => Vector.empty
      refs += children.size
      if refs > limits.references || text > limits.textUnits then
        fail(
          s"node $index exceeds references $refs/${limits.references} or text $text/${limits.textUnits}"
        )
      var cost  = 1L
      var depth = 1
      children.foreach { child =>
        if child < 0 || child >= index then
          fail(s"node $index has non-postorder reference $child")
        if costs(child) > limits.expandedNodes - cost then
          fail(s"node $index expands beyond ${limits.expandedNodes} nodes")
        cost += costs(child)
        depth = math.max(depth, depths(child) + 1)
      }
      if cost > limits.expandedNodes || depth > limits.depth then
        fail(
          s"node $index exceeds expansion $cost/${limits.expandedNodes} or depth $depth/${limits.depth}"
        )
      costs(index) = cost
      depths(index) = depth
    }
    // All stored values must belong to the root; silently ignoring a dictionary
    // record would hide data that the physical document claims to contain.
    val reached    = mutable.BitSet(root)
    val usedShapes = mutable.BitSet.empty
    nodes.indices.reverseIterator.foreach { index =>
      if reached(index) then
        nodes(index) match
          case Node.Arr(items)         => reached ++= items
          case Node.Obj(shape, values) =>
            usedShapes += shape
            reached ++= values
          case _ => ()
    }
    if reached.size != nodes.size || usedShapes.size != shapes.size then
      fail(
        s"unreachable records: ${nodes.size - reached.size} nodes and ${shapes.size - usedShapes.size} shapes"
      )
    Right(
      new Document(
        nodes,
        shapes,
        root,
        Statistics(
          nodes.size,
          shapes.size,
          nodes.count { case Node.Arr(_) => true; case _ => false },
          shapes.iterator.map(_.size.toLong).sum,
          refs,
          text,
          costs(root),
          depths(root)
        )
      )
    )

  def build(doc: CanonicalDoc, limits: Limits): Either[CodecError, Document] = boundary:
    valid(limits)
    val nodes      = mutable.ArrayBuffer.empty[Node]
    val ids        = mutable.HashMap.empty[Node, Int]
    val shapes     = mutable.ArrayBuffer.empty[Vector[String]]
    val shapeIds   = mutable.HashMap.empty[Vector[String], Int]
    var references = 0L
    var textUnits  = 0L
    var visited    = 0L

    def intern(node: Node): Int = ids.getOrElseUpdate(
      node, {
        if nodes.size >= limits.nodes then fail(s"node count exceeds ${limits.nodes}")
        node match
          case Node.Number(token, _, _) => textUnits += token.length
          case Node.Text(value)         => textUnits += value.length
          case Node.Arr(items)          => references += items.size
          case Node.Obj(_, values)      => references += values.size
          case _                        => ()
        if references > limits.references || textUnits > limits.textUnits then
          fail(
            s"references $references/${limits.references} or text $textUnits/${limits.textUnits} exceeded"
          )
        val id = nodes.size
        nodes += node
        id
      }
    )

    def visit(value: CanonicalDoc, depth: Int): Int =
      visited += 1
      if visited > limits.expandedNodes || depth > limits.depth then
        fail(s"input traversal exceeds ${limits.expandedNodes} nodes or depth ${limits.depth}")
      value match
        case CanonicalDoc.Deferred(make) => visit(make(), depth)
        case CanonicalDoc.Leaf(json)     =>
          json.fold(
            intern(Node.Empty),
            b => intern(if b then Node.True else Node.False),
            n =>
              intern(
                Node.Number(
                  n.toString,
                  java.lang.Double.doubleToLongBits(n.toDouble) == Long.MinValue,
                  Json.fromJsonNumber(n)
                )
              ),
            s => intern(Node.Text(s)),
            a => visitArray(a.size, i => Right(CanonicalDoc.Leaf(a(i))), depth),
            o => visitObject(o.toVector.map((k, v) => k -> CanonicalDoc.Leaf(v)), depth)
          )
        case CanonicalDoc.Arr(size, item) => visitArray(size, item, depth)
        case CanonicalDoc.Obj(members)    => visitObject(members.toVector, depth)

    def visitArray(size: Int, item: Int => Either[CodecError, CanonicalDoc], depth: Int): Int =
      if size < 0 || size.toLong > limits.references || size.toLong > limits.expandedNodes - visited
      then fail(s"array length $size exceeds remaining traversal or reference limit")
      intern(Node.Arr(Vector.tabulate(size)(i => visit(take(item(i)), depth + 1))))

    def visitObject(members: Vector[(String, CanonicalDoc)], depth: Int): Int =
      val keys  = members.map(_._1)
      val shape = shapeIds.getOrElseUpdate(
        keys, {
          if shapes.size >= limits.nodes then fail(s"shape count exceeds ${limits.nodes}")
          references += keys.size
          if references > limits.references then
            fail(s"shape fields exceed ${limits.references}")
          textUnits += keys.iterator.map(_.length.toLong).sum
          if textUnits > limits.textUnits then
            fail(s"shape text exceeds ${limits.textUnits} units")
          val id = shapes.size
          shapes += keys
          id
        }
      )
      intern(Node.Obj(shape, members.map((_, v) => visit(v, depth + 1))))

    val root = visit(doc, 1)
    checked(nodes.toVector, shapes.toVector, root, limits)

  def encode(document: Document, limits: Limits): Either[CodecError, IArray[Byte]] = boundary:
    valid(limits)
    take(checked(document.nodes, document.shapes, document.root, limits))
    val out                = mutable.ArrayBuffer.empty[Byte]
    def byte(v: Int): Unit =
      if out.size >= limits.bytes then fail(s"stored bytes exceed ${limits.bytes}")
      out += v.toByte
    def uint(value: Int): Unit =
      var n = value
      while n >= 128 do
        byte((n & 127) | 128)
        n >>>= 7
      byte(n)
    def text(value: String): Unit =
      val ascii = value.forall(_ < 128)
      byte(if ascii then 0 else 1)
      uint(value.length)
      value.foreach { c =>
        if !ascii then byte(c.toInt >>> 8)
        byte(c.toInt & 255)
      }
    def packedUnsignedLsb(values: IndexedSeq[Int]): Unit =
      val base    = if values.isEmpty then 0 else values.min
      val largest = if values.isEmpty then 0 else values.max - base
      val width   = 32 - java.lang.Integer.numberOfLeadingZeros(largest)
      uint(base)
      byte(width)
      var acc  = 0L
      var held = 0
      values.foreach { value =>
        acc |= (value - base).toLong << held
        held += width
        while held >= 8 do
          byte((acc & 255).toInt)
          acc >>>= 8
          held -= 8
      }
      if held > 0 then byte(acc.toInt)

    Magic.foreach(b => byte(b.toInt))
    uint(document.nodes.size)
    uint(document.shapes.size)
    document.shapes.foreach(keys => { uint(keys.size); keys.foreach(text) })
    document.nodes.foreach {
      case Node.Empty           => uint(0)
      case Node.False           => uint(1)
      case Node.True            => uint(2)
      case Node.Number(_, _, _) => uint(3)
      case Node.Text(_)         => uint(4)
      case Node.Arr(_)          => uint(5)
      case Node.Obj(s, _)       => uint(s + 6)
    }
    document.nodes.foreach {
      case Node.Number(token, negativeZero, _) =>
        text(token); byte(if negativeZero then 1 else 0)
      case Node.Text(value) => text(value)
      case Node.Arr(items)  => uint(items.size); items.foreach(uint)
      case _                => ()
    }
    val grouped = Array.fill(document.shapes.size)(mutable.ArrayBuffer.empty[Vector[Int]])
    document.nodes.foreach {
      case Node.Obj(s, values) => grouped(s) += values
      case _                   => ()
    }
    document.shapes.indices.foreach { s =>
      document
        .shapes(s)
        .indices
        .foreach(c => packedUnsignedLsb(grouped(s).map(row => row(c)).toVector))
    }
    Right(IArray.unsafeFromArray(out.toArray))

  def decode(bytes: IArray[Byte], limits: Limits): Either[CodecError, Document] = boundary:
    valid(limits)
    if bytes.length > limits.bytes then
      fail(s"stored bytes ${bytes.length} exceed ${limits.bytes}")
    var offset      = 0
    var references  = 0L
    var textUnits   = 0L
    def byte(): Int =
      if offset >= bytes.length then fail(s"truncated at byte $offset")
      val b = bytes(offset) & 255
      offset += 1
      b
    def uint(): Int =
      var value = 0L
      var shift = 0
      var more  = true
      while more do
        val b = byte()
        if shift == 28 && b > 7 then fail(s"integer overflow at byte ${offset - 1}")
        value |= (b & 127).toLong << shift
        more = (b & 128) != 0
        if shift > 0 && !more && b == 0 then fail(s"overlong integer at byte ${offset - 1}")
        shift += 7
      value.toInt
    def count(max: Long, field: String): Int =
      val n = uint()
      if n.toLong > max || n > bytes.length - offset then
        fail(s"$field count $n exceeds remaining byte or resource bound $max at byte $offset")
      n
    def text(): String =
      val mode = byte()
      if mode > 1 then fail(s"invalid text mode $mode at byte ${offset - 1}")
      val unitBytes = mode + 1
      val n         = count(
        math
          .min(limits.textUnits.toLong - textUnits, (bytes.length - offset) / unitBytes.toLong),
        "text"
      )
      textUnits += n
      val out = new java.lang.StringBuilder(n)
      var i   = 0
      while i < n do
        val value = if mode == 1 then (byte() << 8) | byte() else byte()
        if mode == 0 && value >= 128 then fail(s"non-ASCII value $value at byte ${offset - 1}")
        out.append(value.toChar)
        i += 1
      out.toString
    def packedUnsignedLsb(n: Int): Vector[Int] =
      references += n
      if references > limits.references then
        fail(s"column references $references exceed ${limits.references}")
      val base  = uint()
      val width = byte()
      if width > 31 then fail(s"invalid column width $width at byte ${offset - 1}")
      val size = (n.toLong * width + 7L) / 8L
      if size > bytes.length - offset then fail(s"column needs $size bytes at $offset")
      var acc    = 0L
      var held   = 0
      val mask   = (1L << width) - 1L
      val result = Vector.fill(n) {
        while held < width do
          acc |= byte().toLong << held
          held += 8
        val value = base.toLong + (acc & mask)
        acc >>>= width
        held -= width
        if value > Int.MaxValue then fail(s"column reference $value overflows at byte $offset")
        value.toInt
      }
      if acc != 0L then fail(s"nonzero column padding at byte ${offset - 1}")
      result
    def refs(n: Int): Vector[Int] =
      references += n
      if references > limits.references || n > bytes.length - offset then
        fail(s"reference count $references exceeds ${limits.references} or remaining bytes")
      Vector.fill(n)(uint())

    Magic.foreach(b =>
      if byte() != (b & 255) then fail(s"invalid magic/version at byte ${offset - 1}")
    )
    val nodeCount = count(limits.nodes, "node")
    if nodeCount == 0 then fail("empty node table")
    val shapeCount = count(limits.nodes, "shape")
    val shapes     = Vector.fill(shapeCount) {
      val n = count(limits.references.toLong - references, "shape field")
      references += n
      Vector.fill(n)(text())
    }
    val tags = Vector.fill(nodeCount)(uint())
    if tags.exists(t => t >= 6 && t - 6 >= shapeCount) then fail("unknown object shape tag")
    val values = tags.map {
      case 0 => Node.Empty
      case 1 => Node.False
      case 2 => Node.True
      case 3 =>
        val token  = text()
        val number = io.circe.JsonNumber
          .fromString(token)
          .getOrElse(fail(s"invalid JSON number at byte $offset: $token"))
        val sign = byte()
        if sign > 1 || (sign == 1 && number.toDouble != 0.0) then
          fail(s"invalid negative-zero flag $sign for $token at byte ${offset - 1}")
        val parsedSign = java.lang.Double.doubleToLongBits(number.toDouble) == Long.MinValue
        val value      =
          if parsedSign == (sign == 1) then Json.fromJsonNumber(number)
          else if number.toBigDecimal.exists(_ == BigDecimal(0)) then
            Json.fromDoubleOrNull(if sign == 1 then -0.0 else 0.0)
          else fail(s"number sign cannot be preserved for $token at byte $offset")
        Node.Number(token, sign == 1, value)
      case 4 => Node.Text(text())
      case 5 => Node.Arr(refs(count(limits.references, "array")))
      case t => Node.Obj(t - 6, Vector.empty)
    }.toArray
    val grouped = Array.fill(shapes.size)(mutable.ArrayBuffer.empty[Int])
    tags.zipWithIndex.foreach { (tag, index) =>
      if tag >= 6 then grouped(tag - 6) += index
    }
    shapes.indices.foreach { s =>
      val rows    = grouped(s)
      val columns = shapes(s).map(_ => packedUnsignedLsb(rows.size))
      rows.zipWithIndex.foreach { (id, r) => values(id) = Node.Obj(s, columns.map(_(r))) }
    }
    if offset != bytes.length then fail(s"${bytes.length - offset} trailing bytes at $offset")
    checked(values.toVector, shapes, nodeCount - 1, limits)
