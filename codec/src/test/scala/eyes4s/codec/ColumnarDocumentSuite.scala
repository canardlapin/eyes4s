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

import io.circe.{Json, JsonNumber}

class ColumnarDocumentSuite extends munit.FunSuite:
  private val limits = ColumnarDocument.Limits(1000000, 10000, 100000, 100000, 1000000L, 64)
  private def get[E, A](e: Either[E, A]): A     = e.fold(x => fail(s"$x"), identity)
  private def bytes(values: Int*): IArray[Byte] = IArray.from(values.map(_.toByte))
  private def round(doc: CanonicalDoc)          =
    val built   = get(ColumnarDocument.build(doc, limits))
    val encoded = get(ColumnarDocument.encode(built, limits))
    (encoded, get(ColumnarDocument.decode(encoded, limits)))

  test("exact numeric spellings, signed zero, UTF-16, member order and repeated rows survive") {
    val tokens =
      Vector("1", "1.0", "1e0", "-0.0", "0.0", "9007199254740993", "0.10000000000000001")
    val text = "é\u0000\ud800x\udfff"
    val row  = Json.obj(
      "second" -> Json.fromString(text),
      "first"  -> Json.fromValues(
        tokens.map(t => Json.fromJsonNumber(JsonNumber.fromString(t).get))
      )
    )
    val original = Json.arr(row, row, Json.obj(), Json.arr(), Json.False, Json.True, Json.Null)
    val (encoded, restored) = round(CanonicalDoc.Leaf(original))
    val found               = get(restored.canonical.json)
    assertEquals(found.noSpaces, original.noSpaces)
    assertEquals(found.asArray.get.head.asObject.get.keys.toVector, Vector("second", "first"))
    assertEquals(get(ColumnarDocument.encode(restored, limits)).toVector, encoded.toVector)
  }

  test("canonical identities agree with the existing document renderer") {
    val original = Json.obj(
      "rows" -> Json.fromValues(
        Vector.fill(300)(
          Json.obj(
            "x"     -> Json.fromDoubleOrNull(-0.0),
            "flag"  -> Json.True,
            "label" -> Json.fromString("retained")
          )
        )
      )
    )
    val (encoded, restored) = round(CanonicalDoc.Leaf(original))
    assert(encoded.length < original.noSpaces.length / 4)
    assertEquals(
      get(CanonicalDigest.streamed[Json](restored.canonical)),
      get(CanonicalDigest.document[Json](original))
    )
    assertEquals(restored.statistics.expandedNodes, 1202L)
  }

  test("positive and negative zero never share an interned number") {
    val original      = Json.arr(Json.fromDoubleOrNull(-0.0), Json.fromDoubleOrNull(0.0))
    val (_, restored) = round(CanonicalDoc.Leaf(original))
    val found         = get(restored.canonical.json).asArray.get
    assertEquals(
      java.lang.Double.doubleToLongBits(found(0).asNumber.get.toDouble),
      Long.MinValue
    )
    assertEquals(java.lang.Double.doubleToLongBits(found(1).asNumber.get.toDouble), 0L)
    assertEquals(
      get(CanonicalDigest.streamed[Json](restored.canonical)),
      get(CanonicalDigest.document[Json](original))
    )
  }

  test("stored zero signs override platform-specific JSON number parsing") {
    val prefix = Vector(69, 52, 67, 68, 1, 1, 0, 3, 0)
    Vector("-0.0", "0", "0.0").foreach { token =>
      Vector(0, 1).foreach { sign =>
        val wire =
          bytes((prefix ++ Vector(token.length) ++ token.map(_.toInt) ++ Vector(sign))*)
        val doc   = get(ColumnarDocument.decode(wire, limits))
        val found = get(doc.canonical.json).asNumber.get.toDouble
        assertEquals(
          java.lang.Double.doubleToLongBits(found),
          if sign == 1 then Long.MinValue else 0L
        )
        assertEquals(get(ColumnarDocument.encode(doc, limits)).toVector, wire.toVector)
        val expected = Json.fromDoubleOrNull(if sign == 1 then -0.0 else 0.0)
        assertEquals(
          get(CanonicalDigest.streamed[Json](doc.canonical)),
          get(CanonicalDigest.document[Json](expected))
        )
      }
    }
  }

  test("packed columns cross byte boundaries and restore ordered rows exactly") {
    val rows = Vector.tabulate(1000)(i =>
      Json.obj(
        "id"    -> Json.fromInt((i * 97) % 1009),
        "group" -> Json.fromInt(i % 3),
        "same"  -> Json.True
      )
    )
    val original      = Json.fromValues(rows)
    val (_, restored) = round(CanonicalDoc.Leaf(original))
    val found         = get(StructuredRows.read(restored.canonical, "rows")((j, _) => Right(j)))
    assertEquals(found, rows)
    assertEquals(
      get(CanonicalDigest.streamed[Json](restored.canonical)),
      get(CanonicalDigest.document[Json](original))
    )
  }

  test("mutated binary input always refuses or returns a bounded re-encodable document") {
    val (wire, _) =
      round(CanonicalDoc.Leaf(Json.obj("x" -> Json.arr(Json.fromInt(17), Json.True))))
    val random = new scala.util.Random(1147L)
    (0 until 500).foreach { _ =>
      val changed = wire.toVector.toArray
      changed(random.nextInt(changed.length)) = random.nextInt(256).toByte
      ColumnarDocument.decode(IArray.unsafeFromArray(changed), limits) match
        case Left(_)    => ()
        case Right(doc) =>
          assert(doc.statistics.nodes <= limits.nodes)
          assert(doc.statistics.references <= limits.references)
          assert(doc.statistics.expandedNodes <= limits.expandedNodes)
          val encoded = get(ColumnarDocument.encode(doc, limits))
          assert(ColumnarDocument.decode(encoded, limits).isRight)
    }
  }

  test("deferred object members are visited in order only when consumed") {
    var seen      = Vector.empty[Int]
    val described = CanonicalDoc.obj(
      "first"  -> CanonicalDoc.Deferred(() => { seen :+= 1; CanonicalDoc.Leaf(Json.True) }),
      "second" -> CanonicalDoc.Deferred(() => { seen :+= 2; CanonicalDoc.Leaf(Json.False) })
    )
    val mapped = described.mapError(identity)
    assertEquals(seen, Vector.empty[Int])
    assertEquals(
      get(CanonicalDigest.streamed[Json](mapped)),
      get(
        CanonicalDigest.document[Json](Json.obj("first" -> Json.True, "second" -> Json.False))
      )
    )
    assertEquals(seen, Vector(1, 2))
    val (_, restored) = round(mapped)
    assertEquals(
      get(restored.canonical.json),
      Json.obj("first" -> Json.True, "second" -> Json.False)
    )
  }

  test("row restoration is sequential, preserves all rows and stops at the first failure") {
    var made   = Vector.empty[Int]
    val source = CanonicalDoc.Arr(
      10,
      i => {
        made :+= i
        Right(CanonicalDoc.Leaf(Json.fromInt(i)))
      }
    )
    val result = StructuredRows.read(source, "rows")((j, i) =>
      if i == 3 then Left(CodecError.Unsupported("row", "intentional"))
      else Right(j.asNumber.get.toInt.get)
    )
    assert(result.isLeft)
    assertEquals(made, Vector(0, 1, 2, 3))
    val (_, doc) = round(CanonicalDoc.Arr(10, i => Right(CanonicalDoc.Leaf(Json.fromInt(i)))))
    assertEquals(
      get(
        StructuredRows.read(doc.canonical, "rows")((j, _) => Right(j.asNumber.get.toInt.get))
      ),
      (0 until 10).toVector
    )
  }

  test("writer bounds apply to lazy input before calling an oversized array's item") {
    var called = false
    val huge   = CanonicalDoc.Arr(
      Int.MaxValue,
      _ => { called = true; Right(CanonicalDoc.Leaf(Json.Null)) }
    )
    assert(ColumnarDocument.build(huge, limits).isLeft)
    assert(!called)
    val (_, doc) = round(CanonicalDoc.Leaf(Json.fromString("a moderately long string")))
    assert(ColumnarDocument.encode(doc, limits.copy(bytes = 8)).isLeft)
    assert(
      ColumnarDocument
        .build(CanonicalDoc.Leaf(Json.arr(Json.True, Json.False)), limits.copy(nodes = 1))
        .isLeft
    )
    assert(
      ColumnarDocument
        .build(CanonicalDoc.Leaf(Json.fromString("abcd")), limits.copy(textUnits = 3))
        .isLeft
    )
  }

  test("every truncation, trailing data and an unsupported version are refused") {
    val (encoded, _) =
      round(CanonicalDoc.Leaf(Json.obj("x" -> Json.arr(Json.True, Json.False))))
    encoded.indices.foreach(n =>
      assert(ColumnarDocument.decode(IArray.from(encoded.take(n)), limits).isLeft, s"prefix $n")
    )
    assert(ColumnarDocument.decode(IArray.from(encoded.toVector :+ 0.toByte), limits).isLeft)
    val changed = encoded.toVector.toArray
    changed(4) = 2
    assert(ColumnarDocument.decode(IArray.unsafeFromArray(changed), limits).isLeft)
  }

  test(
    "hostile counts, overflowing varints, forward references and invalid numbers are refused"
  ) {
    val magic  = Vector(69, 52, 67, 68, 1)
    val probes = Vector(
      magic ++ Vector(255, 255, 255, 255, 7), // huge node table, no bytes
      magic ++ Vector(128, 0),                // overlong integer
      magic ++ Vector(255, 255, 255, 255, 8), // signed Int overflow
      magic ++ Vector(1, 0, 5, 1, 0),         // array 0 references itself
      magic ++ Vector(1, 0, 3, 0, 1, 120, 0)  // numeric token x
    )
    probes.foreach(p => assert(ColumnarDocument.decode(bytes(p*), limits).isLeft, s"$p"))
  }

  test("unused dictionary records are refused rather than silently ignored") {
    assert(ColumnarDocument.decode(bytes(69, 52, 67, 68, 1, 2, 0, 0, 1), limits).isLeft)
    // An empty object shape followed by an unrelated null root.
    assert(ColumnarDocument.decode(bytes(69, 52, 67, 68, 1, 1, 1, 0, 0), limits).isLeft)
  }

  test("a tiny shared graph cannot cause exponential expansion") {
    // null, [null,null], [previous,previous], ... has only 15 nodes, but 32767 expanded nodes.
    val payload = Vector(69, 52, 67, 68, 1, 15, 0, 0) ++ Vector.fill(14)(5) ++
      (1 until 15).flatMap(i => Vector(2, i - 1, i - 1))
    assert(ColumnarDocument.decode(bytes(payload*), limits.copy(expandedNodes = 10000L)).isLeft)
    assert(ColumnarDocument.decode(bytes(payload*), limits.copy(depth = 10)).isLeft)
    val doc = get(ColumnarDocument.decode(bytes(payload*), limits))
    assertEquals(doc.statistics.expandedNodes, 32767L)
  }
