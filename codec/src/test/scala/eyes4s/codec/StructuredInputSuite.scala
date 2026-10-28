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

import eyes4s.kernel.Unit2D.Px
import io.circe.Json

class StructuredInputSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val limits = ColumnarDocument.Limits(1000000, 20000, 100000, 100000, 1000000L, 64)
  private def stored(doc: CanonicalDoc): CanonicalDoc =
    val graph = get(ColumnarDocument.build(doc, limits))
    val bytes = get(ColumnarDocument.encode(graph, limits))
    get(ColumnarDocument.decode(bytes, limits)).canonical

  test("compact input restores the frozen v1 keys, rows, raw clocks and canonical identity") {
    val codec     = StudyInputCodecs.study[Px]
    val frozen    = get(io.circe.parser.parse(StudyInputFixtures.inputVersionOne))
    val original  = get(codec.input.decode(frozen))
    val described = get(codec.input.document(original))
    assertEquals(get(described.json), frozen)
    val restored = get(codec.readInputDocument(stored(described)))
    assertEquals(get(codec.input.encode(restored)), frozen)
    assertEquals(restored.reference, original.reference)
    assertEquals(get(codec.input.digest(restored)), get(CanonicalDigest.document(frozen)))
    assertEquals(restored.trials.rows.map(_.value.fixations.size).sum, 48)
  }

  test("compact ledger v1 preserves rejected records and its earliest schema") {
    val codec     = StudyInputCodecs.study[Px]
    val frozen    = get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    val original  = get(codec.ledger.decode(frozen))
    val described = get(codec.ledger.document(original))
    assert(!described.isInstanceOf[CanonicalDoc.Leaf])
    assertEquals(get(described.json), frozen)
    assertEquals(get(codec.ledger.digest(original)), get(CanonicalDigest.document(frozen)))
    val restored = get(codec.readLedgerDocument(stored(described)))
    assertEquals(get(codec.ledger.encode(restored)), frozen)
    assertEquals(restored.records, original.records)
    assertEquals(restored.version, 1)
  }

  test("compact ledger v3 restores every inventory, flag, record attribute and integer") {
    val codec     = StudyInputCodecs.trial[Px]
    val frozen    = get(io.circe.parser.parse(LedgerV3Mirrors.ledgerVersionThree))
    val original  = get(codec.ledger.decode(frozen))
    val described = get(codec.ledger.document(original))
    assert(!described.isInstanceOf[CanonicalDoc.Leaf])
    assertEquals(get(described.json), frozen)
    assertEquals(get(codec.ledger.digest(original)), get(CanonicalDigest.document(frozen)))
    val restored = get(codec.readLedgerDocument(stored(described)))
    assertEquals(get(codec.ledger.encode(restored)), frozen)
    assertEquals(restored.records, original.records)
    assertEquals(restored.inventory, original.inventory)
  }

  test("typed ledger restoration stops before producing the row after an invalid record") {
    val codec  = StudyInputCodecs.study[Px]
    val frozen = get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    val schema = frozen.hcursor.downField("schema").focus.get
    val value  = frozen.hcursor.downField("value").focus.get.asObject.get
    val first  =
      value("records").get.asArray.get.head.mapObject(_.add("kind", Json.fromString("bogus")))
    var seen = Vector.empty[Int]
    val rows = CanonicalDoc.Arr(
      2,
      i => {
        seen :+= i
        if i == 0 then Right(CanonicalDoc.Leaf(first))
        else Left(CodecError.Unsupported("second row", "must not be produced"))
      }
    )
    val payload = CanonicalDoc.obj(
      value.toVector.map((k, v) =>
        k -> (if k == "records" then rows else CanonicalDoc.Leaf(v))
      )*
    )
    val doc   = CanonicalDoc.obj("schema" -> CanonicalDoc.Leaf(schema), "value" -> payload)
    val error = codec.readLedgerDocument(doc).swap.toOption.get
    assert(error.message.contains("unknown disposition bogus"))
    assert(error.message.contains("records[0]"))
    assertEquals(seen, Vector(0))
  }

  test("compact input still refuses forged identity and unsupported logical schemas") {
    val codec  = StudyInputCodecs.study[Px]
    val frozen = get(io.circe.parser.parse(StudyInputFixtures.inputVersionOne))
    val forged = frozen.mapObject(
      _.add(
        "value",
        frozen.hcursor
          .downField("value")
          .focus
          .get
          .mapObject(_.add("input", Json.fromString("0000000000000000")))
      )
    )
    assert(codec.readInputDocument(stored(CanonicalDoc.Leaf(forged))).isLeft)
    val wrong = frozen.mapObject(
      _.add(
        "schema",
        Json.obj("name" -> Json.fromString("test.unknown"), "version" -> Json.fromInt(1))
      )
    )
    assert(codec.readInputDocument(stored(CanonicalDoc.Leaf(wrong))).isLeft)
  }

  test("ledger canonical binding streams records and retains each earliest schema") {
    val codec = StudyInputCodecs.study[Px]
    Vector(
      StudyInputFixtures.ledgerVersionOne,
      StudyV2Mirrors.ledgerVersionTwo,
      SourceIdentityMirrors.ledgerV4
    ).foreach { fixture =>
      val frozen  = get(io.circe.parser.parse(fixture))
      val value   = get(codec.ledger.decode(frozen))
      val doc     = get(codec.ledger.document(value))
      val records = get(get(StructuredValue(doc).member("value")).member("records"))
      assert(records.document.isInstanceOf[CanonicalDoc.Arr])
      assertEquals(get(doc.json), frozen)
      assertEquals(get(codec.ledger.digest(value)), get(CanonicalDigest.document(frozen)))
      assertEquals(get(codec.ledger.encode(value)), frozen)
      assertEquals(
        codec.ledger.ladder.map(_.earliest(value)),
        codec.ledgerLadder.codec.ladder.map(_.earliest(value))
      )
    }
  }
