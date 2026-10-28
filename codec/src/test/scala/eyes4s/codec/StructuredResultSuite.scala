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
import eyes4s.kernel.Provenance
import io.circe.Json

class StructuredResultSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val results                           = StudyResultCodecs.cosine[Px]
  private val archiveCodec                      = new DensityArchiveCodec(results)
  private val result = get(results.codec.parse(StudyResultFixtures.resultVersionOne))
  private val limits = ColumnarDocument.Limits(2000000, 30000, 200000, 200000, 1000000L, 64)
  private def stored(doc: CanonicalDoc): CanonicalDoc =
    val graph = get(ColumnarDocument.build(doc, limits))
    val bytes = get(ColumnarDocument.encode(graph, limits))
    get(ColumnarDocument.decode(bytes, limits)).canonical

  test("compact inline and packed result descriptions restore the exact frozen result") {
    Vector(DensityStorage.Inline, DensityStorage.Packed).foreach { storage =>
      val bundle      = get(archiveCodec.encode(result, storage))
      val json        = get(archiveCodec.codec.encode(bundle.archive))
      val description = archiveCodec.document(bundle.archive)
      assertEquals(get(description.json), json)
      val archive  = get(archiveCodec.decodeDocument(stored(description)))
      var calls    = 0
      val restored = get(archive.materialize(ref => {
        calls += 1
        bundle.chunks.find(_._2.ref == ref).map(_._2)
      }))
      assertEquals(
        get(results.codec.encode(restored)),
        get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne))
      )
      assertEquals(get(results.codec.digest(restored)), get(results.codec.digest(result)))
      assertEquals(archive.original, storage == DensityStorage.Inline)
      if storage == DensityStorage.Packed then assert(calls > 0) else assertEquals(calls, 0)
    }
  }

  test(
    "compact reconstruction metadata reuses verified masses without making the full JSON tree"
  ) {
    val description = get(results.storageMetadataDocument(result))
    val masses      = result.scales.map(_.estimation.map(_._2.toOption))
    val restored    = get(results.restoreDensitiesDocument(stored(description), masses))
    assertEquals(get(results.codec.digest(restored)), get(results.codec.digest(result)))
    assertEquals(
      restored.scales.map(_.analyses.controlSource.rows.size),
      result.scales.map(_.analyses.controlSource.rows.size)
    )
  }

  test("described outcome replacement refuses changed scale or row cardinality") {
    val description = get(results.storageMetadataDocument(result))
    assert(DensityWire.replaceOutcomesDocument(description, Vector.empty).isLeft)
    val malformed = get(DensityWire.replaceOutcomesDocument(description, Vector(Vector.empty)))
    assert(malformed.json.isLeft)
  }

  test("provenance fields stream beyond the bounded single-row JSON projection") {
    val fields  = Vector("trials" -> Vector.fill(30000)(Provenance.Param.Text("retained")))
    val doc     = CanonicalDoc.obj("description" -> ResultWire.describeFields(fields))
    val compact = stored(doc)
    assert(StructuredValue(compact).json.isLeft)
    assertEquals(get(ResultWire.readFields(StructuredValue(compact))), fields)
    assertEquals(
      get(CanonicalDigest.streamed[Unit](compact)),
      get(CanonicalDigest.streamed[Unit](doc))
    )
  }

  test("replacing a described member retains its original ordered position") {
    val doc = CanonicalDoc.obj(
      "first"  -> CanonicalDoc.Leaf(Json.fromInt(1)),
      "middle" -> CanonicalDoc.Leaf(Json.fromInt(2)),
      "last"   -> CanonicalDoc.Leaf(Json.fromInt(3))
    )
    val replaced =
      get(StructuredValue(doc).replace("middle", CanonicalDoc.Leaf(Json.fromInt(4))))
    assertEquals(
      get(replaced.json).asObject.map(_.keys.toVector),
      Some(Vector("first", "middle", "last"))
    )
  }
