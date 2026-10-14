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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.Json

class ArchiveMemorySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
  private val codec    = StudyResultCodecs.cosine[Px]
  private val result   = get(codec.codec.parse(StudyResultFixtures.resultVersionOne))
  private val original = get(codec.codec.encode(result))
  private val payload  = get(original.hcursor.get[Json]("value"))
  private val masses   = result.scales.map(_.estimation.map(_._2.toOption))
  private val first = masses.head.head.getOrElse(fail("fixture needs a successful first mass"))

  private def changedFirst(mass: Option[Mass[Px]]) =
    masses.updated(0, masses.head.updated(0, mass))

  test("metadata writer preserves the ordinary document except inline density cells") {
    val outcomes = get(payload.hcursor.get[Vector[Json]]("scales")).map { scale =>
      get(scale.hcursor.get[Vector[Json]]("estimation")).map { row =>
        val value = get(row.hcursor.get[Json]("outcome"))
        value.mapObject(_.remove("values"))
      }
    }
    assertEquals(
      get(codec.storageMetadata(result)),
      DensityWire.replaceOutcomes(payload, outcomes)
    )
  }

  test("packed and recomputable bytes retain the legacy writer's complete metadata") {
    val archive = new DensityArchiveCodec(codec)
    for storage <- Vector(DensityStorage.Packed, DensityStorage.Recomputable) do
      val bundle      = get(archive.encode(result, storage))
      val saved       = get(archive.codec.encode(bundle.archive))
      val value       = get(saved.hcursor.get[Json]("value"))
      val savedScales = get(value.hcursor.get[Vector[Json]]("scales"))
      val outcomes    = get(payload.hcursor.get[Vector[Json]]("scales")).zip(savedScales).map {
        (scale, stored) =>
          val rows       = get(scale.hcursor.get[Vector[Json]]("estimation"))
          val storedRows = get(stored.hcursor.get[Vector[Json]]("estimation"))
          rows.zip(storedRows).map { (row, storedRow) =>
            val outcome = get(row.hcursor.get[Json]("outcome"))
            if outcome.hcursor.get[String]("kind").contains("failure") then outcome
            else
              outcome.mapObject(
                _.remove("values").add(
                  "storage",
                  get(storedRow.hcursor.downField("outcome").get[Json]("storage"))
                )
              )
          }
      }
      val expected = Json.obj(
        "schema" -> Wire.id(DensityArchiveDefinitions.studyResultV2),
        "value"  -> DensityWire.replaceOutcomes(payload, outcomes)
      )
      assertEquals(saved, expected)
      assertEquals(
        get(CanonicalDigest.document[Any](saved)).sha256,
        get(CanonicalDigest.document[Any](expected)).sha256
      )
  }

  test("typed reconstruction preserves the full result and source-free packed restore") {
    assertEquals(
      get(codec.codec.encode(get(codec.restoreDensities(payload, masses)))),
      original
    )
    val archive  = new DensityArchiveCodec(codec)
    val bundle   = get(archive.encode(result, DensityStorage.Packed))
    val loaded   = get(archive.codec.decode(get(archive.codec.encode(bundle.archive))))
    val restored = get(loaded.materialize(ref => bundle.chunks.find(_._2.ref == ref).map(_._2)))
    assertEquals(get(codec.codec.encode(restored)), original)
    assertEquals(get(codec.codec.digest(restored)), get(codec.codec.digest(result)))
  }

  test("typed reconstruction rejects missing scales and rows") {
    assert(
      codec
        .restoreDensities(payload, masses.dropRight(1))
        .left
        .toOption
        .exists(_.message.contains("materialized scales"))
    )
    assert(
      codec
        .restoreDensities(payload, masses.updated(0, masses.head.dropRight(1)))
        .left
        .toOption
        .exists(_.message.contains("materialized rows"))
    )
  }

  test("typed reconstruction cannot omit a successful density") {
    assert(
      codec
        .restoreDensities(payload, changedFirst(None))
        .left
        .toOption
        .exists(_.message.contains("missing materialized mass"))
    )
  }

  test("typed reconstruction checks archived geometry and provenance") {
    val grid = get(
      Grid.over(
        first.grid.frame.withId(FrameId("wrong-restored-frame")),
        first.grid.nx,
        first.grid.ny
      )
    )
    val geometry = get(Surface.mass(grid, first.values, first.provenance))
    assert(codec.restoreDensities(payload, changedFirst(Some(geometry))).isLeft)
    val provenance = get(
      Surface.mass(
        first.grid,
        first.values,
        Provenance(ContentHash.ofString("wrong-restored-source"), Vector.empty)
      )
    )
    assert(
      codec
        .restoreDensities(payload, changedFirst(Some(provenance)))
        .left
        .toOption
        .exists(_.message.contains("provenance"))
    )
  }
