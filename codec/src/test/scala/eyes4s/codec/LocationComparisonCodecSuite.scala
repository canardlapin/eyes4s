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
import eyes4s.plan.*
import io.circe.Json

class LocationComparisonCodecSuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private val frame                         = get(Frame.screen("location-codec", 1000, 1000))
  private def trial(id: String, points: Vector[(Double, Double)]): LocationTrial =
    get(
      LocationTrial.of(
        get(LocationTrialId.of(id)),
        points.zipWithIndex.map { case ((x, y), i) =>
          LocationObservation(get(LocationSourceId.of(id)), i.toLong + 1, Pt[Unit2D.Px](x, y))
        }
      )
    )
  private val trials = Vector(
    trial("f", Vector(1.0 -> 1.0, 1.0 -> 900.0)),
    trial("m", Vector(1.0 -> 1.0)),
    trial("c", Vector(900.0 -> 1.0))
  )
  private def query(swapped: Boolean) = get(
    LocationComparisonQuery.of(
      "q",
      trials(0).id,
      trials(if swapped then 2 else 1).id,
      trials(if swapped then 1 else 2).id
    )
  )
  private def plan(swapped: Boolean = false) = get(
    LocationComparisonPlan.binned(
      get(LocationInput.of(frame, trials)),
      2,
      2,
      Vector(query(swapped))
    )
  )
  private val original = plan()
  private val run      = LocationComparisonRun(original, original.run)
  private val codec    = LocationComparisonResultCodec.codec
  private def set(j: Json, k: String, v: Json): Json = j.mapObject(_.add(k, v))

  test("asymmetric stored maps and scores roundtrip canonically") {
    val encoded  = get(codec.encode(run))
    val restored = get(codec.decode(encoded))
    assertEquals(
      restored.result.maps.map(_.result.map(_.bins)),
      run.result.maps.map(_.result.map(_.bins))
    )
    assertEquals(
      restored.result.outcomes.map(_.matched.map(_.value)),
      run.result.outcomes.map(_.matched.map(_.value))
    )
    assertEquals(restored.result.outcomes.head.control.map(_.value), Right(0.0))
    assertEquals(get(codec.encode(restored)), encoded)
  }
  test("writer refuses a result attached to changed comparison operands") {
    assert(codec.encode(LocationComparisonRun(plan(true), run.result)).isLeft)
  }
  test("decoder refuses missing duplicate impossible and mismatched results") {
    val encoded   = get(codec.encode(run))
    val value     = get(encoded.hcursor.get[Json]("value"))
    val maps      = get(value.hcursor.get[Vector[Json]]("maps"))
    val outcomes  = get(value.hcursor.get[Vector[Json]]("outcomes"))
    val mutations = Vector(
      set(value, "maps", Json.arr(maps.tail*)),
      set(value, "maps", Json.arr((maps :+ maps.head)*)),
      set(value, "outcomes", Json.arr()),
      set(value, "outcomes", Json.arr((outcomes :+ outcomes.head)*)),
      set(
        value,
        "outcomes",
        Json.arr(
          set(outcomes.head, "matched", ResultWire.tagged("score", "value" -> Json.fromInt(2)))
        )
      ),
      set(value, "inputHash", Json.fromString(ContentHash.ofString("wrong").render)),
      set(value, "planHash", Json.fromString(ContentHash.ofString("wrong").render))
    )
    mutations.foreach(v => assert(codec.decode(set(encoded, "value", v)).isLeft))
  }
  test("plan decoder refuses changed coordinates retaining their old identity") {
    val encoded = get(LocationComparisonResultCodec.plans.encode(original))
    val value   = get(encoded.hcursor.get[Json]("value"))
    val rows    = get(value.hcursor.get[Vector[Json]]("trials"))
    val obs     = get(rows.head.hcursor.get[Vector[Json]]("observations"))
    val row     = set(
      rows.head,
      "observations",
      Json.arr((set(obs.head, "x", Json.fromInt(20)) +: obs.tail)*)
    )
    assert(
      LocationComparisonResultCodec.plans
        .decode(set(encoded, "value", set(value, "trials", Json.arr((row +: rows.tail)*))))
        .isLeft
    )
  }
  test("failure wire forms retain typed operands and refuse unknown tags") {
    val id     = trials.head.id
    val errors = Vector(
      LocationMapError.Measure(id, SurfaceError.DegenerateTotal(0)),
      LocationMapError.Binning(id, GeometryError.DegenerateGrid(0, 1)),
      LocationMapError.Surface(id, SurfaceError.DegenerateTotal(0))
    )
    errors.foreach { e =>
      assertEquals(
        get(LocationComparisonCodec.readMapError(LocationComparisonCodec.mapError(e))),
        e
      )
      val r = LocationRunError.Map("q", "focal", id, e)
      assertEquals(
        get(LocationComparisonCodec.readRunError(LocationComparisonCodec.runError(r))),
        r
      )
    }
    val e = LocationRunError.Comparison(
      "q",
      "matched",
      id,
      trials(1).id,
      eyes4s.compare.CompareError.Frames(GeometryError.DegenerateGrid(0, 1))
    )
    assertEquals(
      get(LocationComparisonCodec.readRunError(LocationComparisonCodec.runError(e))),
      e
    )
    assert(LocationComparisonCodec.readMapError(ResultWire.tagged("unknown")).isLeft)
    assert(LocationComparisonCodec.readRunError(ResultWire.tagged("unknown")).isLeft)
  }

  test(
    "standalone input codec binds exact coordinates and source ordinals without analysis fields"
  ) {
    val inputs   = LocationInputCodec.of(get(DefinitionId.of("test.location-input", 1)))
    val encoded  = get(inputs.encode(original.input))
    val restored = get(inputs.decode(encoded))
    assertEquals(
      restored.trials.map(t => t.id -> t.observations),
      original.input.trials.map(t => t.id -> t.observations)
    )
    assertEquals(get(inputs.digest(restored)), get(inputs.digest(original.input)))
    val value = get(encoded.hcursor.get[Json]("value"))
    assertEquals(value.asObject.get.keys.toSet, Set("frame", "trials"))
    val rows         = get(value.hcursor.get[Vector[Json]]("trials"))
    val observations = get(rows.head.hcursor.get[Vector[Json]]("observations"))
    val bad          = set(
      rows.head,
      "observations",
      Json.arr((set(observations.head, "ordinal", Json.fromString("0")) +: observations.tail)*)
    )
    assert(
      inputs
        .decode(set(encoded, "value", set(value, "trials", Json.arr((bad +: rows.tail)*))))
        .isLeft
    )
  }
