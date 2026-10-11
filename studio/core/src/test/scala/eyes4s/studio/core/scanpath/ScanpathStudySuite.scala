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

package eyes4s.studio.core.scanpath

import io.circe.Json
import munit.FunSuite

class ScanpathStudySuite extends FunSuite:
  private def get[E, A](e: Either[E, A]): A         = e.fold(x => fail(x.toString), identity)
  private def request(short: Boolean = false): Json =
    def item(id: String, x: Vector[Double], template: Option[String]): Json =
      val n    = x.size
      val base = Json.obj(
        "id"          -> Json.fromString(id),
        "participant" -> Json.fromString("P"),
        "fixations"   -> Json.obj(
          "x"           -> Json.arr(x.map(Json.fromDoubleOrNull)*),
          "y"           -> Json.arr(Vector(20, 40, 10).take(n).map(Json.fromInt)*),
          "onset_ms"    -> Json.arr(Vector(0, 200, 400).take(n).map(Json.fromInt)*),
          "duration_ms" -> Json.arr(Vector.fill(n)(Json.fromInt(100))*)
        )
      )
      template.fold(base)(t =>
        base.deepMerge(
          Json.obj("template" -> Json.fromString(t), "phase" -> Json.fromString("cue"))
        )
      )
    Json.obj(
      "schema_version"   -> Json.fromInt(1),
      "screen_width_px"  -> Json.fromInt(400),
      "screen_height_px" -> Json.fromInt(300),
      "sample_rate_hz"   -> Json.fromInt(250),
      "templates"        -> Json.arr(
        item("a", Vector(30, 60, 70), None),
        item("b", Vector(100.0, 120.0, 130.0).take(if short then 2 else 3), None)
      ),
      "sources" -> Json.arr(
        item("q1", Vector(30, 60, 70), Some("a")),
        item("q2", Vector(100, 120, 130), Some("b"))
      )
    )
  test("identity scores, exact pair cardinality and all components survive JSON") {
    val p      = get(ScanpathStudy.read(request()))
    val result = get(p.run("cue"))
    assertEquals(get(result.hcursor.get[Vector[Json]]("pairs")).size, 4)
    val queries = get(result.hcursor.get[Vector[Json]]("queries"))
    queries.foreach { q =>
      ScanpathStudy.components.foreach { component =>
        assertEquals(
          get(q.hcursor.downField("values").downField(component).get[Double]("observed")),
          1.0
        )
      }
    }
    assertEquals(get(get(ScanpathStudy.parse(p.request.noSpaces)).run("cue")), result)
    assert(p.run("delay").isLeft)
  }
  test("short matched and control operands remain explicit unavailable queries") {
    val result = get(get(ScanpathStudy.read(request(true))).run("cue"))
    val pairs  = get(result.hcursor.get[Vector[Json]]("pairs"))
    assertEquals(pairs.count(_.hcursor.downField("error").succeeded), 2)
    val queries = get(result.hcursor.get[Vector[Json]]("queries"))
    assert(queries.forall(_.hcursor.downField("values").focus.contains(Json.Null)))
    assert(queries.forall(q => get(q.hcursor.get[Vector[Json]]("unavailable_pairs")).size == 1))
  }
  test("forged and duplicate pair schedules are rejected before execution") {
    val pair = Json.obj(
      "source"    -> Json.fromString("q1"),
      "reference" -> Json.fromString("b"),
      "kind"      -> Json.fromString("matched")
    )
    assert(
      ScanpathStudy.read(request().deepMerge(Json.obj("pairs" -> Json.arr(pair, pair)))).isLeft
    )
  }
  test("malformed fields name the input operand") {
    val invalid = request().mapObject(_.remove("screen_width_px"))
    val error   = ScanpathStudy.read(invalid).swap.toOption.get
    assert(error.message.contains("screen_width_px"))
  }
