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

package eyes4s.examples

import eyes4s.compare.Distribution
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.{EdgePolicy, Smoother}
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}

/** Independently estimates every map from fixation coordinates, then scores explicit pairs. */
object EndToEndDensityCli:
  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalArgumentException(x.toString), identity)

  def main(args: Array[String]): Unit =
    val root = Path.of(sys.env("EYES4S_PRIVATE_RUN"))
    val c    = get(parser.parse(Files.readString(root.resolve("request.json")))).hcursor
    def num(key: String) = get(c.get[Double](key))
    val bounds    = get(Bounds.of[Px](num("x_min"), num("y_min"), num("x_max"), num("y_max")))
    val frame     = Frame.of(FrameId("private-density-window"), bounds, YAxis.Down)
    val grid      = get(Grid.over(frame, get(c.get[Int]("nx")), get(c.get[Int]("ny"))))
    val smoother  = Smoother.gaussian(get(Sigma.px(num("sigma_px"))), EdgePolicy.Truncate)
    val templates = get(c.get[Vector[Json]]("templates"))
    val sources   = get(c.get[Vector[Json]]("sources"))
    def str(j: Json, key: String)   = get(j.hcursor.get[String](key))
    def estimate(j: Json): Mass[Px] =
      val f  = j.hcursor.downField("fixations")
      val xs = get(f.get[Vector[Double]]("x"))
      val ys = get(f.get[Vector[Double]]("y"))
      val ds = get(f.get[Vector[Double]]("duration_ms"))
      require(xs.size == ys.size && xs.size == ds.size, "Fixation column lengths differ")
      require(xs.size >= get(c.get[Int]("min_fixations")), "Too few fixations")
      val pts = xs.zip(ys).map((x, y) => Pt[Px](x, y))
      require(pts.forall(bounds.contains), "Unaccounted crop exclusion")
      get(
        smoother.density(get(PointMeasure.of(frame, IArray.from(pts), IArray.from(ds))), grid)
      )
    val maps = (templates ++ sources).map(j => str(j, "id") -> estimate(j)).toMap
    require(maps.size == templates.size + sources.size, "Duplicate map identity")
    val eligible = sources.groupMap(j => str(j, "participant"))(j => str(j, "template"))
    val cosine   = Distribution.cosine[Px]
    def score(a: String, b: String) = get(cosine.compare(maps(a), maps(b))).value
    val scores                      = sources.map { s =>
      val id          = str(s, "id")
      val participant = str(s, "participant")
      val matched     = str(s, "template")
      require(
        templates.exists(t => str(t, "id") == matched && str(t, "participant") == participant),
        "Matched template belongs to another participant"
      )
      val controls = templates
        .filter(t =>
          str(t, "participant") == participant &&
            str(t, "id") != matched && eligible(participant).contains(str(t, "id"))
        )
        .map(str(_, "id"))
      require(controls.nonEmpty, "No within-participant controls")
      val observed = score(id, matched)
      val baseline = controls.map(score(id, _)).sum / controls.size
      Json.obj(
        "id"         -> Json.fromString(id),
        "template"   -> Json.fromString(matched),
        "controls"   -> Json.arr(controls.map(Json.fromString)*),
        "observed"   -> Json.fromDoubleOrNull(observed),
        "baseline"   -> Json.fromDoubleOrNull(baseline),
        "difference" -> Json.fromDoubleOrNull(observed - baseline)
      )
    }
    val output = Json.obj(
      "maps" -> Json.arr((templates ++ sources).map { j =>
        Json.obj(
          "id"     -> Json.fromString(str(j, "id")),
          "values" -> Json.arr(maps(str(j, "id")).values.toVector.map(Json.fromDoubleOrNull)*)
        )
      }*),
      "sources" -> Json.arr(scores*)
    )
    Files.writeString(
      root.resolve("eyes4s.json"),
      output.noSpaces,
      StandardOpenOption.CREATE_NEW
    ): Unit
