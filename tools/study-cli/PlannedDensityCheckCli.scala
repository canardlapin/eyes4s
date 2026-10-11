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
import eyes4s.laws.Tolerance
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}

/** Direct public-library checks of the cached-rank sampled-control evaluator. */
object PlannedDensityCheckCli:
  private def get[E, A](v: Either[E, A]): A =
    v.fold(e => throw new IllegalArgumentException(e.toString), identity)
  def main(args: Array[String]): Unit =
    val root = Path.of(sys.env("EYES4S_PRIVATE_RUN"))
    val c    = get(parser.parse(Files.readString(root.resolve("verification.json")))).hcursor
    val grid = get(
      Grid.over(
        get(Frame.screen("planned-rank-check", get(c.get[Int]("nx")), get(c.get[Int]("ny")))),
        get(c.get[Int]("nx")),
        get(c.get[Int]("ny"))
      )
    )
    val tolerance = Tolerance(absolute = 2e-12, relative = 2e-12)
    val rows      = get(c.get[Vector[Json]]("scores"))
    val selected  = rows
      .groupBy(j => get(j.hcursor.get[String]("phase")))
      .toVector
      .sortBy(_._1)
      .flatMap(_._2.take(4))
    var maximum = 0.0
    var checked = 0
    Vector("eyes4s", "eyesim").foreach { engine =>
      val maps = get(c.downField("maps").get[Map[String, Vector[Double]]](engine)).map {
        (id, v) =>
          id -> get(
            Surface
              .intensity(grid, IArray.from(v), Provenance.raw(ContentHash.empty))
              .flatMap(_.normalised)
          )
      }
      val sources = get(c.get[Vector[Json]]("sources"))
        .map(j => get(j.hcursor.get[String]("id")) -> get(j.hcursor.get[String]("template")))
        .toMap
      selected.foreach { row =>
        val id                         = get(row.hcursor.get[String]("id"))
        def score(ref: String): Double =
          checked += 1
          get(Distribution.spearman[Px].compare(maps(id), maps(ref))).value
        val observed = score(sources(id))
        val controls = get(row.hcursor.get[Vector[String]]("controls"))
        val baseline = controls.map(score).sum / controls.size
        Vector(
          "observed"   -> observed,
          "baseline"   -> baseline,
          "difference" -> (observed - baseline)
        ).foreach { (key, value) =>
          val expected = get(row.hcursor.downField(engine).get[Double](key))
          require(
            tolerance.approxEquals(value, expected),
            s"Spearman $key differs for $engine $id"
          )
          maximum = math.max(maximum, math.abs(value - expected))
        }
      }
    }
    Files.writeString(
      root.resolve("native-check.json"),
      Json
        .obj(
          "status"       -> Json.fromString("passed"),
          "direct_pairs" -> Json.fromInt(checked),
          "max_abs"      -> Json.fromDoubleOrNull(maximum)
        )
        .noSpaces,
      StandardOpenOption.CREATE_NEW
    ): Unit
