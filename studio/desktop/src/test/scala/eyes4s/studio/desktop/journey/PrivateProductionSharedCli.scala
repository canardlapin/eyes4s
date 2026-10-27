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

package eyes4s.studio.desktop.journey

import eyes4s.compare.Distribution
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.{Json, parser}
import java.nio.file.{Files, Path}

/** Reverse array bridge: the public eyes4s comparison over current eyesim's exact arrays.
  * These index-space checks do not identify endpoint grids with cell-center grids.
  */
object PrivateProductionSharedCli:
  import PrivateProductionInputs.{get, write, external}
  private final case class Tolerance(absolute: Double, relative: Double):
    def approxEquals(a: Double, b: Double): Boolean =
      a.isFinite && b.isFinite && math
        .abs(a - b) <= absolute + relative * math.max(math.abs(a), math.abs(b))
  private val ScoreTolerance          = Tolerance(absolute = 2e-12, relative = 2e-12)
  def main(args: Array[String]): Unit =
    require(!sys.env.contains("CI") && !sys.env.contains("GITHUB_ACTIONS"))
    val root               = external(Path.of(sys.env("EYES4S_PRIVATE_REFERENCE")))
    def read(name: String) = get(parser.parse(Files.readString(root.resolve(name)))).hcursor
    val participants       = get(read("manifest.json").get[Vector[Json]]("participants"))
    var pairs              = 0L
    var maximum            = 0.0
    participants.foreach { participant =>
      val row     = participant.hcursor
      val request = read(get(row.get[String]("file")))
      val result  = read(get(row.get[String]("output")))
      val frame   = get(
        Frame.screen(
          "private-array-bridge",
          get(request.get[Int]("width_px")),
          get(request.get[Int]("height_px"))
        )
      )
      val grid = get(Grid.over(frame, get(request.get[Int]("nx")), get(request.get[Int]("ny"))))
      val maps = get(result.get[Vector[Json]]("maps")).map { item =>
        val c = item.hcursor
        get(c.get[String]("id")) -> get(
          Surface
            .intensity(
              grid,
              IArray.from(get(c.get[Vector[Double]]("values"))),
              Provenance.raw(ContentHash.empty)
            )
            .flatMap(_.normalised)
        )
      }.toMap
      val comparison                          = Distribution.cosine[Px]
      def score(a: String, b: String): Double =
        pairs += 1
        get(comparison.compare(maps(a), maps(b))).value
      get(result.get[Vector[Json]]("scores")).foreach { item =>
        val c        = item.hcursor
        val query    = get(c.get[String]("id"))
        val matched  = score(query, get(c.get[String]("matched")))
        val controls = get(c.get[Vector[String]]("controls"))
        val baseline = controls.map(score(query, _)).sum / controls.size
        Vector(
          "observed"   -> matched,
          "baseline"   -> baseline,
          "difference" -> (matched - baseline)
        ).foreach { (name, found) =>
          val expected = get(c.downField("eyesim").get[Double](name))
          val gap      = math.abs(found - expected)
          require(
            ScoreTolerance.approxEquals(found, expected),
            s"Shared-array $name differs"
          )
          maximum = math.max(maximum, gap)
        }
      }
    }
    write(
      root.resolve("eyes4s-on-eyesim.json"),
      Json.obj(
        "status"             -> Json.fromString("passed"),
        "pairs"              -> Json.fromLong(pairs),
        "max_abs"            -> Json.fromDoubleOrNull(maximum),
        "absolute_tolerance" -> Json.fromDoubleOrNull(ScoreTolerance.absolute),
        "relative_tolerance" -> Json.fromDoubleOrNull(ScoreTolerance.relative)
      )
    )
