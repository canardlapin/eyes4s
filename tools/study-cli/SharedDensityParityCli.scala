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

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.control.NonFatal

/** Compare scores on identical R-generated arrays; does not assert estimator equivalence. */
object SharedDensityParityCli:
  private val ScoreTolerance                    = Tolerance(absolute = 1e-12, relative = 1e-12)
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.toString), identity)

  private def execute(directory: Path): Json =
    val input = get(
      parser.parse(Files.readString(directory.resolve("shared.json"), UTF_8))
    ).hcursor
    val nx = get(input.get[Int]("nx"))
    val ny = get(input.get[Int]("ny"))
    // A common index lattice: physical KDE locations are held in the R request, not inferred here.
    val frame                      = get(Frame.screen("shared-density-index-lattice", nx, ny))
    val grid                       = get(Grid.over(frame, nx, ny))
    def mass(json: Json): Mass[Px] =
      val values = get(json.hcursor.get[Vector[Double]]("values"))
      get(
        Surface
          .intensity(grid, IArray.from(values), Provenance.raw(ContentHash.empty))
          .flatMap(_.normalised)
      )
    val templates = get(input.get[Vector[Json]]("templates")).map { json =>
      val c = json.hcursor
      (get(c.get[String]("id")), get(c.get[String]("participant")), mass(json))
    }
    require(templates.map(_._1).distinct.size == templates.size, "Duplicate template identity")
    val sources = get(input.get[Vector[Json]]("sources"))
    require(sources.nonEmpty, "No comparisons")
    val eligible = sources
      .map { json =>
        val c = json.hcursor
        get(c.get[String]("participant")) -> get(c.get[String]("template"))
      }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toMap
    val cosine      = Distribution.cosine[Px]
    val differences = sources.map { json =>
      val c           = json.hcursor
      val participant = get(c.get[String]("participant"))
      val matched     = get(c.get[String]("template"))
      val source      = mass(json)
      val template    = templates
        .find(t => t._1 == matched && t._2 == participant)
        .getOrElse(throw new IllegalArgumentException("No participant-specific template"))
      val controls = templates.filter(t =>
        t._2 == participant && t._1 != matched &&
          eligible(participant).contains(t._1)
      )
      require(controls.nonEmpty, "No distinct within-participant control")
      val observed = get(cosine.compare(source, template._3)).value
      val baseline =
        controls.map(t => get(cosine.compare(source, t._3)).value).sum / controls.size
      require(controls.size == get(c.get[Int]("n_perm")), "Control cardinality differs")
      Vector(
        "observed"   -> observed,
        "baseline"   -> baseline,
        "difference" -> (observed - baseline)
      ).map { (name, value) =>
        val expected = get(c.get[Double](name))
        require(ScoreTolerance.approxEquals(value, expected), s"$name differs from eyesim")
        math.abs(value - expected)
      }.max
    }
    Json.obj(
      "status"                      -> Json.fromString("passed"),
      "sources"                     -> Json.fromInt(sources.size),
      "templates"                   -> Json.fromInt(templates.size),
      "maximum_absolute_difference" -> Json.fromDoubleOrNull(differences.max),
      "absolute_tolerance"          -> Json.fromDoubleOrNull(ScoreTolerance.absolute),
      "relative_tolerance"          -> Json.fromDoubleOrNull(ScoreTolerance.relative),
      "scope"                       -> Json.fromString(
        "cosine and exhaustive within-participant controls on shared maps"
      )
    )

  def main(args: Array[String]): Unit =
    val directory = Path.of(
      sys.env.getOrElse(
        "EYES4S_PRIVATE_RUN",
        throw new IllegalArgumentException(
          "Set EYES4S_PRIVATE_RUN to an external run directory"
        )
      )
    )
    val result = try execute(directory)
    catch
      case NonFatal(error) =>
        Json.obj(
          "status" -> Json.fromString("failed"),
          "error"  -> Json.fromString(error.toString)
        )
    Files.writeString(
      directory.resolve("comparison.json"),
      result.spaces2,
      UTF_8,
      StandardOpenOption.CREATE_NEW
    )
    if get(result.hcursor.get[String]("status")) != "passed" then sys.exit(1)
