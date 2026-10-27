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

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.real.{RealPrepared, RealResults, RealStudyBackend}
import io.circe.Json
import io.circe.syntax.*
import java.nio.file.Files

/** The public library's prepared-plan execution, before the application journey. */
object PrivateProductionApiCli:
  import PrivateProductionInputs.*
  def main(args: Array[String]): Unit =
    val input    = load
    val root     = input.root
    val admitted = input.admit
    write(root.resolve("api-admission.json"), admitted.summary.asJson)
    write(root.resolve("api-ledger.json"), admitted.ledger.asJson)
    write(root.resolve("api-spec.json"), input.spec.asJson)
    val grids     = get(input.request.get[Vector[Vector[Int]]]("grid_candidates"))
    val attempted = grids.iterator.map { grid =>
      val p = get(
        RealPrepared.of(
          AnalysisRevision(1),
          DatasetRevision(1),
          input.recipe(grid(0), grid(1)),
          admitted
        )
      )
      (p, p.counts.totalMaps * grid(0).toLong * grid(1))
    }
    val (prepared, cells) = attempted
      .find(_._2 <= 20000000L)
      .getOrElse(
        throw new AssertionError("No declared grid fits the existing density-cell budget")
      )
    write(root.resolve("api-recipe.json"), prepared.recipe.asJson)
    write(
      root.resolve("api-preflight.json"),
      Json.obj(
        "summary"          -> prepared.summary.asJson,
        "maps"             -> Json.fromLong(prepared.counts.totalMaps),
        "density_cells"    -> Json.fromLong(cells),
        "source_reference" -> Json.fromString(admitted.input.reference.digest),
        "plan_sha256"      -> Json.fromString(
          get(prepared.plans.codec.digest(prepared.plan)).sha256.hex
        )
      )
    )
    println(
      s"admission and preflight complete; maps=${prepared.counts.totalMaps}, cells=$cells"
    )
    val result = get(prepared.work.run)
    val read   = get(
      RealResults.of(
        RunId(1),
        RealStudyBackend.RealRun(prepared, result, RealStudyBackend.RunOrigin.Computed)
      )
    )
    val rows = Vector.newBuilder[QueryRow]
    var next = Option(0)
    while next.nonEmpty do
      val page = get(read.queries(get(PageRequest.of(next.get, PageRequest.MaximumSize))))
      rows ++= page.rows
      next = page.page.next
    val queries = rows.result()
    write(root.resolve("api-queries.json"), queries.asJson)
    write(root.resolve("api-summary.json"), get(read.summary).asJson)
    val maps = root.resolve("api-maps")
    Files.createDirectory(maps)
    val manifest = result.scales.head.estimation
      .groupBy(_._1.participant)
      .toVector
      .sortBy(_._1)
      .zipWithIndex
      .map { case ((participant, estimates), index) =>
        val name = f"participant-$index%03d.json"
        val data = estimates.map { (key, outcome) =>
          Json.obj(
            "key"     -> RealResults.key(key).asJson,
            "item"    -> Json.fromString(key.item),
            "mass"    -> outcome.fold(_ => Json.Null, m => m.values.toVector.asJson),
            "failure" -> outcome.fold(e => Json.fromString(e.toString), _ => Json.Null)
          )
        }
        write(maps.resolve(name), data.asJson)
        Json.obj("participant" -> Json.fromString(participant), "file" -> Json.fromString(name))
      }
    write(maps.resolve("manifest.json"), manifest.asJson)
    write(
      root.resolve("api-result.json"),
      Json.obj(
        "status"           -> Json.fromString("passed"),
        "queries"          -> Json.fromInt(queries.size),
        "result_sha256"    -> Json.fromString(get(prepared.digest(result))),
        "source_reference" -> Json.fromString(admitted.input.reference.digest)
      )
    )
    println(s"API complete: ${queries.size} queries")
