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

package eyes4s.io

import eyes4s.codec.{LocationComparisonResultCodec, LocationComparisonRun}
import eyes4s.plan.LocationComparisonPlan
import java.nio.file.{Files, Path}

/** Opt-in qualification of an existing, checksum-verified location bundle. */
object LocationBundleWitness:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw IllegalArgumentException(e.toString), identity)
  def main(args: Array[String]): Unit =
    require(
      args.length == 3,
      "LocationBundleWitness score|restore bundle-directory output-directory"
    )
    val output = Path.of(args(2))
    if args(0) == "restore" then
      val bytes    = Files.readString(output.resolve("location-run.json"))
      val start    = System.nanoTime()
      val restored = get(
        LocationComparisonResultCodec.codec.decode(get(io.circe.parser.parse(bytes)))
      )
      println(
        s"RESTORED maps=${restored.result.maps.size} queries=${restored.result.outcomes.size} milliseconds=${(System.nanoTime() - start) / 1000000} bytes=${bytes.length}"
      )
    else
      require(args(0) == "score", "unknown mode")
      val base               = Path.of(args(1))
      def read(name: String) = Files.readString(base.resolve(name))
      val start              = System.nanoTime()
      val imported           = get(
        LocationBundleImport.read(
          LocationBundleText(
            read("trials.csv"),
            read("locations.tsv"),
            read("pairs.tsv"),
            read("phase-ledger.tsv"),
            read("location-ledger.tsv")
          )
        )
      )
      val admittedAt = System.nanoTime()
      val plan       = get(
        LocationComparisonPlan.binned(
          imported.input,
          imported.profile.columns,
          imported.profile.rows,
          imported.queries
        )
      )
      val result   = plan.run
      val scoredAt = System.nanoTime()
      Files.createDirectories(output)
      val scores = result.outcomes.map(o =>
        s"${o.query.id}\t${get(o.matched).value}\t${get(o.control).value}\t${get(o.difference)}"
      )
      Files.writeString(
        output.resolve("scores.tsv"),
        ("pair_id\tmatched\tcontrol\tdifference" +: scores).mkString("\n") + "\n"
      )
      val bins = result.maps.flatMap { entry =>
        get(entry.result).bins.zipWithIndex.collect {
          case (count, index) if count > 0 =>
            s"${entry.trial.value}\t${index % plan.grid.nx}\t${index / plan.grid.nx}\t$count"
        }
      }
      Files.writeString(
        output.resolve("bins.tsv"),
        ("trial_id\tbin_x\tbin_y\tcount" +: bins).mkString("\n") + "\n"
      )
      val archive = get(
        LocationComparisonResultCodec.codec.encode(LocationComparisonRun(plan, result))
      )
      Files.writeString(output.resolve("location-run.json"), archive.noSpaces)
      println(
        s"SCORED phases=${imported.phaseLedger.size} raw=${imported.locationLedger.size} admitted=${imported.input.trials.map(_.observations.size.toLong).sum} trials=${result.maps.size} queries=${result.outcomes.size} occupiedBins=${bins.size} unpairedNonempty=${imported.unpairedRetrievals.count(_.trial.nonEmpty)} importMs=${(admittedAt - start) / 1000000} scoreMs=${(scoredAt - admittedAt) / 1000000} archiveBytes=${archive.noSpaces.length}"
      )
