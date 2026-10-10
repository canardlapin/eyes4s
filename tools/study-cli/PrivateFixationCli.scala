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

import eyes4s.codec.*
import eyes4s.design.Trials
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.{Json, parser}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.control.NonFatal

/** Opt-in JVM probe launched by tools/private-fixtures/run.py. No test discovers private data. */
object PrivateFixationCli:
  private def get[E, A](result: Either[E, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.toString), identity)

  private def write(directory: Path, name: String, value: Json): Unit =
    Files.writeString(
      directory.resolve(name),
      value.noSpaces,
      UTF_8,
      StandardOpenOption.CREATE_NEW
    )
    ()

  private def execute(directory: Path): Boolean =
    val request = get(
      parser.parse(Files.readString(directory.resolve("request.json"), UTF_8))
    ).hcursor
    val frame = get(
      Frame.screen(
        "private-fixture-display",
        get(request.get[Int]("width_px")),
        get(request.get[Int]("height_px"))
      )
    )
    val unit = get(request.get[String]("time_unit")) match
      case "seconds"      => TimestampUnit.Seconds
      case "milliseconds" => TimestampUnit.Milliseconds
      case "microseconds" => TimestampUnit.Microseconds
      case _              => throw new IllegalArgumentException("Unknown time unit")
    val rate  = get(Hz(get(request.get[Double]("sample_rate_hz"))))
    val trial = get(TrialColumns.of("participant", "phase", "trial"))
    val table = get(
      FixationTable.of(
        trial,
        "ordinal",
        "x",
        "y",
        TimeColumns("onset", "duration", unit),
        SampleCountRule.DerivedFromDuration(rate),
        Some("item")
      )
    )
    // This inventory describes observed groups only; it makes no claim about absent trials.
    val inventory = get(
      TrialInventory.read(
        Files.readString(directory.resolve("inventory.csv"), UTF_8),
        get(TrialInventoryColumns.of(trial, None))
      )
    )
    val imported = get(
      FixationCsv.admitInventory(
        Files.readString(directory.resolve("input.csv"), UTF_8),
        table,
        inventory,
        frame
      )
    )
    val report    = imported.fixations
    val accounted = report.admitted.map(_.rowNumber) ++ report.rejected.map(_.rowNumber)
    require(
      accounted.sorted == (2 until report.sourceRows.size + 2).toVector,
      "Source rows are not a lossless partition"
    )
    require(
      report.accepted.rows.map(_.value.n).sum == report.admitted.size,
      "Accepted scanpaths do not account for admitted rows"
    )

    // Bound codec scratch space while round-tripping every admitted trial, in original order.
    val codec = StudyInputCodecs.trial[Px].input
    report.accepted.rows.grouped(128).foreach { rows =>
      val input    = StudyInput(Trials(rows))
      val restored = get(codec.parse(get(codec.encode(input)).noSpaces))
      require(restored.reference == input.reference, "Study input codec identity changed")
    }
    val trials = report.accepted.rows.map { row =>
      val key  = row.key
      val path = row.value
      Json.obj(
        "key" -> Json.arr(
          Json.fromString(key.participant),
          Json.fromString(key.phase),
          Json.fromString(key.trial),
          Json.fromString(key.item)
        ),
        "fixations"       -> Json.fromInt(path.n),
        "dwell_us"        -> Json.fromLong(path.dwellTotal.toMicros),
        "first_onset_us"  -> Json.fromLong(path.first.span.onset.toMicros),
        "last_offset_us"  -> Json.fromLong(path.last.span.offset.toMicros),
        "path_length_px"  -> Json.fromDoubleOrNull(path.pathLength),
        "derived_samples" -> Json.fromLong(
          path.fixations.iterator.map(_.sampleCount.toLong).sum
        )
      )
    }
    write(
      directory,
      "actual.json",
      Json.obj(
        "source_rows"        -> Json.fromInt(report.sourceRows.size),
        "admitted_rows"      -> Json.fromInt(report.admitted.size),
        "outside_frame_rows" -> Json.arr(report.outsideFrame.map(r => Json.fromInt(r.record))*),
        "rejected"           -> Json.arr(
          report.rejected.map(r =>
            Json.obj(
              "record" -> Json.fromInt(r.rowNumber),
              "reason" -> Json.fromString(r.error.message)
            )
          )*
        ),
        "roundtrip_trials" -> Json.fromInt(trials.size),
        "trials"           -> Json.arr(trials*)
      )
    )
    report.rejected.isEmpty && report.sourceRows.nonEmpty

  def main(args: Array[String]): Unit =
    sys.env.get("EYES4S_PRIVATE_RUN") match
      case None =>
        System.err.println("Use tools/private-fixtures/run.py with explicit local paths.")
        sys.exit(2)
      case Some(value) =>
        val directory = Path.of(value)
        try if !execute(directory) then sys.exit(1)
        catch
          case NonFatal(error) =>
            write(
              directory,
              "failure.json",
              Json.obj(
                "error" -> Json.fromString(error.toString)
              )
            )
            System.err.println(
              "Private fixture failed; details are in the external run directory."
            )
            sys.exit(1)
