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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.{Json, parser}
import java.nio.file.{Files, Path, StandardOpenOption}

/** Keeps every requested pair, including failed and too-short operands. */
object PrivateMultiMatchCli:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalArgumentException(e.toString), identity)
  def main(args: Array[String]): Unit =
    val root   = Path.of(sys.env("EYES4S_PRIVATE_RUN"))
    val cursor = get(
      parser.parse(Files.readString(root.resolve("multimatch-request.json")))
    ).hcursor
    val frame = get(
      Frame.screen(
        "private-multimatch-screen",
        get(cursor.get[Int]("screen_width_px")),
        get(cursor.get[Int]("screen_height_px"))
      )
    )
    val clock                      = ClockId("private-multimatch-trial-relative")
    def str(j: Json, name: String) = get(j.hcursor.get[String](name))
    def path(j: Json): Either[String, Scanpath[Px]] =
      val f                    = j.hcursor.downField("fixations")
      def column(name: String) = get(f.get[Vector[Double]](name))
      val x                    = column("x"); val y               = column("y")
      val onset                = column("onset_ms"); val duration = column("duration_ms")
      import cats.syntax.all.*
      x.indices.toVector
        .traverse { i =>
          val start          = math.round(onset(i) * 1000)
          val span           = math.round(duration(i) * 1000)
          val derivedSamples = math.max(
            1,
            math
              .min(
                Int.MaxValue.toDouble,
                math.ceil(span * get(cursor.get[Double]("sample_rate_hz")) / 1000000.0)
              )
              .toInt
          )
          for
            interval <- Interval
              .of(clock, Instant.micros(start), Instant.micros(start + span))
              .leftMap(_.toString)
            fixation <- Event.Fixation
              .of(interval, Pt[Px](x(i), y(i)), 0, DispersionMethod.RmsRadius, derivedSamples)
              .leftMap(_.toString)
          yield fixation
        }
        .flatMap(fs => Scanpath.of(frame, clock, IArray.from(fs)).leftMap(_.toString))
    val items =
      get(cursor.get[Vector[Json]]("templates")) ++ get(cursor.get[Vector[Json]]("sources"))
    val paths  = items.map(j => str(j, "id") -> path(j)).toMap
    val output = Files.newBufferedWriter(
      root.resolve("multimatch-eyes4s.jsonl"),
      StandardOpenOption.CREATE_NEW
    )
    try
      get(cursor.get[Vector[Json]]("pairs")).foreach { pair =>
        val result = for
          left  <- paths(str(pair, "source"))
          right <- paths(str(pair, "reference"))
          a     <- BaselineScanpath.of(left, CompareOperand.Left).left.map(_.message)
          b     <- BaselineScanpath.of(right, CompareOperand.Right).left.map(_.message)
          score <- ScanpathComparison.baseline(a, b).left.map(_.message)
        yield score
        val value = result.fold(
          error => Json.obj("error" -> Json.fromString(error)),
          score =>
            Json.obj(
              "values" -> Json.obj(
                Vector(
                  "shape"     -> score.shape,
                  "direction" -> score.direction,
                  "length"    -> score.length,
                  "position"  -> score.position,
                  "duration"  -> score.duration
                )
                  .map((key, v) => key -> Json.fromDoubleOrNull(v))*
              )
            )
        )
        output.write(pair.deepMerge(value).noSpaces)
        output.newLine()
      }
    finally output.close()
