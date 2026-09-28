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

import eyes4s.core.*
import eyes4s.kernel.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Locale

/** Correctness preflight for the PM3.4 materialized binocular ASC adapters. */
object PmAscAdmissionMain:
  private val header   = "time_us,eye,x_px,y_px,pupil,status\n"
  private val messages = Vector(
    "RECCFG CR 1000 2 2 2 2 LR",
    "GAZE_COORDS 0 0 1919 1079",
    "DISPLAY_COORDS 0 0 1919 1079"
  )

  private def decimal(value: Double): String =
    String.format(Locale.ROOT, "%.6f", Double.box(value))

  def main(args: Array[String]): Unit =
    if args.length != 3 then sys.error("usage: PmAscAdmissionMain INPUT OUTPUT EXPECTED_ROWS")
    val input    = Path.of(args(0))
    val output   = Path.of(args(1))
    val expected = args(2).toInt
    if expected < 1 then sys.error("expected row count must be positive")
    val bytes    = IArray.from(Files.readAllBytes(input))
    val settings = AscStreamSettings
      .of(input.toString, 65536, 65536)
      .fold(error => sys.error(error.message), identity)
    val frame = Frame
      .screen("pm3-asc-display", 1920, 1080)
      .fold(error => sys.error(error.message), identity)
    val config = EyeLinkAscSessionConfig
      .of(
        frame,
        ClockId("pm3-asc-tracker"),
        ConversionEvidencePolicy.Exploratory,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
        AscUnspecifiedPupilPolicy.RejectMeasuredValues
      )
      .fold(error => sys.error(error.message), identity)
    val result = EyeLinkAscImport.fromBytes(bytes, settings, config)
    if !result.report.isReconciled || result.report.sampleRecords != expected ||
      result.report.parsedSamples != expected || result.report.materializedSamples != expected ||
      result.report.errors != 0
    then sys.error(s"ASC admission failed: ${result.report}")
    val session = result.trusted.fold(
      errors => sys.error(errors.toVector.map(_.message).mkString("; ")),
      identity
    )
    val observedMessages = session.observedMessages.marks.map(
      _.value.payload.ascii.getOrElse(sys.error("non-ASCII fixture message"))
    )
    if session.recordingBlocks.length != 1 || observedMessages != messages then
      sys.error(
        s"ASC messages or block count changed: ${session.recordingBlocks.length}, $observedMessages"
      )
    val recording = session.recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Binocular(value) => value
      case EyeLinkAscRecordingArtifact.Monocular(_)     => sys.error("ASC eye layout changed")
    if recording.size != expected || recording.pupilUnit != Some(PupilUnit.Area) then
      sys.error("ASC rows or pupil unit changed")

    val writer = Files.newBufferedWriter(output, StandardCharsets.UTF_8)
    try
      writer.write(header)
      for index <- 0 until recording.size do
        val time = recording.timestamps(index).toMicros
        def emit(eye: String, gaze: Gaze[Unit2D.Px]): Unit = gaze match
          case Gaze.Tracked(position, Some(pupil)) =>
            writer.write(
              s"$time,$eye,${decimal(position.x)},${decimal(position.y)},${decimal(pupil)},tracked\n"
            )
          case Gaze.Lost() => writer.write(s"$time,$eye,,,,lost\n")
          case other       => sys.error(s"unsupported fixture gaze: $other")
        emit("left", recording.leftGaze(index))
        emit("right", recording.rightGaze(index))
    finally writer.close()
    val canonical = Sha256.ofBytes(IArray.from(Files.readAllBytes(output))).hex
    println(
      s"{\"input_sha256\":\"${Sha256.ofBytes(bytes).hex}\",\"output_sha256\":\"$canonical\"," +
        s"\"sample_rows\":$expected,\"output_rows\":${expected * 2},\"messages\":3,\"eyes\":2}"
    )

end PmAscAdmissionMain
