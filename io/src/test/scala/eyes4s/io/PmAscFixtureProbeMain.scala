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

import java.nio.file.{Files, Path}

/** Correctness-only probe of a generated ASC input, outside timed measurements. */
object PmAscFixtureProbeMain:
  def main(args: Array[String]): Unit =
    if args.length != 3 then sys.error("usage: PmAscFixtureProbeMain INPUT ROWS mono|binocular")
    val path     = Path.of(args(0))
    val expected = args(1).toInt
    val layout   = args(2)
    if expected < 1 || (layout != "mono" && layout != "binocular") then
      sys.error("invalid expected row count or layout")

    val bytes    = IArray.from(Files.readAllBytes(path))
    val settings = AscStreamSettings
      .of(path.toString, 65536, 65536)
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
      result.report.parsedSamples != expected || result.report.materializedSamples != expected
    then sys.error(s"ASC accounting failed: ${result.report}")
    val session = result.trusted.fold(
      errors => sys.error(errors.toVector.map(_.message).mkString("; ")),
      identity
    )
    if session.recordingBlocks.length != 1 || session.observedMessages.marks.length != 3
    then sys.error("ASC blocks or messages changed")
    val recording    = session.recordingBlocks.head.recording
    val actualLayout = recording match
      case EyeLinkAscRecordingArtifact.Monocular(value) =>
        if value.size != expected || value.pupilUnit != Some(PupilUnit.Area) then
          sys.error("monocular sample or pupil identity changed")
        "mono"
      case EyeLinkAscRecordingArtifact.Binocular(value) =>
        if value.size != expected || value.pupilUnit != Some(PupilUnit.Area) then
          sys.error("binocular sample or pupil identity changed")
        "binocular"
    if actualLayout != layout then sys.error("ASC eye layout changed")
    println(
      s"{\"sha256\":\"${Sha256.ofBytes(bytes).hex}\",\"rows\":$expected," +
        s"\"messages\":3,\"layout\":\"$actualLayout\"}"
    )

end PmAscFixtureProbeMain
