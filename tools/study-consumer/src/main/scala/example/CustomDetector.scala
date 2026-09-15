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

package example

import eyes4s.codec.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.plan.*
import io.circe.Json

/** Extension-owned parameters for a registered laboratory I-VT method. */
final class LabIvtParameters private (
    val threshold: IvtThreshold,
    val minimumDuration: MinimumEventDuration
)

object LabIvtParameters:
  def of(
      thresholdDegreesPerSecond: Double,
      minimumDurationMicros: Long
  ): Either[String, LabIvtParameters] =
    for
      velocity <- Velocity
        .perSecond[Deg](thresholdDegreesPerSecond)
        .left
        .map(_.message)
      threshold <- IvtThreshold.of(velocity).left.map(_.message)
      minimum   <- MinimumEventDuration
        .of(Span.micros(minimumDurationMicros))
        .left
        .map(_.message)
    yield new LabIvtParameters(threshold, minimum)

object CustomDetector:
  def method(id: DefinitionId): RecordingMethod[LabIvtParameters] =
    new RecordingMethod(
      id,
      parameters = p =>
        Vector(
          "thresholdDegPerSecond" -> Provenance.Param.Num(p.threshold.velocity.value),
          "minimumDurationMicros" -> Provenance.Param.Text(
            p.minimumDuration.span.toMicros.toString
          )
        ),
      detector = (p, clock) => Right(Detectors.ivt(p.threshold, p.minimumDuration, clock))
    )

  def parameterCodec(schema: DefinitionId): VersionedCodec[LabIvtParameters] =
    VersionedCodec.of[LabIvtParameters](schema)(p =>
      Json.obj(
        "thresholdDegPerSecond" -> Json.fromDoubleOrNull(p.threshold.velocity.value),
        "minimumDurationMicros" -> Json.fromString(p.minimumDuration.span.toMicros.toString)
      )
    ) { json =>
      for
        threshold <- json.hcursor
          .get[Double]("thresholdDegPerSecond")
          .left
          .map(e => CodecError.Field("thresholdDegPerSecond", json, e.message))
        durationText <- json.hcursor
          .get[String]("minimumDurationMicros")
          .left
          .map(e => CodecError.Field("minimumDurationMicros", json, e.message))
        duration <- durationText.toLongOption.toRight(
          CodecError.Field(
            "minimumDurationMicros",
            json,
            s"invalid integer microseconds '$durationText'"
          )
        )
        parameters <- LabIvtParameters
          .of(threshold, duration)
          .left
          .map(e => CodecError.Field("parameters", json, e))
      yield parameters
    }

  def persistence(
      schema: DefinitionId,
      methodId: DefinitionId,
      parameterSchema: DefinitionId
  ): RecordingPlanCodec[LabIvtParameters] =
    new RecordingPlanCodec(schema, method(methodId), parameterCodec(parameterSchema))
