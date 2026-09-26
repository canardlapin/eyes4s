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
  /** The threshold through the library's own smart constructors. */
  def threshold(degreesPerSecond: Double): Either[String, IvtThreshold] =
    Velocity
      .perSecond[Deg](degreesPerSecond)
      .left
      .map(_.message)
      .flatMap(IvtThreshold.of(_).left.map(_.message))

  /** The minimum event duration through the library's own smart constructor. */
  def minimum(micros: Long): Either[String, MinimumEventDuration] =
    MinimumEventDuration.of(Span.micros(micros)).left.map(_.message)

  def of(
      thresholdDegreesPerSecond: Double,
      minimumDurationMicros: Long
  ): Either[String, LabIvtParameters] =
    for
      velocity <- threshold(thresholdDegreesPerSecond)
      duration <- minimum(minimumDurationMicros)
    yield new LabIvtParameters(velocity, duration)

object CustomDetector:
  /** The extension's own typed field for its threshold: raw degrees per
    * second in, the library's `IvtThreshold` out, through the same smart
    * constructors the parameters use; its error type is the extension's.
    */
  def thresholdField
      : Either[DescriptorError, ParameterDescriptor[Double, IvtThreshold, String]] =
    for
      id    <- FieldId.of("thresholdDegPerSecond")
      field <- NumericField.of[String, Double, IvtThreshold](
        id,
        1,
        "Conservative I-VT velocity threshold on the angular samples",
        Quantity.Rate(PlanarUnit.Deg),
        NumericBounds.positive
      )(LabIvtParameters.threshold, _.velocity.value, identity)
    yield new ParameterDescriptor(
      ParameterInfo.of(field.view),
      LabIvtParameters.threshold,
      identity,
      None,
      Some(field)
    )

  /** The extension's own typed field for its minimum duration in microseconds. */
  def minimumField
      : Either[DescriptorError, ParameterDescriptor[Long, MinimumEventDuration, String]] =
    for
      id    <- FieldId.of("minimumDurationMicros")
      field <- NumericField.of[String, Long, MinimumEventDuration](
        id,
        1,
        "Minimum event duration; shorter candidates stay unclassified",
        Quantity.Duration,
        NumericBounds.positive
      )(LabIvtParameters.minimum, _.span.toMicros, identity)
    yield new ParameterDescriptor(
      ParameterInfo.of(field.view),
      LabIvtParameters.minimum,
      identity,
      None,
      Some(field)
    )

  /** The detector's descriptor: its typed fields and the algorithm card of
    * the canonical I-VT machine it wraps, which is the card its detections
    * carry. The recording descriptor's execution capability is the library's
    * (a whole synchronous operation per step); the runner still feeds the
    * machine in sample chunks.
    */
  def descriptor(
      id: DefinitionId
  ): Either[DescriptorError, RecordingMethodDescriptor[LabIvtParameters]] =
    for
      threshold <- thresholdField
      minimum   <- minimumField
      fields    <- ParameterSet.of(
        Vector(
          threshold.bind[LabIvtParameters](_.threshold)(t =>
            Provenance.Param.Num(t.velocity.value)
          ),
          minimum.bind[LabIvtParameters](_.minimumDuration)(m =>
            Provenance.Param.Text(m.span.toMicros.toString)
          )
        )
      )
    yield new RecordingMethodDescriptor(id, fields, AlgorithmCards.ivt)

  private def detector(
      p: LabIvtParameters,
      clock: ClockId
  ): Either[DetectorDefinitionError, EventDetector[Deg]] =
    Right(Detectors.ivt(p.threshold, p.minimumDuration, clock))

  /** The method without a descriptor: it runs, and preflight names it as an
    * undescribed method, a warning rather than a blocker.
    */
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
      detector = detector
    )

  /** The described method: its provenance parameters are the descriptor's own values. */
  def describedMethod(
      id: DefinitionId
  ): Either[DescriptorError, RecordingMethod[LabIvtParameters]] =
    descriptor(id).map(d => new RecordingMethod(id, d.parameters.values, detector, Some(d)))

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

  /** The registered plan codec of the described method; `results` is its
    * `recording-result@1` archive.
    */
  def persistence(
      schema: DefinitionId,
      methodId: DefinitionId,
      parameterSchema: DefinitionId
  ): Either[DescriptorError, RecordingPlanCodec[LabIvtParameters]] =
    describedMethod(methodId).map(
      new RecordingPlanCodec(schema, _, parameterCodec(parameterSchema))
    )
