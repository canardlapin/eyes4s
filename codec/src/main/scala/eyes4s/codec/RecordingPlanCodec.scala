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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Px, Deg}
import eyes4s.plan.*
import io.circe.Json

/** Recording plan codecs for the built-in detectors. Each fixes the method identity and
  * the parameter schema, so a saved plan names exactly which detector and units it uses.
  */
object RecordingCodecs:
  /** I-DT parameters are angular per-axis extents and exact microsecond duration. */
  def idt(
      schema: DefinitionId,
      methodId: DefinitionId,
      parameterSchema: DefinitionId
  ): RecordingPlanCodec[IdtParameters] =
    new RecordingPlanCodec(
      schema,
      RecordingMethod.idt(methodId),
      VersionedCodec.of[IdtParameters](parameterSchema)(p =>
        Json.obj(
          "extentWidthDeg"  -> Json.fromDoubleOrNull(p.extent.width),
          "extentHeightDeg" -> Json.fromDoubleOrNull(p.extent.height),
          "minimumMicros"   -> Json.fromString(p.minimumDuration.span.toMicros.toString)
        )
      ) { json =>
        for
          width  <- Wire.field[Double](json, "extentWidthDeg")
          height <- Wire.field[Double](json, "extentHeightDeg")
          extent <- Extent
            .of[Deg](width, height)
            .left
            .map(e => CodecError.Field("extent", json, e.message))
          duration <- RecordingWire.micros(json, "minimumMicros")
          minimum  <- MinimumEventDuration
            .of(Span.micros(duration))
            .left
            .map(e => CodecError.Field("minimumMicros", json, e.message))
        yield IdtParameters(extent, minimum)
      }
    )

  /** Fixed thresholds, not a lambda or an instruction to estimate from input. */
  def engbertKliegl(
      schema: DefinitionId,
      methodId: DefinitionId,
      parameterSchema: DefinitionId
  ): RecordingPlanCodec[EkParameters] =
    new RecordingPlanCodec(
      schema,
      RecordingMethod.engbertKliegl(methodId),
      VersionedCodec.of[EkParameters](parameterSchema)(p =>
        Json.obj(
          "etaXDegPerSecond" -> Json.fromDoubleOrNull(p.thresholds.etaX),
          "etaYDegPerSecond" -> Json.fromDoubleOrNull(p.thresholds.etaY),
          "minimumSamples"   -> Json.fromInt(p.minimumSamples.value)
        )
      ) { json =>
        for
          x          <- Wire.field[Double](json, "etaXDegPerSecond")
          y          <- Wire.field[Double](json, "etaYDegPerSecond")
          thresholds <- EkThresholds
            .of(x, y)
            .left
            .map(e => CodecError.Field("thresholds", json, e.message))
          n       <- Wire.field[Int](json, "minimumSamples")
          minimum <- EkMinimumSamples
            .of(n)
            .left
            .map(e => CodecError.Field("minimumSamples", json, e.message))
        yield EkParameters(thresholds, minimum)
      }
    )

  def ivt(
      schema: DefinitionId,
      methodId: DefinitionId,
      parameterSchema: DefinitionId
  ): RecordingPlanCodec[IvtParameters] =
    new RecordingPlanCodec(
      schema,
      RecordingMethod.ivt(methodId),
      VersionedCodec.of[IvtParameters](parameterSchema)(p =>
        Json.obj(
          "threshold"     -> Json.fromDoubleOrNull(p.threshold.velocity.value),
          "minimumMicros" -> Json.fromString(p.minimumDuration.span.toMicros.toString)
        )
      ) { json =>
        for
          raw      <- Wire.field[Double](json, "threshold")
          velocity <- Velocity
            .perSecond[Deg](raw)
            .left
            .map(e => CodecError.Field("threshold", json, e.message))
          threshold <- IvtThreshold
            .of(velocity)
            .left
            .map(e => CodecError.Field("threshold", json, e.message))
          duration <- RecordingWire.micros(json, "minimumMicros")
          minimum  <- MinimumEventDuration
            .of(Span.micros(duration))
            .left
            .map(e => CodecError.Field("minimumMicros", json, e.message))
        yield IvtParameters(threshold, minimum)
      }
    )

private[codec] object RecordingWire:
  def micros(json: Json, field: String): Either[CodecError, Long] =
    DomainWire.micros(json, field)

/** Versioned codec for a `RecordingPlan` of one detector method. Encoding refuses a plan of
  * another method (`CodecError.Schema`). Decoding checks the method, reads the parameters
  * through their own versioned codec, and rebuilds the plan through `RecordingPlan.of`,
  * so every plan invariant is the constructor's own refusal (`CodecError.Field`). Display
  * areas are pixel bounds; times are integer microseconds.
  */
final class RecordingPlanCodec[P](
    val schema: DefinitionId,
    val method: RecordingMethod[P],
    val parameters: VersionedCodec[P]
):
  val codec: VersionedCodec[RecordingPlan[P]] = VersionedCodec.checked[RecordingPlan[P]](
    schema
  )(plan =>
    for
      _ <- Either
        .cond(plan.method.id == method.id, (), CodecError.Schema(method.id, plan.method.id))
      encoded <- parameters.encode(plan.parameters)
    yield Json.obj(
      "method"        -> Wire.id(method.id),
      "parameters"    -> encoded,
      "input"         -> Json.fromString(plan.input.digest),
      "source"        -> Json.fromString(plan.source.value),
      "display"       -> StudyCodecs.frame(plan.display),
      "trackerClock"  -> Json.fromString(plan.trackerClock.name),
      "analysisClock" -> Json.fromString(plan.analysisClock.name),
      "angularFrame"  -> Json.fromString(plan.angularFrameId.name),
      "viewing"   -> plan.viewing.fold(Json.Null)(v => DomainWire.perspective(v.perspective)),
      "syncModel" -> Json.fromString(plan.synchronizationModel.toString),
      "marks"     -> Json.arr(plan.marks.map(DomainWire.mark)*),
      "residualLimitMicros" -> plan.residualLimit.fold(Json.Null)(l =>
        Json.fromString(l.span.toMicros.toString)
      ),
      "interpolationGapMicros" -> Json.fromString(plan.interpolationGap.span.toMicros.toString),
      "areas"                  -> Json.arr(
        plan.areas.map(a =>
          Json.obj(
            "id"    -> Json.fromString(a.id),
            "label" -> Json.fromString(a.label),
            "xMin"  -> Json.fromDoubleOrNull(a.bounds.xMin),
            "yMin"  -> Json.fromDoubleOrNull(a.bounds.yMin),
            "xMax"  -> Json.fromDoubleOrNull(a.bounds.xMax),
            "yMax"  -> Json.fromDoubleOrNull(a.bounds.yMax)
          )
        )*
      )
    )
  ) { json =>
    for
      methodId <- Wire.definition(json, "method")
      _        <- Either.cond(methodId == method.id, (), CodecError.Schema(method.id, methodId))
      encoded  <- Wire.field[Json](json, "parameters")
      params   <- parameters.decode(encoded)
      digest   <- Wire.field[String](json, "input")
      input    <- ArtifactRef.parse[Recording[Px]](digest).left.map(CodecError.Definition.apply)
      source   <- Wire.field[String](json, "source")
      displayJson <- Wire.field[Json](json, "display")
      display     <- StudyCodecs.readFrame[Px](displayJson)
      tracker     <- Wire.field[String](json, "trackerClock")
      analysis    <- Wire.field[String](json, "analysisClock")
      angular     <- Wire.field[String](json, "angularFrame")
      viewingJson <- Wire.field[Option[Json]](json, "viewing")
      viewing     <- viewingJson.traverse(value =>
        DomainWire.readPerspective(value).map(Viewing.apply)
      )
      modelName <- Wire.field[String](json, "syncModel")
      model     <- SyncFitMode.values
        .find(_.toString == modelName)
        .toRight(CodecError.Field("syncModel", json, s"unknown model '$modelName'"))
      rawMarks <- Wire.field[Vector[Json]](json, "marks")
      marks    <- rawMarks.traverse(DomainWire.readMark)
      rawLimit <- Wire.field[Option[Json]](json, "residualLimitMicros")
      limit    <- rawLimit.traverse(text =>
        for
          value <- DomainWire.readTime(text, "residualLimitMicros")
          limit <- SyncResidualLimit
            .of(Span.micros(value))
            .left
            .map(e => CodecError.Field("residualLimitMicros", json, e.message))
        yield limit
      )
      gapMicros <- RecordingWire.micros(json, "interpolationGapMicros")
      gap       <- InterpolationGap
        .of(Span.micros(gapMicros))
        .left
        .map(e => CodecError.Field("interpolationGapMicros", json, e.message))
      rawAreas <- Wire.field[Vector[Json]](json, "areas")
      areas    <- rawAreas.traverse { value =>
        for
          id     <- Wire.field[String](value, "id")
          label  <- Wire.field[String](value, "label")
          x0     <- Wire.field[Double](value, "xMin")
          y0     <- Wire.field[Double](value, "yMin")
          x1     <- Wire.field[Double](value, "xMax")
          y1     <- Wire.field[Double](value, "yMax")
          bounds <- Bounds
            .of[Px](x0, y0, x1, y1)
            .left
            .map(e => CodecError.Field("area", value, e.message))
          area <- RecordingArea
            .of(id, label, bounds)
            .left
            .map(e => CodecError.Field("area", value, e.message))
        yield area
      }
      plan <- RecordingPlan
        .of(
          input,
          RecordingRef(source),
          display,
          ClockId(tracker),
          ClockId(analysis),
          FrameId(angular),
          viewing,
          model,
          marks,
          limit,
          gap,
          areas,
          method,
          params
        )
        .left
        .map(e => CodecError.Field("recordingPlan", json, e.message))
    yield plan
  }

  /** The result archive for this recording plan family. */
  def results: RecordingResultCodec[P] =
    new RecordingResultCodec(DefinitionId.recordingResult, this)

  def registration: RecordingRegistration =
    val registered = this
    new RecordingRegistration:
      val methodId: DefinitionId                                  = registered.method.id
      override def schema: Option[DefinitionId]                   = Some(registered.schema)
      def decode(json: Json): Either[CodecError, LoadedRecording] =
        registered.codec.decode(json).map { value =>
          new LoadedRecording:
            type Parameters = P
            val plan: RecordingPlan[P]           = value
            def encode: Either[CodecError, Json] = registered.codec.encode(value)
        }

/** A decoded recording plan whose detector parameter type stays abstract but fixed. */
trait LoadedRecording:
  /** The detector parameter type of the decoded method. */
  type Parameters
  val plan: RecordingPlan[Parameters]
  def encode: Either[CodecError, Json]

/** One recording plan codec as a registry sees it: the method it decodes and a decoder. */
trait RecordingRegistration:
  val methodId: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedRecording]

  /** The envelope schema this registration decodes, when it declares one. */
  def schema: Option[DefinitionId] = None

/** Recording plan codecs by method identity; lookup reads the payload's `method` and
  * refuses a missing (`CodecError.MissingMethod`) or duplicate
  * (`CodecError.DuplicateMethod`) registration.
  */
final class RecordingRegistry private (definitions: Vector[RecordingRegistration]):
  def register(definition: RecordingRegistration): Either[CodecError, RecordingRegistry] =
    if definitions.exists(_.methodId == definition.methodId) then
      Left(CodecError.DuplicateMethod(definition.methodId))
    else Right(new RecordingRegistry(definitions :+ definition))

  /** The envelope schemas the registrations declare. */
  def schemas: Vector[DefinitionId] = definitions.flatMap(_.schema).distinct

  def decode(json: Json): Either[CodecError, LoadedRecording] = for
    value      <- Wire.field[Json](json, "value")
    id         <- Wire.definition(value, "method")
    definition <- definitions.find(_.methodId == id).toRight(CodecError.MissingMethod(id))
    loaded     <- definition.decode(json)
  yield loaded
object RecordingRegistry:
  val empty: RecordingRegistry = new RecordingRegistry(Vector.empty)
