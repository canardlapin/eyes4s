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

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

class RecordingMethodsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(e => fail(s"$e"), identity)
  private def id(name: String): DefinitionId = get(DefinitionId.of(name, 1))
  private val idt = RecordingCodecs.idt(id("recording"), id("idt"), id("idt.parameters"))
  private val ek = RecordingCodecs.engbertKliegl(id("recording"), id("ek"), id("ek.parameters"))
  private val duration      = get(MinimumEventDuration.of(Span.micros(20000)))
  private val idtParameters = IdtParameters(get(Extent.of[Deg](0.2, 0.4)), duration)
  private val ekParameters  =
    EkParameters(get(EkThresholds.of(1, 2)), get(EkMinimumSamples.of(3)))
  private val frame  = get(Frame.screen("display", 1000, 1000))
  private val clock  = ClockId("tracker")
  private val source = RecordingRef("analytic-recording")

  private def recording(moving: Boolean, irregular: Boolean = false): Recording[Px] = get(
    Recording.of(
      frame,
      clock,
      if irregular then Rate.Irregular else Rate.Fixed(get(Hz(100))),
      Eye.Left,
      None,
      IArray.from((0 until 10).map { i =>
        val time = i * 10000L + (if irregular && i >= 5 then 1000L else 0L)
        Sample(
          Instant.micros(time),
          Gaze.Tracked(Pt[Px](500 + (if moving then i * 10 else 0), 500), None)
        )
      })
    )
  )
  private def plan[P](
      input: Recording[Px],
      method: RecordingMethod[P],
      params: P
  ): RecordingPlan[P] = get(
    RecordingPlan.of(
      ArtifactRef.of(input.contentHash),
      source,
      frame,
      clock,
      ClockId("analysis"),
      FrameId("angular"),
      Some(get(Viewing.millimetres(600, 500, 500))),
      SyncFitMode.OffsetOnly,
      Vector(get(SyncMark.of("start", Instant.micros(0), Instant.micros(1000)))),
      None,
      InterpolationGap.none,
      Vector(get(RecordingArea.of("screen", "Screen", frame.bounds))),
      method,
      params
    )
  )

  private def sameDetection(a: DetectionResult[Deg], b: DetectionResult[Deg]): Unit =
    assertEquals(a.identity, b.identity)
    assertEquals(a.eventSeries.events, b.eventSeries.events)
    assertEquals(a.eventSeries.support, b.eventSeries.support)
    assertEquals(a.labels.toVector, b.labels.toVector)
    assertEquals(a.report, b.report)
    assertEquals(a.provenance, b.provenance)

  private def verify[P](
      input: Recording[Px],
      codec: RecordingPlanCodec[P],
      params: P,
      direct: ClockId => EventDetector[Deg]
  ): RecordingAnalysis[P] =
    val original = plan(input, codec.method, params)
    val restored = get(codec.codec.parse(get(codec.codec.encode(original)).noSpaces))
    assertEquals(restored.diff(original), Vector.empty)
    assertEquals(get(restored.inspect).description, get(original.inspect).description)
    assert(original.preflight(Some(input)).ready)
    assert(restored.preflight(Some(input)).ready)
    val result    = get(original.run(input))
    val replayed  = get(restored.run(input))
    val canonical = get(
      Detection.run(
        source,
        result.prepared,
        direct(result.prepared.clock),
        GapPolicy.Break,
        result.prepared.representedSupport.policy
      )
    )
    sameDetection(result.detection, canonical)
    sameDetection(replayed.detection, canonical)
    assertEquals(result.prepared.contentHash, replayed.prepared.contentHash)
    result

  test("I-DT direct, interpreted and restored detection agree with stationary event oracle") {
    val result = verify(
      recording(false),
      idt,
      idtParameters,
      c => Detectors.idt(idtParameters.extent, duration, c)
    )
    assertEquals(result.detection.identity.algorithmCard, Some(AlgorithmCards.idt))
    assertEquals(result.detection.labels.toVector, Vector.fill(10)(SampleClass.Fixation))
    assertEquals(
      result.detection.eventSeries.events.map(e =>
        (e.span.onset.toMicros, e.span.offset.toMicros)
      ),
      Vector(1000L -> 101000L)
    )
  }

  test(
    "fixed-threshold EK preserves five-point interior support, without estimating a zero Y spread"
  ) {
    // The straight horizontal trajectory has degenerate Y variance. An implicit
    // whole-trial threshold fit would fail; the supplied thresholds must be used.
    val result = verify(
      recording(true),
      ek,
      ekParameters,
      c => Detectors.engbertKliegl(ekParameters.thresholds, ekParameters.minimumSamples, c)
    )
    assertEquals(result.detection.identity.algorithmCard, Some(AlgorithmCards.engbertKliegl))
    assertEquals(
      result.detection.labels.toVector,
      Vector.fill(2)(SampleClass.Unclassified) ++ Vector.fill(6)(SampleClass.Saccade) ++ Vector
        .fill(2)(SampleClass.Unclassified)
    )
    assertEquals(
      result.detection.eventSeries.events.map(e =>
        (e.span.onset.toMicros, e.span.offset.toMicros)
      ),
      Vector(21000L -> 81000L)
    )
  }

  test("axis thresholds and minimum sample count change the actual detector") {
    val input = recording(true)
    val noX   = EkParameters(get(EkThresholds.of(1000, 1)), ekParameters.minimumSamples)
    val highY = EkParameters(get(EkThresholds.of(1, 1000)), ekParameters.minimumSamples)
    assert(get(plan(input, ek.method, noX).run(input)).detection.eventSeries.events.isEmpty)
    assertEquals(
      get(plan(input, ek.method, highY).run(input)).detection.eventSeries.events.size,
      1
    )
    val tooLong = ekParameters.copy(minimumSamples = get(EkMinimumSamples.of(7)))
    assert(get(plan(input, ek.method, tooLong).run(input)).detection.eventSeries.events.isEmpty)
    val narrow = IdtParameters(get(Extent.of[Deg](0.1, 10)), duration)
    val wide   = IdtParameters(get(Extent.of[Deg](10, 0.1)), duration)
    assert(get(plan(input, idt.method, narrow).run(input)).detection.eventSeries.events.isEmpty)
    assertEquals(
      get(plan(input, idt.method, wide).run(input)).detection.eventSeries.events.size,
      1
    )
    val long = wide.copy(minimumDuration = get(MinimumEventDuration.of(Span.micros(200000))))
    assert(get(plan(input, idt.method, long).run(input)).detection.eventSeries.events.isEmpty)
  }

  test("irregular sampling remains a typed EK failure after persistence") {
    val input    = recording(true, irregular = true)
    val original = plan(input, ek.method, ekParameters)
    val restored = get(ek.codec.decode(get(ek.codec.encode(original))))
    val failure  = original.run(input).left.toOption
    assertEquals(restored.run(input).left.toOption, failure)
    assert(failure.exists {
      case RecordingPlanError.Detection(
            DetectionResultError.DetectorEmissionFailed(_, _, _, DetectionFailure.Kinematics(_))
          ) =>
        true
      case _ => false
    })
  }

  test("parameter wire shapes independently fix axes, units and exact duration") {
    val exact = idtParameters.copy(minimumDuration =
      get(MinimumEventDuration.of(Span.micros(9007199254740993L)))
    )
    val encoded = get(idt.parameters.encode(exact))
    assertEquals(
      get(encoded.hcursor.get[Json]("value")),
      Json.obj(
        "extentWidthDeg"  -> Json.fromDoubleOrNull(0.2),
        "extentHeightDeg" -> Json.fromDoubleOrNull(0.4),
        "minimumMicros"   -> Json.fromString("9007199254740993")
      )
    )
    assertEquals(get(idt.parameters.decode(encoded)), exact)
    assertEquals(
      get(get(ek.parameters.encode(ekParameters)).hcursor.get[Json]("value")),
      Json.obj(
        "etaXDegPerSecond" -> Json.fromInt(1),
        "etaYDegPerSecond" -> Json.fromInt(2),
        "minimumSamples"   -> Json.fromInt(3)
      )
    )
  }

  private def changedValue(json: Json, key: String, value: Json): Json =
    val original = get(json.hcursor.get[Json]("value"))
    json.mapObject(_.add("value", original.mapObject(_.add(key, value))))

  test("malformed extents, thresholds, counts and durations are refused") {
    val i = get(idt.parameters.encode(idtParameters))
    val e = get(ek.parameters.encode(ekParameters))
    for
      key <- Vector("extentWidthDeg", "extentHeightDeg");
      bad <- Vector(Json.fromInt(0), Json.fromInt(-1), Json.Null, Json.fromString("NaN"))
    do assert(idt.parameters.decode(changedValue(i, key, bad)).isLeft)
    for bad <- Vector("0", "-1", "9223372036854775808", "0.5") do
      assert(
        idt.parameters.decode(changedValue(i, "minimumMicros", Json.fromString(bad))).isLeft
      )
    for
      key <- Vector("etaXDegPerSecond", "etaYDegPerSecond");
      bad <- Vector(Json.fromInt(0), Json.fromInt(-1), Json.Null)
    do assert(ek.parameters.decode(changedValue(e, key, bad)).isLeft)
    for bad <- Vector(
        Json.fromInt(0),
        Json.fromInt(-1),
        Json.fromDoubleOrNull(1.5),
        Json.fromLong(2147483648L)
      )
    do assert(ek.parameters.decode(changedValue(e, "minimumSamples", bad)).isLeft)
  }

  test("unknown method and parameter schema versions cannot replay under another detector") {
    val original = plan(recording(false), idt.method, idtParameters)
    val encoded  = get(idt.codec.encode(original))
    assertEquals(
      ek.codec.decode(encoded).left.toOption,
      Some(CodecError.Schema(id("ek"), id("idt")))
    )
    val unknown =
      Json.obj("name" -> Json.fromString("missing-detector"), "version" -> Json.fromInt(1))
    assertEquals(
      idt.codec.decode(changedValue(encoded, "method", unknown)).left.toOption,
      Some(CodecError.Schema(id("idt"), id("missing-detector")))
    )
    val parameters  = get(idt.parameters.encode(idtParameters))
    val wrongSchema = parameters.mapObject(
      _.add(
        "schema",
        Json.obj("name" -> Json.fromString("idt.parameters"), "version" -> Json.fromInt(2))
      )
    )
    assertEquals(
      idt.codec.decode(changedValue(encoded, "parameters", wrongSchema)).left.toOption,
      Some(CodecError.Schema(id("idt.parameters"), get(DefinitionId.of("idt.parameters", 2))))
    )
  }

  test("descriptors expose physical units, validation and synchronous capability") {
    val d = get(idt.method.descriptor.toRight("missing descriptor"))
    val e = get(ek.method.descriptor.toRight("missing descriptor"))
    assertEquals(d.execution, ExecutionCapability.SynchronousWholeOperation)
    assertEquals(e.execution, ExecutionCapability.SynchronousWholeOperation)
    assertEquals(
      d.parameters.fields.map(_.descriptor.info.units),
      Vector(
        ParameterUnits.Spatial("deg"),
        ParameterUnits.Spatial("deg"),
        ParameterUnits.Microseconds
      )
    )
    assertEquals(
      e.parameters.fields.map(_.descriptor.info.units),
      Vector(
        ParameterUnits.PerSecond("deg"),
        ParameterUnits.PerSecond("deg"),
        ParameterUnits.Dimensionless
      )
    )
    for bad <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      assert(RecipeParameters.idtWidth.parse(bad).isLeft)
      assert(RecipeParameters.idtHeight.parse(bad).isLeft)
      assert(RecipeParameters.ekEtaX.parse(bad).isLeft)
      assert(RecipeParameters.ekEtaY.parse(bad).isLeft)
    assert(RecipeParameters.ekMinimumSamples.parse(0).isLeft)
  }

  test(
    "heterogeneous recording registry replays both methods and refuses absent or duplicate registrations"
  ) {
    val registry = get(
      RecordingRegistry.empty.register(idt.registration).flatMap(_.register(ek.registration))
    )
    val stationary = recording(false)
    val moving     = recording(true)
    val i          = plan(stationary, idt.method, idtParameters)
    val e          = plan(moving, ek.method, ekParameters)
    val cases      = Vector(
      (get(idt.codec.encode(i)), stationary, get(i.run(stationary)).detection),
      (get(ek.codec.encode(e)), moving, get(e.run(moving)).detection)
    )
    cases.foreach { case (json, input, expected) =>
      val loaded = get(registry.decode(json))
      assertEquals(get(loaded.encode), json)
      assert(loaded.plan.preflight(Some(input)).ready)
      sameDetection(get(loaded.plan.run(input)).detection, expected)
    }
    assertEquals(
      RecordingRegistry.empty.decode(cases.head._1).left.toOption,
      Some(CodecError.MissingMethod(id("idt")))
    )
    assertEquals(
      registry.register(ek.registration).left.toOption,
      Some(CodecError.DuplicateMethod(id("ek")))
    )
  }

  test("pixel extents cannot enter an angular recording method") {
    assert(typeCheckErrors("""
      import eyes4s.plan.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      import eyes4s.detect.*
      def invalid(e: Extent[Px], d: MinimumEventDuration) = IdtParameters(e, d)
    """).nonEmpty)
  }
