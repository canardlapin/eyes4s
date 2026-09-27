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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json

class DescriptorPersistenceSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private def id(n: String): DefinitionId   = get(DefinitionId.of(n, 1))
  private val frame                         = get(Frame.screen("display", 800, 600))
  private val grid                          = get(Grid.of(GridId("g"), frame, 10, 8))
  private val study                         = get(
    StudyPlan.cosine(
      ArtifactRef.of[StudyInput[StudyKey, Px]](ContentHash.empty),
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(10)), EdgePolicy.Truncate)
      ),
      FailurePolicy.RequireAll
    )
  )
  private val recordingCodec = RecordingCodecs.ivt(id("recording"), id("ivt"), id("ivt.params"))
  private val recording      = get(
    RecordingPlan.of(
      ArtifactRef.of[Recording[Px]](ContentHash.empty),
      RecordingRef("source"),
      frame,
      ClockId("tracker"),
      ClockId("analysis"),
      FrameId("angular"),
      Some(Viewing(get(Perspective.millimetres(600, 400, 300)))),
      SyncFitMode.OffsetOnly,
      Vector(get(SyncMark.of("trigger", Instant.micros(100), Instant.micros(200)))),
      Some(get(SyncResidualLimit.of(Span.micros(1000)))),
      InterpolationGap.none,
      Vector(get(RecordingArea.of("image", "Image", get(Bounds.of[Px](0, 0, 800, 600))))),
      recordingCodec.method,
      IvtParameters(
        get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
        get(MinimumEventDuration.of(Span.micros(60000)))
      )
    )
  )
  private val temporalCodec = new TemporalStudyCodec(id("temporal"), StudyCodecs.cosine[Px])
  private val temporal      = get(
    TemporalStudyPlan.of(
      study,
      ArtifactRef.of[TemporalStudyInput[StudyKey, Px]](ContentHash.empty),
      Vector(get(StudyWindow.of("early", get(Window.of(Span.zero, Span.micros(1000000)))))),
      Vector(get(RepetitionContrast.withinParticipant("recall", "recall", "encode"))),
      FixationBoundary.ClipDuration
    )
  )

  private def changed(j: Json, path: List[String], value: Json): Json = path match
    case Nil          => value
    case head :: tail =>
      j.asArray match
        case Some(xs) => Json.arr(xs.updated(head.toInt, changed(xs(head.toInt), tail, value))*)
        case None     =>
          j.mapObject(o =>
            o.add(head, changed(o(head).getOrElse(fail(s"missing $head")), tail, value))
          )

  private def checkChanges[A](
      codec: VersionedCodec[A],
      original: A,
      inspect: A => Either[DescriptorError, RecipeInspection],
      diff: (A, A) => Vector[PlanChange],
      mutations: Vector[(String, Json, String)]
  ): Unit =
    val json     = get(codec.encode(original))
    val baseline = get(inspect(original)).description.toMap
    assertEquals(get(inspect(get(codec.parse(json.noSpaces)))).description.toMap, baseline)
    mutations.foreach { case (path, replacement, expected) =>
      val restored =
        get(codec.decode(changed(json, ("value." + path).split('.').toList, replacement)))
      val fields = diff(restored, original).map(_.field)
      assertEquals(fields, Vector(expected), path)
      val described = get(inspect(restored)).description.toMap
      assertNotEquals(described(expected), baseline(expected), path)
      assertEquals(
        get(inspect(get(codec.decode(get(codec.encode(restored)))))).description.toMap,
        described
      )
    }

  test(
    "every study scientific field is described and persisted; individual changes remain visible"
  ) {
    assertEquals(
      get(study.inspect).fields.map(_.info.id),
      Vector(
        "input",
        "layout",
        "method",
        "phases",
        "weight",
        "failurePolicy",
        "frame",
        "grid",
        "estimate.0",
        "estimate.1"
      )
    )
    val children = get(study.inspect).fields.find(_.info.id == "estimate.1").get.children
    assertEquals(children.map(_.info.id), Vector("sigma", "edges"))
    assertEquals(children.head.info.quantity, Quantity.Planar(PlanarUnit.Px))
    assertEquals(
      children.head.info.kind,
      FieldKind.Numeric(
        Quantity.Planar(PlanarUnit.Px),
        NumberShape.Real,
        NumericBounds.positive
      )
    )
    assertEquals(children.head.values, Vector(Provenance.Param.Num(10)))
    val s = Json.fromString
    val n = Json.fromInt
    checkChanges(
      StudyCodecs.cosine[Px].codec,
      study,
      _.inspect,
      _.diff(_),
      Vector(
        ("input", s("0123456789abcdef"), "input"),
        ("frame.id", s("other"), "frame"),
        ("frame.xMin", n(-1), "frame"),
        ("frame.yMin", n(-1), "frame"),
        ("frame.xMax", n(900), "frame"),
        ("frame.yMax", n(700), "frame"),
        ("frame.yAxis", s("Up"), "frame"),
        ("gridId", s("other"), "grid"),
        ("nx", n(11), "grid"),
        ("ny", n(9), "grid"),
        ("focalPhase", s("test"), "phases"),
        ("referencePhase", s("learn"), "phases"),
        ("weight", s("Uniform"), "weight"),
        ("policy", Json.obj("kind" -> s("successfulOnly"), "minimum" -> n(2)), "failurePolicy"),
        ("estimates.1.sigma", n(20), "estimate.1"),
        ("estimates.1.edges", s("Renormalise"), "estimate.1"),
        (
          "estimates.0",
          Json.obj("kind" -> s("gaussian"), "sigma" -> n(5), "edges" -> s("Truncate")),
          "estimate.0"
        )
      )
    )
  }

  test("recording descriptors preserve every geometric, temporal, detector and AOI choice") {
    assertEquals(
      get(recording.inspect).fields.map(_.info.id),
      Vector(
        "input",
        "source",
        "frame",
        "clocks",
        "angularFrame",
        "viewing",
        "syncModel",
        "residualLimitMicros",
        "interpolationGapMicros",
        "detector",
        "detector.minimumDurationMicros",
        "detector.thresholdDegPerSecond",
        "sync.0",
        "area.0"
      )
    )
    val s = Json.fromString
    val n = Json.fromInt
    checkChanges(
      recordingCodec.codec,
      recording,
      _.inspect,
      _.diff(_),
      Vector(
        ("input", s("0123456789abcdef"), "input"),
        ("source", s("second"), "source"),
        ("display.id", s("other"), "frame"),
        ("display.xMin", n(-1), "frame"),
        ("display.yMin", n(-1), "frame"),
        ("display.xMax", n(900), "frame"),
        ("display.yMax", n(700), "frame"),
        ("display.yAxis", s("Up"), "frame"),
        ("trackerClock", s("other"), "clocks"),
        ("analysisClock", s("other"), "clocks"),
        ("angularFrame", s("other"), "angularFrame"),
        ("viewing.distanceMm", n(700), "viewing"),
        ("viewing.widthMm", n(500), "viewing"),
        ("viewing.heightMm", n(400), "viewing"),
        ("syncModel", s("Affine"), "syncModel"),
        ("residualLimitMicros", s("1500"), "residualLimitMicros"),
        ("interpolationGapMicros", s("75000"), "interpolationGapMicros"),
        ("parameters.value.threshold", n(35), "detector.thresholdDegPerSecond"),
        ("parameters.value.minimumMicros", s("70000"), "detector.minimumDurationMicros"),
        ("marks.0.id", s("other"), "sync.0"),
        ("marks.0.sourceMicros", s("110"), "sync.0"),
        ("marks.0.targetMicros", s("210"), "sync.0"),
        ("areas.0.id", s("other"), "area.0"),
        ("areas.0.label", s("Other"), "area.0"),
        ("areas.0.xMin", n(-1), "area.0"),
        ("areas.0.yMin", n(-1), "area.0"),
        ("areas.0.xMax", n(900), "area.0"),
        ("areas.0.yMax", n(700), "area.0")
      )
    )
  }

  test(
    "temporal descriptors cover exact relative windows, repetitions and observed-time convention"
  ) {
    assertEquals(
      get(temporal.inspect).fields.map(_.info.id).drop(get(study.inspect).fields.size),
      Vector(
        "temporal.input",
        "temporal.boundary",
        "temporal.scope",
        "window.0",
        "repetition.0"
      )
    )
    val s = Json.fromString
    checkChanges(
      temporalCodec.codec,
      temporal,
      _.inspect,
      _.diff(_),
      Vector(
        ("input", s("0123456789abcdef"), "temporal.input"),
        ("boundary", s("FullyContained"), "temporal.boundary"),
        ("windows.0.name", s("late"), "window.0"),
        ("windows.0.fromMicros", s("10"), "window.0"),
        ("windows.0.untilMicros", s("9007199254740993"), "window.0"),
        ("repetitions.0.name", s("other"), "repetition.0"),
        ("repetitions.0.focal", s("second-recall"), "repetition.0"),
        ("repetitions.0.reference", s("learn"), "repetition.0")
      )
    )
    assert(get(temporal.inspect).conventions.exists(_.contains("observed coverage")))
    // Each cell runs the base study's cursor: the temporal plan states the
    // base method's execution capability, not the whole-operation default.
    assertEquals(get(study.inspect).execution, ExecutionCapability.BoundedComparison)
    assertEquals(get(temporal.inspect).execution, get(study.inspect).execution)
  }
