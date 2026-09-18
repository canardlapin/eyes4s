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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Round-trip laws for the saved plan codecs: the built-in study plan
  * (`eyes4s.study@1` with its `cosine@1` method, layout, key and `unit@1`
  * parameter identities) and the temporal and recording plan codecs the
  * fresh-process harness saves under the conventional
  * `eyes4s.temporal-study@1` and `eyes4s.recording-plan@1` schemas. Plans are
  * compared by their full typed description and input reference, which is
  * what `plan.diff` and the manifest's result relation compare.
  *
  * {{{
  * | codec               | killed mutants                                          |
  * |---------------------|---------------------------------------------------------|
  * | eyes4s.study@1      | dropped scale, swapped phases, failure policy reset      |
  * | temporal study plan | dropped window, boundary flipped                        |
  * | recording plan      | dropped synchronization mark, detector threshold moved  |
  * }}}
  */
class PlanCodecLawSuite extends munit.DisciplineSuite:
  import PlanCodecLawSuite.*

  private type Cosine   = StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type Temporal = TemporalStudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val studies   = StudyCodecs.cosine[Px]
  private val temporals =
    new TemporalStudyCodec(definition("eyes4s.temporal-study", 1), studies)
  private val recordingSchema = definition("eyes4s.recording-plan", 1)
  private val ivt             = RecordingCodecs.ivt(
    recordingSchema,
    definition("eyes4s.recording.ivt", 1),
    definition("eyes4s.ivt-parameters", 1)
  )
  private val idt = RecordingCodecs.idt(
    recordingSchema,
    definition("eyes4s.recording.idt", 1),
    definition("eyes4s.idt-parameters", 1)
  )
  private val ek = RecordingCodecs.engbertKliegl(
    recordingSchema,
    definition("eyes4s.recording.engbert-kliegl", 1),
    definition("eyes4s.engbert-kliegl-parameters", 1)
  )

  private def sameStudy(a: Cosine, b: Cosine): Boolean =
    a.description == b.description && a.input == b.input && a.diff(b).isEmpty
  private def sameTemporal(a: Temporal, b: Temporal): Boolean =
    a.description == b.description && a.input == b.input && a.diff(b).isEmpty
  private def sameRecording[P](a: RecordingPlan[P], b: RecordingPlan[P]): Boolean =
    a.description == b.description && a.input == b.input && a.parameters == b.parameters

  checkAll("study plan", CodecLaws.roundTrip(studies.codec, cosinePlans, sameStudy))
  checkAll(
    "temporal study plan",
    CodecLaws.roundTrip(temporals.codec, temporalPlans, sameTemporal)
  )
  checkAll(
    "I-VT recording plan",
    CodecLaws.roundTrip(ivt.codec, recordingPlans(ivt.method, ivtParameters), sameRecording)
  )
  checkAll(
    "I-DT recording plan",
    CodecLaws.roundTrip(idt.codec, recordingPlans(idt.method, idtParameters), sameRecording)
  )
  checkAll(
    "Engbert-Kliegl recording plan",
    CodecLaws.roundTrip(ek.codec, recordingPlans(ek.method, ekParameters), sameRecording)
  )

  // -------------------------------------------------------------------------
  // Mutants
  // -------------------------------------------------------------------------

  /** Mutant checks run from a fixed seed, so every kill is reproducible evidence. */
  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(60).withInitialSeed(0x53364b494c4cL)

  /** The shipped codec survives: every property passes outright. */
  private def survives[A](
      codec: VersionedCodec[A],
      gen: Gen[A],
      eq: (A, A) => Boolean
  ): Boolean =
    CodecLaws
      .roundTrip(codec, gen, eq)
      .all
      .properties
      .forall((_, prop) => Test.check(killParameters, prop).passed)

  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { (_, prop) =>
      Test.check(killParameters, prop).status match
        case Test.Failed(_, _) | Test.PropException(_, _, _) => true
        case _                                               => false
    }

  /** Wrap a codec so that decoding applies a deliberate change to the value. */
  private def mutant[A](codec: VersionedCodec[A])(change: A => Option[A]): VersionedCodec[A] =
    VersionedCodec.checked[A](codec.schema)(a =>
      codec
        .encode(a)
        .flatMap(j =>
          j.hcursor.get[Json]("value").left.map(e => CodecError.Field("value", j, e.message))
        )
    )(raw =>
      codec
        .decode(
          Json.obj(
            "schema" -> Json.obj(
              "name"    -> Json.fromString(codec.schema.name),
              "version" -> Json.fromInt(codec.schema.version)
            ),
            "value" -> raw
          )
        )
        .map(a => change(a).getOrElse(a))
    )

  private def cosine(
      p: Cosine,
      focal: String,
      reference: String,
      estimates: Vector[StudyEstimate[Px]],
      policy: FailurePolicy
  ): Option[Cosine] =
    StudyPlan.cosine(p.input, p.grid, focal, reference, p.weight, estimates, policy).toOption

  test("the study-plan law kills a dropped scale, swapped phases and a reset failure policy") {
    val droppedScale = mutant(studies.codec)(p =>
      Option
        .when(p.estimates.size > 1)(p)
        .flatMap(p => cosine(p, p.focalPhase, p.referencePhase, p.estimates.init, p.policy))
    )
    assert(killed(droppedScale, cosinePlans, sameStudy))
    val swapped = mutant(studies.codec)(p =>
      cosine(p, p.referencePhase, p.focalPhase, p.estimates, p.policy)
    )
    assert(killed(swapped, cosinePlans, sameStudy))
    val reset = mutant(studies.codec)(p =>
      cosine(p, p.focalPhase, p.referencePhase, p.estimates, FailurePolicy.RequireAll)
    )
    assert(killed(reset, cosinePlans, sameStudy))
    assert(survives(studies.codec, cosinePlans, sameStudy))
  }

  test("the temporal-plan law kills a dropped window and a flipped boundary") {
    def rebuilt(p: Temporal, windows: Vector[StudyWindow], boundary: FixationBoundary) =
      TemporalStudyPlan.of(p.base, p.input, windows, p.repetitions, boundary).toOption
    val droppedWindow = mutant(temporals.codec)(p =>
      Option.when(p.windows.size > 1)(p).flatMap(p => rebuilt(p, p.windows.init, p.boundary))
    )
    assert(killed(droppedWindow, temporalPlans, sameTemporal))
    val flipped = mutant(temporals.codec)(p =>
      rebuilt(
        p,
        p.windows,
        if p.boundary == FixationBoundary.ClipDuration then FixationBoundary.FullyContained
        else FixationBoundary.ClipDuration
      )
    )
    assert(killed(flipped, temporalPlans, sameTemporal))
    assert(survives(temporals.codec, temporalPlans, sameTemporal))
  }

  test("the recording-plan law kills a dropped synchronization mark and a moved threshold") {
    def rebuilt(
        p: RecordingPlan[IvtParameters],
        marks: Vector[SyncMark],
        parameters: IvtParameters
    ) =
      RecordingPlan
        .of(
          p.input,
          p.source,
          p.display,
          p.trackerClock,
          p.analysisClock,
          p.angularFrameId,
          p.viewing,
          p.synchronizationModel,
          marks,
          p.residualLimit,
          p.interpolationGap,
          p.areas,
          p.method,
          parameters
        )
        .toOption
    val plans       = recordingPlans(ivt.method, ivtParameters)
    val droppedMark = mutant(ivt.codec)(p =>
      Option.when(p.marks.nonEmpty)(p).flatMap(p => rebuilt(p, p.marks.init, p.parameters))
    )
    assert(killed(droppedMark, plans, sameRecording))
    val moved = mutant(ivt.codec)(p =>
      Velocity
        .perSecond[Deg](p.parameters.threshold.velocity.value * 2)
        .toOption
        .flatMap(v => IvtThreshold.of(v).toOption)
        .flatMap(t => rebuilt(p, p.marks, p.parameters.copy(threshold = t)))
    )
    assert(killed(moved, plans, sameRecording))
    assert(survives(ivt.codec, plans, sameRecording))
  }

object PlanCodecLawSuite:
  /** Generated operands satisfy every constructor invariant by construction,
    * so a `Left` here is a generator bug rather than a discarded case.
    */
  def sure[E, A](value: Either[E, A]): A =
    value.fold(
      e => throw new IllegalStateException(s"generator invariant violated: $e"),
      identity
    )

  def definition(name: String, version: Int): DefinitionId = sure(
    DefinitionId.of(name, version)
  )

  /** Names that JSON must carry exactly: quotes, separators, non-ASCII text. */
  private val labelValues: Vector[String] =
    Vector("recall", "encode", "retest", "phase \"two\"", "fase-é", "阶段", "a/b\\c")
  private val labels: Gen[String] = Gen.oneOf(labelValues)

  /** Two distinct labels, chosen without discarding anything. */
  private val distinctLabels: Gen[(String, String)] = for
    first  <- labels
    second <- Gen.oneOf(labelValues.filterNot(_ == first))
  yield (first, second)

  private val references: Gen[String] = Gen.long.map(l => f"$l%016x")

  val frames: Gen[Frame[Px]] = for
    name  <- labels
    x0    <- Gen.choose(-500.0, 500.0)
    y0    <- Gen.choose(-500.0, 500.0)
    w     <- Gen.oneOf(Gen.choose(0.5, 4000.0), Gen.const(0.1), Gen.const(1920.0))
    h     <- Gen.oneOf(Gen.choose(0.5, 4000.0), Gen.const(1080.0))
    yAxis <- Gen.oneOf(YAxis.Down, YAxis.Up)
  yield Frame.of(FrameId(name), sure(Bounds.of[Px](x0, y0, x0 + w, y0 + h)), yAxis)

  private val estimates: Gen[Vector[StudyEstimate[Px]]] = for
    n     <- Gen.choose(1, 4)
    drawn <- Gen.listOfN(
      n,
      Gen.frequency(
        1 -> Gen.const(StudyEstimate.Binned[Px]()),
        3 -> (for
          sigma <- Gen.oneOf(Gen.choose(0.01, 200.0), Gen.const(0.1), Gen.const(1.0 / 3.0))
          edges <- Gen.oneOf(EdgePolicy.values.toIndexedSeq)
        yield StudyEstimate.Gaussian[Px](sure(Sigma.of[Px](sigma)), edges))
      )
    )
  yield drawn.toVector.distinctBy(_.name)

  private val policies: Gen[FailurePolicy] =
    Gen.oneOf(
      Gen.const(FailurePolicy.RequireAll),
      Gen.choose(1, 5).map(n => sure(FailurePolicy.successfulOnly(n)))
    )

  private def cosine(weights: Gen[Weight]) = for
    input  <- references.map(r => sure(ArtifactRef.parse[StudyInput[StudyKey, Px]](r)))
    frame  <- frames
    nx     <- Gen.choose(1, 64)
    ny     <- Gen.choose(1, 64)
    phases <- distinctLabels
    (focal, reference) = phases
    weight <- weights
    scales <- estimates
    policy <- policies
  yield sure(
    StudyPlan.cosine(
      input,
      sure(Grid.over(frame, nx, ny)),
      focal,
      reference,
      weight,
      scales,
      policy
    )
  )

  val cosinePlans: Gen[StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    cosine(Gen.oneOf(Weight.values.toIndexedSeq))

  /** Temporal plans over duration-weighted bases, with windows beyond
    * JavaScript's exact integer range and every fixation boundary.
    */
  val temporalPlans: Gen[TemporalStudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]] =
    for
      base  <- cosine(Gen.const(Weight.Duration))
      input <- references.map(r => sure(ArtifactRef.parse[TemporalStudyInput[StudyKey, Px]](r)))
      count <- Gen.choose(1, 4)
      origins <- Gen.listOfN(
        count,
        Gen.oneOf(Gen.choose(-1000000L, 1000000L), Gen.const(9007199254740993L))
      )
      widths <- Gen.listOfN(count, Gen.choose(1L, 5000000L))
      windows = origins.zip(widths).zipWithIndex.map { case ((from, width), i) =>
        sure(
          StudyWindow.of(
            s"window-$i",
            sure(Window.of(Span.micros(from), Span.micros(from + width)))
          )
        )
      }
      repeats <- Gen.choose(1, 3)
      phases  <- Gen.listOfN(repeats, distinctLabels)
      contrasts = phases.zipWithIndex.map { case ((f, r), i) =>
        sure(RepetitionContrast.withinParticipant(s"repetition-$i", f, r))
      }
      boundary <- Gen.oneOf(FixationBoundary.values.toIndexedSeq)
    yield sure(
      TemporalStudyPlan.of(base, input, windows.toVector, contrasts.toVector, boundary)
    )

  private val durations: Gen[MinimumEventDuration] =
    Gen
      .oneOf(Gen.choose(1L, 500000L), Gen.const(9007199254740993L))
      .map(us => sure(MinimumEventDuration.of(Span.micros(us))))

  val ivtParameters: Gen[IvtParameters] = for
    velocity <- Gen.oneOf(Gen.choose(0.5, 1000.0), Gen.const(30.0))
    minimum  <- durations
  yield IvtParameters(sure(IvtThreshold.of(sure(Velocity.perSecond[Deg](velocity)))), minimum)

  val idtParameters: Gen[IdtParameters] = for
    width   <- Gen.choose(0.1, 10.0)
    height  <- Gen.choose(0.1, 10.0)
    minimum <- durations
  yield IdtParameters(sure(Extent.of[Deg](width, height)), minimum)

  val ekParameters: Gen[EkParameters] = for
    x <- Gen.choose(0.5, 200.0)
    y <- Gen.choose(0.5, 200.0)
    n <- Gen.choose(1, 12)
  yield EkParameters(sure(EkThresholds.of(x, y)), sure(EkMinimumSamples.of(n)))

  /** Recording plans with or without viewing geometry, zero to four marks at
    * extreme instants, an optional residual limit and one to three areas.
    */
  def recordingPlans[P](method: RecordingMethod[P], parameters: Gen[P]): Gen[RecordingPlan[P]] =
    for
      input   <- references.map(r => sure(ArtifactRef.parse[Recording[Px]](r)))
      source  <- labels
      display <- frames
      tracker <- labels.map(l => ClockId(s"tracker $l"))
      angular <- labels.map(l => FrameId(s"angular $l"))
      viewing <- Gen.option(
        for
          d <- Gen.choose(300.0, 1000.0)
          w <- Gen.choose(200.0, 800.0)
          h <- Gen.choose(200.0, 600.0)
        yield sure(Viewing.millimetres(d, w, h))
      )
      model <- Gen.oneOf(SyncFitMode.values.toIndexedSeq)
      marks <- Gen
        .choose(0, 4)
        .flatMap(n =>
          Gen.listOfN(
            n,
            Gen.zip(
              Gen.oneOf(Gen.choose(-1000000000L, 1000000000L), Gen.const(9007199254740993L)),
              Gen.choose(-5000000L, 5000000L)
            )
          )
        )
      limit <- Gen.option(
        Gen.choose(0L, 100000L).map(us => sure(SyncResidualLimit.of(Span.micros(us))))
      )
      gap   <- Gen.choose(0L, 200000L).map(us => sure(InterpolationGap.of(Span.micros(us))))
      areas <- Gen
        .choose(1, 3)
        .flatMap(n =>
          Gen.listOfN(
            n,
            for
              x0 <- Gen.choose(0.0, 500.0)
              y0 <- Gen.choose(0.0, 500.0)
              w  <- Gen.choose(1.0, 500.0)
              h  <- Gen.choose(1.0, 500.0)
              l  <- labels
            yield (l, sure(Bounds.of[Px](x0, y0, x0 + w, y0 + h)))
          )
        )
      params <- parameters
    yield sure(
      RecordingPlan.of(
        input,
        RecordingRef(source),
        display,
        tracker,
        ClockId("analysis"),
        angular,
        viewing,
        model,
        marks.toVector.zipWithIndex.map { case ((at, offset), i) =>
          sure(SyncMark.of(s"mark-$i", Instant.micros(at), Instant.micros(at + offset)))
        },
        limit,
        gap,
        areas.toVector.zipWithIndex.map { case ((label, bounds), i) =>
          sure(RecordingArea.of(s"area-$i", label, bounds))
        },
        method,
        params
      )
    )
