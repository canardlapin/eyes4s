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

import eyes4s.aoi.*
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.{Gen, Test}

/** Published round-trip laws for recording result archives, over generated
  * recordings with fixations, saccades, blinks, signal loss and off-screen
  * samples, synchronized by generated marks (some rejected as outliers),
  * warped to visual angle, optionally interpolated, detected by I-VT or I-DT
  * and assigned to generated areas; with deliberate mutants that a lawful
  * archive must not survive.
  */
class RecordingResultCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  /** Generated operands satisfy every constructor invariant by construction,
    * so a `Left` here is a generator bug and surfaces as a failure rather
    * than as a discarded case; the generators therefore cannot exhaust.
    */
  private def sure[E, A](value: Either[E, A]): A =
    value.fold(
      e => throw new IllegalStateException(s"generator invariant violated: $e"),
      identity
    )

  private def id(name: String): DefinitionId = get(DefinitionId.of(name, 1))

  private val ivt = RecordingCodecs.ivt(
    id("eyes4s.recording-plan"),
    id("eyes4s.recording.ivt"),
    id("eyes4s.ivt-parameters")
  )
  private val idt = RecordingCodecs.idt(
    id("eyes4s.recording-plan"),
    id("eyes4s.recording.idt"),
    id("eyes4s.idt-parameters")
  )

  private val display  = get(Frame.screen("law-display", 1000, 1000))
  private val tracker  = ClockId("law-tracker")
  private val analysis = ClockId("law-analysis")
  private val viewing  = get(Viewing.millimetres(600, 500, 500))
  private val period   = 2000L

  /** Fixation segments around generated centres, joined by three-sample
    * saccades, with occasional blinks, signal loss and off-screen samples.
    * A lossless path starts at (250, 250), inside the first area of every
    * generated area set.
    */
  private def gazes(segments: Int, lossy: Boolean): Gen[Vector[Gaze[Px]]] = for
    drawn <- Gen.listOfN(segments, Gen.zip(Gen.choose(150.0, 850.0), Gen.choose(150.0, 850.0)))
    centres = if lossy then drawn else (250.0, 250.0) :: drawn.drop(1)
    lengths <- Gen.listOfN(segments, Gen.choose(8, 16))
    jitter  <- Gen.listOfN(segments * 16, Gen.zip(Gen.choose(-0.3, 0.3), Gen.choose(-0.3, 0.3)))
    loss    <- Gen.listOfN(segments * 20, Gen.frequency(24 -> 0, 1 -> 1, 1 -> 2, 1 -> 3))
  yield
    val fixations = centres.zip(lengths).zipWithIndex.map { case (((x, y), n), s) =>
      Vector.tabulate(n) { i =>
        val (dx, dy) = jitter((s * 16 + i) % jitter.size)
        Gaze.Tracked(Pt[Px](x + dx, y + dy), None): Gaze[Px]
      }
    }
    val joined = fixations.zipWithIndex.flatMap { (segment, s) =>
      if s + 1 >= centres.size then segment
      else
        val (x0, y0) = centres(s)
        val (x1, y1) = centres(s + 1)
        segment ++ Vector(1, 2, 3).map { k =>
          Gaze
            .Tracked(Pt[Px](x0 + (x1 - x0) * k / 4.0, y0 + (y1 - y0) * k / 4.0), None): Gaze[Px]
        }
    }.toVector
    if !lossy then joined
    else
      joined.zipWithIndex.map { (gaze, i) =>
        loss(i % loss.size) match
          case 1 => Gaze.Blink[Px]()
          case 2 => Gaze.Lost[Px]()
          case 3 => Gaze.OffScreen[Px](Pt[Px](1200.0, 500.0))
          case _ => gaze
      }

  private def recordings(segments: Int, lossy: Boolean): Gen[Recording[Px]] = for
    origin <- Gen.oneOf(0L, 123457L)
    path   <- gazes(segments, lossy)
  yield sure(
    Recording.of(
      display,
      tracker,
      Rate.Fixed(sure(Hz(500.0))),
      Eye.Right,
      None,
      IArray.from(
        path.zipWithIndex.map((g, i) => Sample(Instant.micros(origin + i * period), g))
      )
    )
  )

  /** Offset-only marks with small residuals and, beside at least two of
    * them, possibly an outlier 4.5 ms off that the 2 ms residual limit rejects
    * while the others stay within it.
    */
  private val marks: Gen[(Vector[SyncMark], Option[SyncResidualLimit])] = for
    n       <- Gen.choose(1, 3)
    offset  <- Gen.choose(0L, 5000000L)
    noise   <- Gen.listOfN(n, Gen.choose(0L, 20L))
    outlier <- if n >= 2 then Gen.oneOf(false, true) else Gen.const(false)
  yield
    val ordinary = noise.toVector.zipWithIndex.map { (e, i) =>
      sure(
        SyncMark.of(
          s"m$i",
          Instant.micros(i * 1000000L),
          Instant.micros(i * 1000000L + offset + e)
        )
      )
    }
    if !outlier then ordinary -> None
    else
      val at = n * 1000000L
      (
        ordinary :+ sure(
          SyncMark.of("outlier", Instant.micros(at), Instant.micros(at + offset + 4500L))
        ),
        Some(sure(SyncResidualLimit.of(Span.micros(2000))))
      )

  private val areas: Gen[Vector[RecordingArea]] = Gen.oneOf(
    Vector(sure(RecordingArea.of("left", "Left", sure(Bounds.of[Px](0, 0, 500, 1000))))),
    Vector(
      sure(RecordingArea.of("top", "Top", sure(Bounds.of[Px](0, 0, 1000, 500)))),
      sure(RecordingArea.of("right", "Right", sure(Bounds.of[Px](500, 0, 1000, 1000))))
    )
  )

  private def plan[P](
      recording: Recording[Px],
      method: RecordingMethod[P],
      parameters: P
  ): Gen[RecordingPlan[P]] = for
    (sync, limit) <- marks
    // Gap interpolation only where no off-screen sample borders a gap: the
    // filter interpolates toward an off-screen position, which preparation
    // then refuses as a tracked sample outside the angular frame.
    gap <-
      if recording.samples.exists(_.gaze.isInstanceOf[Gaze.OffScreen[?]]) then Gen.const(0L)
      else Gen.oneOf(0L, 6000L)
    where <- areas
  yield sure(
    RecordingPlan.of(
      ArtifactRef.of[Recording[Px]](recording.contentHash),
      RecordingRef("law-source"),
      display,
      tracker,
      analysis,
      FrameId("law-angular"),
      Some(viewing),
      SyncFitMode.OffsetOnly,
      sync,
      limit,
      sure(InterpolationGap.of(Span.micros(gap))),
      where,
      method,
      parameters
    )
  )

  private def ivtAnalyses(
      segments: Gen[Int],
      lossy: Gen[Boolean],
      thresholds: Gen[Double] = Gen.oneOf(30.0, 100.0),
      minima: Gen[Long] = Gen.oneOf(2000L, 6000L)
  ): Gen[RecordingAnalysis[IvtParameters]] =
    for
      n         <- segments
      loss      <- lossy
      recording <- recordings(n, loss)
      threshold <- thresholds
      minimum   <- minima
      built     <- plan(
        recording,
        ivt.method,
        IvtParameters(
          sure(IvtThreshold.of(sure(Velocity.perSecond[Deg](threshold)))),
          sure(MinimumEventDuration.of(Span.micros(minimum)))
        )
      )
    yield sure(built.run(recording))

  private val idtAnalyses: Gen[RecordingAnalysis[IdtParameters]] = for
    n         <- Gen.choose(1, 4)
    loss      <- Gen.oneOf(false, true)
    recording <- recordings(n, loss)
    extent    <- Gen.oneOf(0.5, 1.0)
    built     <- plan(
      recording,
      idt.method,
      IdtParameters(
        sure(Extent.of[Deg](extent, extent)),
        sure(MinimumEventDuration.of(Span.micros(6000)))
      )
    )
  yield sure(built.run(recording))

  private val ivtResults = ivt.results
  private val idtResults = idt.results

  checkAll(
    "I-VT recording result",
    CodecLaws.roundTrip(
      ivtResults.codec,
      ivtAnalyses(Gen.choose(1, 4), Gen.oneOf(false, true)),
      RecordingResultEquivalence.same
    )
  )
  checkAll(
    "I-DT recording result",
    CodecLaws.roundTrip(idtResults.codec, idtAnalyses, RecordingResultEquivalence.same)
  )

  /** Three clean segments: every analysis has events, tracked samples and
    * samples inside the first area.
    */
  private val mutable =
    ivtAnalyses(Gen.const(3), Gen.const(false), Gen.const(100.0), Gen.const(2000L))

  test("the mutable generator always yields events, tracked samples and assigned samples") {
    (1 to 25).foreach { _ =>
      val analysis = get(mutable.sample.toRight("no sample"))
      assert(analysis.detection.eventSeries.events.nonEmpty)
      assert(analysis.angular.samples(0).gaze.isInstanceOf[Gaze.Tracked[?]])
      assert(analysis.assignment.toVector.exists {
        case SampleMembership.Areas(ids) => ids.contains(analysis.assignment.aoiSet.ids.head)
        case _                           => false
      })
    }
  }

  /** Wrap a codec so that decoding applies a deliberate change to the value. */
  private def mutant[A](codec: VersionedCodec[A])(
      change: A => Either[CodecError, A]
  ): VersionedCodec[A] =
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
        .flatMap(change)
    )

  /** A mutant is killed only by a falsified property, never by exhaustion or an exception. */
  private def killed[A](codec: VersionedCodec[A], gen: Gen[A], eq: (A, A) => Boolean): Boolean =
    CodecLaws.roundTrip(codec, gen, eq).all.properties.exists { case (_, prop) =>
      Test.check(Test.Parameters.default.withMinSuccessfulTests(40), prop).status match
        case Test.Failed(_, _) => true
        case _                 => false
    }

  private type Analysis = RecordingAnalysis[IvtParameters]

  /** Rebuild the analysis through the checked reconstruction with changed parts. */
  private def rebuilt(a: Analysis)(
      angular: Recording[Deg] = a.angular,
      events: Vector[Event[Deg]] = a.detection.eventSeries.events,
      support: Vector[SampleRange] = a.detection.eventSeries.support,
      areas: Vector[Aoi[Deg]] = a.assignment.aoiSet.areas
  ): Either[CodecError, Analysis] =
    RecordingAnalysis
      .reconstruct(a.plan, angular, a.prepared, events, support, areas)
      .left
      .map(CodecError.RecordingResult.apply)

  test("published laws kill a dropped event, a moved angular sample and an emptied area") {
    val droppedEvent = mutant(ivtResults.codec)(a =>
      rebuilt(a)(
        events = a.detection.eventSeries.events.dropRight(1),
        support = a.detection.eventSeries.support.dropRight(1)
      )
    )
    assert(killed(droppedEvent, mutable, RecordingResultEquivalence.same))

    val movedSample = mutant(ivtResults.codec) { a =>
      val first = a.angular.samples(0)
      val moved = first.gaze match
        case Gaze.Tracked(p, pupil) => Gaze.Tracked(Pt[Deg](Math.nextUp(p.x), p.y), pupil)
        case other                  => other
      Recording
        .of(
          a.angular.frame,
          a.angular.clock,
          a.angular.rate,
          a.angular.eye,
          a.angular.pupilUnit,
          a.angular.samples.updated(0, first.copy(gaze = moved)),
          a.angular.samplingTolerance
        )
        .left
        .map(CodecError.Recording("angular", _))
        .flatMap(angular => rebuilt(a)(angular = angular))
    }
    assert(killed(movedSample, mutable, RecordingResultEquivalence.same))

    val emptiedArea = mutant(ivtResults.codec) { a =>
      val first = a.assignment.aoiSet.areas.head
      Aoi
        .of(first.id.value, first.label, first.frame, Region.empty[Deg], first.attributes)
        .left
        .map(e => CodecError.Field("area", Json.Null, e.message))
        .flatMap(empty => rebuilt(a)(areas = empty +: a.assignment.aoiSet.areas.tail))
    }
    assert(killed(emptiedArea, mutable, RecordingResultEquivalence.same))
  }
