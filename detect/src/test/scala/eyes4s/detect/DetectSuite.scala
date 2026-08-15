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

package eyes4s.detect

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import scala.compiletime.testing.typeCheckErrors

class DetectSuite extends munit.FunSuite:

  val clock = ClockId("tracker")

  private def halfWidth(value: Int)   = WindowHalfWidth.of(value).toOption.get
  private def padding(value: Span)    = MissingPadding.of(value).toOption.get
  private def gap(value: Span)        = InterpolationGap.of(value).toOption.get
  private def minimum(value: Span)    = MinimumEventDuration.of(value).toOption.get
  private def ekLambda(value: Double) = EkMultiplier.of(value).toOption.get

  private val min20ms     = minimum(Span.millis(20))
  private val ekMin3      = EkMinimumSamples.of(3).toOption.get
  private val filterFrame =
    Frame.angular("filter-frame", 1200.0, 100.0).toOption.get
  private val trackedOnly = WindowObservationPolicy.RequireTracked

  /** 1000 Hz: one sample per millisecond. */
  private def at(ms: Long, x: Double, y: Double) =
    Sample(Instant.millis(ms), Gaze.Tracked(Pt[Deg](x, y), None))

  private def missing(ms: Long) = Sample(Instant.millis(ms), Gaze.Lost[Deg]())

  /** Holds still at `x` for `n` ms starting at `from`. */
  private def hold(from: Long, n: Int, x: Double) =
    (0 until n).map(i => at(from + i, x, 0.0)).toVector

  /** Moves from x0 to x1 over n ms. */
  private def sweep(from: Long, n: Int, x0: Double, x1: Double) =
    (0 until n).map(i => at(from + i, x0 + (x1 - x0) * i / n, 0.0)).toVector

  private def events[U <: Unit2D](emissions: Vector[DetectionEmission[U]]): Vector[Event[U]] =
    emissions.map {
      case Right(event) => event
      case Left(error)  => fail(error.message)
    }

  private def regular(samples: Vector[Sample[Deg]]): RegularSampling =
    RegularSampling.from(samples).toOption.get

  // -------------------------------------------------------------------------
  // The unit claim, made good on the DATA
  // -------------------------------------------------------------------------

  test("a pixel sample cannot reach the velocity detector") {
    val errs = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.core.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val threshold = IvtThreshold.of(Velocity.degPerSecond(30).toOption.get).toOption.get
      val duration = MinimumEventDuration.of(Span.millis(60)).toOption.get
      val d = Detectors.ivt(threshold, duration, ClockId("c"))
      val pixels: List[Sample[Px]] = Nil
      d.runAll(pixels)
    """)
    assert(errs.nonEmpty, "a pixel recording reached a degrees-per-second detector")
  }

  // -------------------------------------------------------------------------
  // I-VT
  // -------------------------------------------------------------------------

  val threshold = IvtThreshold.of(Velocity.degPerSecond(30.0).toOption.get).toOption.get
  def ivt       = Detectors.ivt(threshold, min20ms, clock)

  test("a still eye yields one fixation") {
    val fixations = events(ivt.runAll(hold(0, 100, 5.0))).collect {
      case f: Event.Fixation[Deg] => f
    }
    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.centre.x, 5.0, 1e-9)
    assertEquals(fixations.head.sampleCount, 100)
    assert(fixations.head.duration.toMillis >= 20.0, clue(fixations.head.duration.toMillis))
  }

  test("a fixation, a sweep and a fixation yield fixation-saccade-fixation") {
    // 1 deg/ms is 1000 deg/s, far above threshold.
    val input    = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    val detected = events(ivt.runAll(input))
    val kinds    = detected.map {
      case _: Event.Fixation[Deg] => "F"
      case _: Event.Saccade[Deg]  => "S"
      case _                      => "?"
    }
    assertEquals(kinds, Vector("F", "S", "F"))
  }

  test("the fixation centroids match where the eye actually was") {
    val input     = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    val fixations = events(ivt.runAll(input)).collect { case f: Event.Fixation[Deg] => f }
    assertEqualsDouble(fixations.head.centre.x, 0.0, 1e-6)
    assertEqualsDouble(fixations.last.centre.x, 30.0, 1e-6)
  }

  test("runs shorter than the minimum duration are discarded as noise") {
    // A 5ms twitch inside a long fixation is below the 20ms floor.
    val input    = hold(0, 60, 0.0) ++ sweep(60, 5, 0.0, 5.0) ++ hold(65, 60, 5.0)
    val detected = events(ivt.runAll(input))
    assert(detected.collect { case s: Event.Saccade[Deg] => s }.isEmpty, clue(detected))
  }

  test("an event still open at the end is flushed, not lost") {
    assertEquals(events(ivt.runAll(hold(0, 50, 1.0))).length, 1)
  }

  test("no event is emitted from samples that were never tracked") {
    assertEquals(
      events(ivt.runAll((0 until 100).map(i => missing(i.toLong)).toVector)),
      Vector.empty[Event[Deg]]
    )
  }

  test("off-surface outer samples cannot drive an I-VT classification") {
    val input = Vector(
      Sample(Instant.millis(0), Gaze.OffScreen(Pt[Deg](0.0, 0.0))),
      at(1, 0.0, 0.0),
      Sample(Instant.millis(2), Gaze.OffScreen(Pt[Deg](10.0, 0.0))),
      at(3, 10.0, 0.0),
      at(4, 10.0, 0.0)
    )
    val shortMinimum = minimum(Span.millis(1))
    val detected     = events(Detectors.ivt(threshold, shortMinimum, clock).runAll(input))

    assertEquals(detected, Vector.empty)
  }

  test("a missing middle sample terminates I-VT support") {
    val input     = hold(0, 40, 0.0) ++ Vector(missing(40)) ++ hold(41, 40, 0.0)
    val fixations = events(ivt.runAll(input)).collect { case fixation: Event.Fixation[Deg] =>
      fixation
    }

    assertEquals(fixations.length, 2)
    assertEqualsDouble(fixations.head.span.offset.toMillis, 40.0, 1e-12)
    assertEqualsDouble(fixations.last.span.onset.toMillis, 41.0, 1e-12)
    assertEquals(fixations.map(_.sampleCount), Vector(40, 40))
  }

  test("an incoherent detector summary is emitted as a named failure, not dropped") {
    val malformed = (0 until 30).map(i => at(i.toLong, Double.NaN, 0.0)).toVector
    val emissions = ivt.runAll(malformed)

    assertEquals(emissions.length, 1)
    assert(
      emissions.head match
        case Left(
              DetectionFailure.EventSummary(
                CoreError.OfEvent(EventError.NonFinitePoint("fixation", "centre", _, x, _))
              )
            ) =>
          x.isNaN
        case _ => false,
      clue(emissions.head)
    )
  }

  // -------------------------------------------------------------------------
  // I-DT
  // -------------------------------------------------------------------------

  def idt = Detectors.idt[Deg](Extent.square[Deg](1.0).toOption.get, min20ms, clock)

  test("dispersion identification finds a still period") {
    val fixations = events(idt.runAll(hold(0, 100, 5.0))).collect {
      case f: Event.Fixation[Deg] => f
    }
    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.centre.x, 5.0, 1e-9)
  }

  test("a move beyond the dispersion box ends the fixation") {
    val input     = hold(0, 60, 0.0) ++ hold(60, 60, 10.0)
    val fixations = events(idt.runAll(input)).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 2)
    assertEqualsDouble(fixations.head.centre.x, 0.0, 1e-9)
    assertEqualsDouble(fixations(1).centre.x, 10.0, 1e-9)
  }

  test("slow drift within the box stays one fixation") {
    // Drifts 0.9 deg over 100ms: below the 1.0 deg box, so not a new fixation.
    val fixations =
      events(idt.runAll(sweep(0, 100, 0.0, 0.9))).collect { case f: Event.Fixation[Deg] =>
        f
      }
    assertEquals(fixations.length, 1)
    assertEquals(
      fixations.head.dispersion.map(_.method),
      Some(DispersionMethod.BoundingBoxDiagonal)
    )
  }

  test("a failed minimum window advances until the stable cluster begins") {
    val clusteredNoise = (0 until 11).map { index =>
      at(index.toLong, if index % 2 == 0 then 2.0 else 0.0, 0.0)
    }.toVector
    val stable    = hold(11, 40, 0.25)
    val fixations = events(idt.runAll(clusteredNoise ++ stable)).collect {
      case fixation: Event.Fixation[Deg] => fixation
    }

    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 11.0, 1e-12)
  }

  test("a qualifying minimum-duration window at end of input is emitted") {
    val fixations = events(idt.runAll(hold(0, 21, 4.0))).collect {
      case fixation: Event.Fixation[Deg] => fixation
    }

    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 0.0, 1e-12)
    assertEqualsDouble(fixations.head.span.offset.toMillis, 21.0, 1e-12)
  }

  test("dispersion takes an Extent, not a bandwidth") {
    val errs = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Deg
      val duration = MinimumEventDuration.of(Span.millis(20)).toOption.get
      Detectors.idt[Deg](Sigma.deg(1.0).toOption.get, duration, ClockId("c"))
    """)
    assert(errs.nonEmpty, "a standard deviation was accepted as a bounding box")
  }

  test("missing data breaks a fixation rather than being interpolated over") {
    val input     = hold(0, 40, 0.0) ++ Vector(missing(40)) ++ hold(41, 40, 0.0)
    val fixations = events(idt.runAll(input)).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 2, "a gap is a gap unless a filter decided otherwise")
  }

  test("blink, loss, and off-surface observations all break I-DT candidates") {
    val invalid = Vector[Sample[Deg]](
      Sample(Instant.millis(40), Gaze.Blink()),
      missing(40),
      Sample(Instant.millis(40), Gaze.OffScreen(Pt[Deg](10.0, 0.0)))
    )

    invalid.foreach { gapSample =>
      val input     = hold(0, 40, 0.0) ++ Vector(gapSample) ++ hold(41, 40, 0.0)
      val fixations = events(idt.runAll(input)).collect { case f: Event.Fixation[Deg] => f }
      assertEquals(fixations.length, 2, clue(gapSample))
    }
  }

  test("a long stationary I-DT run is processed as one fixation") {
    val fixations = events(idt.runAll(hold(0, 50000, 3.0))).collect {
      case fixation: Event.Fixation[Deg] => fixation
    }
    assertEquals(fixations.length, 1)
    assertEquals(fixations.head.sampleCount, 50000)
  }

  // -------------------------------------------------------------------------
  // Filters
  // -------------------------------------------------------------------------

  test("the median filter rejects an isolated spike") {
    val spiked = hold(0, 20, 5.0).updated(10, at(10, 500.0, 0.0))
    val worst  =
      Filter
        .median(filterFrame, halfWidth(2), trackedOnly)
        .runAll(spiked)
        .flatMap(_.gaze.position)
        .map(_.x)
        .max
    assert(worst < 6.0, clue(worst))
  }

  test("a polynomial smoother would not have rejected it, which is why median is here") {
    val spiked = hold(0, 20, 5.0).updated(10, at(10, 500.0, 0.0))
    val sg     = Filter
      .savitzkyGolay(filterFrame, halfWidth(2), trackedOnly, regular(spiked))
      .runAll(spiked)
    assert(sg.flatMap(_.gaze.position).map(_.x).max > 6.0)
  }

  test("Savitzky-Golay coefficients sum to one, so a constant signal is unchanged") {
    val input = hold(0, 20, 7.0)
    Filter
      .savitzkyGolay(filterFrame, halfWidth(3), trackedOnly, regular(input))
      .runAll(input)
      .flatMap(_.gaze.position)
      .foreach(p => assertEqualsDouble(p.x, 7.0, 1e-9))
  }

  test("filters preserve the sample count") {
    val input = hold(0, 30, 1.0)
    assertEquals(
      Filter.median(filterFrame, halfWidth(2), trackedOnly).runAll(input).length,
      input.length
    )
    assertEquals(
      Filter
        .savitzkyGolay(filterFrame, halfWidth(2), trackedOnly, regular(input))
        .runAll(input)
        .length,
      input.length
    )
  }

  test("centred filters preserve timestamp order and leave both edges unchanged") {
    val input  = hold(0, 10, 1.0).updated(0, at(0, 9.0, 0.0)).updated(9, at(9, 8.0, 0.0))
    val median = Filter.median(filterFrame, halfWidth(2), trackedOnly).runAll(input)
    val sg     = Filter
      .savitzkyGolay(filterFrame, halfWidth(2), trackedOnly, regular(input))
      .runAll(input)

    Vector(median, sg).foreach { output =>
      assertEquals(output.map(_.t), input.map(_.t))
      assertEquals(output.take(2), input.take(2))
      assertEquals(output.takeRight(2), input.takeRight(2))
    }
  }

  test("a sequence shorter than the centred window passes through exactly") {
    val input = Vector(at(0, 1.0, 0.0), missing(1), at(2, 3.0, 0.0))
    assertEquals(
      Filter.median(filterFrame, halfWidth(2), trackedOnly).runAll(input),
      input
    )
  }

  test("window validity policy is explicit and missing observations preserve the centre") {
    val offScreen = Sample(Instant.millis(2), Gaze.OffScreen(Pt[Deg](1000.0, 0.0)))
    val input     = Vector(at(0, 0.0, 0.0), at(1, 1.0, 0.0), offScreen)
    val strict    = Filter
      .median(filterFrame, halfWidth(1), WindowObservationPolicy.RequireTracked)
      .runAll(input)
    val inclusive = Filter
      .median(filterFrame, halfWidth(1), WindowObservationPolicy.IncludeOffScreen)
      .runAll(input)

    assertEquals(strict(1).origin, SampleOrigin.Measured)
    assertEquals(inclusive(1).origin, SampleOrigin.Smoothed)

    val withMissing = input.updated(2, missing(2))
    val preserved   = Filter
      .median(filterFrame, halfWidth(1), WindowObservationPolicy.IncludeOffScreen)
      .runAll(withMissing)
    assertEquals(preserved(1), withMissing(1))
  }

  test("smoothing preserves centre pupil in-frame and marks an overshoot off-screen") {
    val narrow   = Frame.angular("narrow-filter", 10.0, 10.0).toOption.get
    val constant = (0 until 5).map { index =>
      Sample(
        Instant.millis(index.toLong),
        Gaze.Tracked(Pt[Deg](1.0, 0.0), if index == 2 then Some(42.0) else None)
      )
    }.toVector
    val inFrame = Filter
      .savitzkyGolay(narrow, halfWidth(2), trackedOnly, regular(constant))
      .runAll(constant)
    assertEquals(inFrame(2).gaze.pupil, Some(42.0))
    assertEquals(inFrame(2).origin, SampleOrigin.Smoothed)

    val xs    = Vector(-4.9, 4.9, 4.9, 4.9, -4.9)
    val input = xs.zipWithIndex.map { case (x, index) =>
      Sample(
        Instant.millis(index.toLong),
        Gaze.Tracked(Pt[Deg](x, 0.0), if index == 2 then Some(42.0) else None)
      )
    }
    val output = Filter
      .savitzkyGolay(narrow, halfWidth(2), trackedOnly, regular(input))
      .runAll(input)

    assertEquals(output(2).origin, SampleOrigin.Smoothed)
    assert(output(2).gaze.isInstanceOf[Gaze.OffScreen[Deg]], clue(output(2)))
    assertEquals(output(2).gaze.pupil, None)
  }

  test("Savitzky-Golay regular sampling proof rejects the exact irregular interval") {
    val irregular = Vector(at(0, 0.0, 0.0), at(1, 1.0, 0.0), at(3, 2.0, 0.0))
    assertEquals(
      RegularSampling.from(irregular),
      Left(
        ConfigurationError.IrregularSamplingInterval(
          2,
          Span.millis(1),
          Span.millis(2)
        )
      )
    )
    assertEquals(
      RegularSampling.from(Vector.empty[Sample[Deg]]),
      Left(ConfigurationError.InsufficientRegularSamples(0))
    )
    assertEquals(
      RegularSampling.from(Vector(at(0, 0.0, 0.0), at(0, 1.0, 0.0))),
      Left(ConfigurationError.NonPositiveSamplingInterval(1, Span.zero))
    )
  }

  test("Savitzky-Golay defensively preserves windows that violate its sampling proof") {
    val declared  = hold(0, 5, 1.0)
    val irregular = Vector(
      at(0, 0.0, 0.0),
      at(1, 1.0, 0.0),
      at(3, 50.0, 0.0),
      at(4, 1.0, 0.0),
      at(5, 0.0, 0.0)
    )
    val output = Filter
      .savitzkyGolay(filterFrame, halfWidth(2), trackedOnly, regular(declared))
      .runAll(irregular)
    assertEquals(output, irregular)
  }

  test("short gaps interpolate; long ones stay missing") {
    val gap  = Vector(at(0, 0.0, 0.0), missing(1), missing(2), at(3, 3.0, 0.0))
    val outS = Filter.interpolateGaps[Deg](this.gap(Span.millis(10))).runAll(gap)
    assert(outS.forall(_.gaze.isUsable), clue(outS))
    assertEqualsDouble(outS(1).gaze.position.get.x, 1.0, 1e-9)
    assertEquals(outS(1).origin, SampleOrigin.Interpolated)
    assertEquals(outS(1).lineage.toVector, Vector(SampleOrigin.Interpolated))

    val smoothed = Filter
      .median(filterFrame, halfWidth(1), trackedOnly)
      .runAll(outS)
    assertEquals(
      smoothed(1).lineage.toVector,
      Vector(SampleOrigin.Interpolated, SampleOrigin.Smoothed)
    )

    val outL = Filter.interpolateGaps[Deg](this.gap(Span.micros(500))).runAll(gap)
    assertEquals(outL.count(_.gaze.isMissing), 2)
  }

  test("blink padding condemns the samples either side") {
    val projected = at(1, 0.0, 0.0).copy(
      lineage = SampleLineage.fromLatest(SampleOrigin.Projected)
    )
    val input = Vector(at(0, 0.0, 0.0), projected, missing(2), at(3, 0.0, 0.0), at(4, 0.0, 0.0))
    val out   = Filter.padMissing[Deg](padding(Span.millis(1))).runAll(input)
    assertEquals(out.length, input.length)
    assertEquals(
      out(1).lineage.toVector,
      Vector(SampleOrigin.Measured, SampleOrigin.Projected)
    )
    // Samples at 1 and 3 are within 1ms of the blink at 2.
    assertEquals(out.count(_.gaze.isMissing), 3, clue(out.map(_.gaze.isMissing)))
  }

  // -------------------------------------------------------------------------
  // Engbert-Kliegl
  // -------------------------------------------------------------------------

  /** Deterministic pseudo-noise.
    *
    * Not a plain alternation: a two-sample square wave sits exactly in the null
    * space of the five-point velocity filter -- (a - a + a - a) = 0 -- so it
    * produces zero velocity at every sample and no threshold at all. That is a
    * real property of the estimator, and a fixture that ran into it would look
    * like a detector bug.
    */
  private def noise(i: Int): Double =
    val h = (i * 1103515245 + 12345) & 0x7fffffff
    ((h % 2000) - 1000) / 1000.0 * 0.03

  test("thresholds are estimated from a whole trial, and say so by being a value") {
    // Two passes, modelled honestly: you cannot know a median until you have
    // seen the series, so the threshold cannot be a streaming parameter.
    // Both axes carry noise. A fixture with a constant y has no vertical
    // velocity at all, so etaY is zero and no threshold exists -- which
    // is correct behaviour and a bad fixture.
    val noisy = (0 until 200).map(i => at(i.toLong, noise(i), noise(i + 7000))).toVector
    assert(EkThresholds.estimate(noisy, ekLambda(6.0)).isRight)
  }

  test("a square wave has no velocity under the five-point filter, and no threshold") {
    // Documenting the null space rather than leaving it as a trap.
    val square =
      (0 until 200).map(i => at(i.toLong, if i % 2 == 0 then 0.01 else -0.01, 0.0)).toVector
    assertEquals(
      EkThresholds.estimate(square, ekLambda(6.0)),
      Left(EkEstimationError.DegenerateVelocitySpread(0.0, 0.0, 6.0))
    )
  }

  test("a perfectly still eye admits no threshold, rather than a zero one") {
    assertEquals(
      EkThresholds.estimate(hold(0, 100, 0.0), ekLambda(6.0)),
      Left(EkEstimationError.DegenerateVelocitySpread(0.0, 0.0, 6.0))
    )
  }

  test("too few samples yield no threshold") {
    assertEquals(
      EkThresholds.estimate(hold(0, 3, 0.0), ekLambda(6.0)),
      Left(EkEstimationError.InsufficientVelocities(0, 5))
    )
  }

  test("threshold estimation names irregular sampling instead of approximating it") {
    val irregular = Vector(
      at(0, 0.0, 0.0),
      at(1, 0.1, 0.1),
      at(3, 0.2, 0.2),
      at(4, 0.3, 0.3),
      at(5, 0.4, 0.4)
    )
    assert(
      EkThresholds.estimate(irregular, ekLambda(6.0)) match
        case Left(
              EkEstimationError.InvalidSampling(
                KinematicsError.InvalidSampling(
                  ConfigurationError.IrregularSamplingInterval(2, expected, observed)
                )
              )
            ) =>
          expected == Span.millis(1) && observed == Span.millis(2)
        case _ => false
    )
  }

  test("Engbert-Kliegl detection preserves irregular sampling as a named failure") {
    val irregular = Vector(
      at(0, 0.0, 0.0),
      at(1, 0.1, 0.1),
      at(3, 0.2, 0.2),
      at(4, 0.3, 0.3),
      at(5, 0.4, 0.4)
    )
    val emissions = Detectors
      .engbertKliegl(EkThresholds.of(1.0, 1.0).toOption.get, ekMin3, clock)
      .runAll(irregular)

    assert(
      emissions.exists {
        case Left(
              DetectionFailure.Kinematics(
                KinematicsError.InvalidSampling(
                  ConfigurationError.IrregularSamplingInterval(2, expected, observed)
                )
              )
            ) =>
          expected == Span.millis(1) && observed == Span.millis(2)
        case _ => false
      },
      clue(emissions)
    )
  }

  test("native, off-surface, and interpolated gaps cannot contribute velocity") {
    val base        = (0 until 9).map(i => at(i.toLong, i.toDouble, 0.0)).toVector
    val interrupted = Vector(
      base.updated(4, missing(4)),
      base.updated(4, Sample(Instant.millis(4), Gaze.OffScreen(Pt[Deg](4.0, 0.0)))),
      base.updated(4, base(4).copy(lineage = SampleLineage.interpolated)),
      base.updated(
        4,
        base(4).copy(lineage = SampleLineage.interpolated.smoothed.projected)
      )
    )

    interrupted.foreach { samples =>
      assertEquals(Kinematics.velocities(samples), Right(Vector.empty))
    }
  }

  test("a microsaccade against a noise floor is detected") {
    // Noise around zero, then a small fast excursion, then noise again.
    val before = (0 until 100).map(i => at(i.toLong, noise(i), noise(i + 7000))).toVector
    val micro  = (0 until 8).map(i => at(100L + i, 0.06 * i, 0.0)).toVector
    val after  =
      (0 until 100).map(i => at(108L + i, 0.42 + noise(i + 500), noise(i + 9000))).toVector
    val trial = before ++ micro ++ after

    val th       = EkThresholds.estimate(trial, ekLambda(5.0)).toOption.get
    val detected = events(Detectors.engbertKliegl(th, ekMin3, clock).runAll(trial))
    assert(detected.nonEmpty, clue(th.render))
    assert(detected.forall(_.isInstanceOf[Event.Saccade[Deg]]))
  }

  test("a detected microsaccade reports a measured peak velocity") {
    val before = (0 until 100).map(i => at(i.toLong, noise(i), noise(i + 7000))).toVector
    val micro  = (0 until 8).map(i => at(100L + i, 0.06 * i, 0.0)).toVector
    val trial  = before ++ micro
    val th     = EkThresholds.estimate(trial, ekLambda(5.0)).toOption.get
    val sacs   = events(Detectors.engbertKliegl(th, ekMin3, clock).runAll(trial)).collect {
      case s: Event.Saccade[Deg] => s
    }
    // Unlike a saccade inferred from a fixation pair, this one measured it.
    assert(sacs.nonEmpty, clue(th.render))
    assert(sacs.forall(_.peakVelocity.isDefined), clue(sacs))
  }

  // -------------------------------------------------------------------------
  // Merging
  // -------------------------------------------------------------------------

  private def fixation(fromMs: Long, toMs: Long, x: Double) =
    Event.Fixation
      .of(
        Interval.of(clock, Instant.millis(fromMs), Instant.millis(toMs)).toOption.get,
        Pt[Deg](x, 0.0),
        dispersion = 0.1,
        method = DispersionMethod.RmsRadius,
        sampleCount = (toMs - fromMs).toInt
      )
      .toOption
      .get

  private val mergeSource = RecordingRef("merge-fixtures")

  private def mergeSeries(
      sampleCount: Int,
      positioned: Vector[(Int, Int, Double)],
      events: Vector[(Event[Deg], SampleRange)],
      lineageAt: Map[Int, SampleLineage] = Map.empty
  ): EventSeries[Deg] =
    val samples = Vector.tabulate(sampleCount) { index =>
      val sample = positioned.find { case (from, until, _) =>
        index >= from && index < until
      } match
        case Some((_, _, x)) => at(index.toLong, x, 0.0)
        case None            => missing(index.toLong)
      lineageAt.get(index).fold(sample)(lineage => sample.copy(lineage = lineage))
    }
    val recording = Recording
      .of(
        filterFrame,
        clock,
        Rate.Fixed(Hz(1000.0).toOption.get),
        Eye.Left,
        None,
        IArray.from(samples)
      )
      .toOption
      .get
    EventSeries
      .of(recording, mergeSource, events.map(_._1), events.map(_._2))
      .toOption
      .get

  private def range(from: Int, until: Int): SampleRange =
    SampleRange.of(from, until).toOption.get

  private def mergeGap(milliseconds: Long): MaximumMergeGap =
    MaximumMergeGap.of(Span.millis(milliseconds)).toOption.get

  test("fragments close in time and space are fused") {
    val series = mergeSeries(
      200,
      Vector((0, 100, 5.0), (110, 200, 5.2)),
      Vector(
        fixation(0, 100, 5.0)   -> range(0, 100),
        fixation(110, 200, 5.2) -> range(110, 200)
      ),
      Map(110 -> SampleLineage.interpolated.smoothed)
    )
    val merged = Merge
      .adjacentFixations(series, mergeGap(20), Distance.deg(0.5).toOption.get)
      .toOption
      .get

    assertEquals(merged.eventSeries.events.length, 1)
    assertEquals(merged.eventSeries.support, Vector(range(0, 200)))
    assertEquals(
      merged.eventSeries.lineage.samples(110).toVector,
      Vector(SampleOrigin.Interpolated, SampleOrigin.Smoothed)
    )
    assertEquals(merged.mergedEventCount, 1)
    assertEquals(merged.provenance.steps.last.operation, "merge-adjacent-fixations")
    val f = merged.eventSeries.events.head.asInstanceOf[Event.Fixation[Deg]]
    assertEquals(f.span.onset.toMillis, 0.0)
    assertEquals(f.span.offset.toMillis, 200.0)
    assertEquals(f.sampleCount, 190)
  }

  test("both criteria are required: a long pause in place is two fixations") {
    val series = mergeSeries(
      600,
      Vector((0, 100, 5.0), (500, 600, 5.1)),
      Vector(fixation(0, 100, 5.0) -> range(0, 100), fixation(500, 600, 5.1) -> range(500, 600))
    )
    val merged = Merge
      .adjacentFixations(series, mergeGap(20), Distance.deg(0.5).toOption.get)
      .toOption
      .get
    assertEquals(merged.eventSeries.events.length, 2)
    assertEquals(merged.mergedEventCount, 0)
  }

  test("both criteria are required: a quick move elsewhere is two fixations") {
    val series = mergeSeries(
      200,
      Vector((0, 100, 5.0), (105, 200, 25.0)),
      Vector(
        fixation(0, 100, 5.0)    -> range(0, 100),
        fixation(105, 200, 25.0) -> range(105, 200)
      )
    )
    val merged = Merge
      .adjacentFixations(series, mergeGap(20), Distance.deg(0.5).toOption.get)
      .toOption
      .get
    assertEquals(merged.eventSeries.events.length, 2)
  }

  test("the fused centre is reconstructed from source support, not copied") {
    // A 10ms fragment should barely move a 200ms fixation.
    val series = mergeSeries(
      215,
      Vector((0, 200, 0.0), (205, 215, 1.0)),
      Vector(fixation(0, 200, 0.0) -> range(0, 200), fixation(205, 215, 1.0) -> range(205, 215))
    )
    val merged = Merge
      .adjacentFixations(series, mergeGap(20), Distance.deg(1.0).toOption.get)
      .toOption
      .get
    val f = merged.eventSeries.events.head.asInstanceOf[Event.Fixation[Deg]]
    assert(f.centre.x < 0.1, clue(f.centre.x))
    assertEquals(f.sampleCount, 210)
  }

  test("a non-fixation event preserves source support and prevents fusion") {
    val blink = Event.Blink
      .of[Deg](Interval.of(clock, Instant.millis(105), Instant.millis(150)).toOption.get)
      .toOption
      .get
    val series = mergeSeries(
      200,
      Vector((0, 100, 5.0), (160, 200, 5.1)),
      Vector(
        fixation(0, 100, 5.0)   -> range(0, 100),
        blink                   -> range(105, 150),
        fixation(160, 200, 5.1) -> range(160, 200)
      )
    )
    val merged = Merge
      .adjacentFixations(series, mergeGap(20), Distance.deg(0.5).toOption.get)
      .toOption
      .get
    assertEquals(merged.eventSeries.events.length, 3)
    assertEquals(merged.eventSeries.support, series.support)
  }

  test("merge gaps are parsed and raw event streams cannot bypass source identity") {
    assertEquals(
      MaximumMergeGap.of(Span.millis(-1)),
      Left(ConfigurationError.NegativeMaximumMergeGap(Span.millis(-1)))
    )
    val errors = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.core.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Deg
      Merge.adjacentFixations[Deg](Span.millis(20), Distance.deg(0.5).toOption.get)
    """)
    assert(errors.nonEmpty, "raw events bypassed EventSeries frame, clock, and source proofs")
  }

  // -------------------------------------------------------------------------
  // Composition
  // -------------------------------------------------------------------------

  test("a filter chain and a detector compose into one machine") {
    val input = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    val pipeline: Machine[Sample[Deg], DetectionEmission[Deg]] =
      Filter
        .padMissing[Deg](padding(Span.millis(1)))
        .andThen(Filter.interpolateGaps(gap(Span.millis(10))))
        .andThen(Filter.median(filterFrame, halfWidth(2), trackedOnly))
        .andThen(ivt.machine)

    assert(events(pipeline.runAll(input)).nonEmpty)
  }

  // -------------------------------------------------------------------------
  // Configuration boundaries
  // -------------------------------------------------------------------------

  test("filter configuration parses zero and rejects negative values exactly") {
    assertEquals(
      WindowHalfWidth.of(0),
      Left(ConfigurationError.NonPositiveWindowHalfWidth(0))
    )
    assertEquals(
      WindowHalfWidth.of(-1),
      Left(ConfigurationError.NonPositiveWindowHalfWidth(-1))
    )
    assert(MissingPadding.of(Span.zero).isRight)
    assertEquals(
      MissingPadding.of(Span.millis(-1)),
      Left(ConfigurationError.NegativeMissingPadding(Span.millis(-1)))
    )
    assert(InterpolationGap.of(Span.zero).isRight)
    assertEquals(
      InterpolationGap.of(Span.millis(-1)),
      Left(ConfigurationError.NegativeInterpolationGap(Span.millis(-1)))
    )
  }

  test("detector configuration rejects every non-positive or non-finite operand exactly") {
    assertEquals(
      MinimumEventDuration.of(Span.zero),
      Left(ConfigurationError.NonPositiveMinimumEventDuration(Span.zero))
    )
    assertEquals(
      MinimumEventDuration.of(Span.millis(-1)),
      Left(ConfigurationError.NonPositiveMinimumEventDuration(Span.millis(-1)))
    )
    val zeroVelocity = Velocity.degPerSecond(0.0).toOption.get
    assertEquals(
      IvtThreshold.of(zeroVelocity),
      Left(ConfigurationError.NonPositiveIvtThreshold(0.0))
    )
    assert(
      EkThresholds.of(Double.NaN, 1.0) match
        case Left(ConfigurationError.InvalidEkThresholds(etaX, etaY)) =>
          etaX.isNaN && etaY == 1.0
        case _ => false
    )
    assertEquals(
      EkMultiplier.of(Double.PositiveInfinity),
      Left(ConfigurationError.InvalidEkMultiplier(Double.PositiveInfinity))
    )
    assertEquals(
      EkMultiplier.of(0.0),
      Left(ConfigurationError.InvalidEkMultiplier(0.0))
    )
    assertEquals(
      EkMinimumSamples.of(0),
      Left(ConfigurationError.NonPositiveEkMinimumSamples(0))
    )
  }

  test("raw configuration values cannot reach filter or detector constructors") {
    val errors = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Deg
      val frame = Frame.angular("f", 20.0, 20.0).toOption.get
      Filter.median[Deg](frame, 2, WindowObservationPolicy.RequireTracked)
      val sampling: RegularSampling = Span.millis(1)
      Filter.padMissing[Deg](Span.zero)
      Detectors.ivt(
        Velocity.degPerSecond(30.0).toOption.get,
        Span.millis(20),
        ClockId("tracker")
      )
      EkThresholds(1.0, 1.0)
    """)
    assert(errors.length >= 5, clue(errors.map(_.message)))
  }

end DetectSuite
