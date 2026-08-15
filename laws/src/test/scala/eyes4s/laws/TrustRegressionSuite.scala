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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}

import scala.compiletime.testing.typeCheckErrors

/** Minimal witnesses for the scientific-correctness trust milestone.
  *
  * These are deliberately semantic assertions. Preserving a vector's length is
  * not a substitute for preserving its timestamps; detecting roughly the same
  * events is not a substitute for reporting the right physical velocity; and a
  * green law run for one configuration cannot certify a stronger public return
  * type for every configuration.
  */
class TrustRegressionSuite extends munit.FunSuite:

  private val clock        = ClockId("trust-court")
  private val width2       = WindowHalfWidth.of(2).toOption.get
  private val pad1ms       = MissingPadding.of(Span.millis(1)).toOption.get
  private val gap2ms       = InterpolationGap.of(Span.millis(2)).toOption.get
  private val min1ms       = MinimumEventDuration.of(Span.millis(1)).toOption.get
  private val min20ms      = MinimumEventDuration.of(Span.millis(20)).toOption.get
  private val ivtThreshold =
    IvtThreshold.of(Velocity.degPerSecond(30.0).toOption.get).toOption.get
  private val filterFrame = Frame.angular("trust-filter", 20.0, 20.0).toOption.get

  private def tracked(ms: Long, x: Double, y: Double = 0.0): Sample[Deg] =
    Sample(Instant.millis(ms), Gaze.Tracked(Pt[Deg](x, y), None))

  private def lost(ms: Long): Sample[Deg] =
    Sample(Instant.millis(ms), Gaze.Lost[Deg]())

  private def offScreen(ms: Long, x: Double, y: Double = 0.0): Sample[Deg] =
    Sample(Instant.millis(ms), Gaze.OffScreen(Pt[Deg](x, y)))

  private def times(samples: Vector[Sample[Deg]]): Vector[Long] =
    samples.map(_.t.toMicros)

  private def events[U <: Unit2D](emissions: Vector[DetectionEmission[U]]): Vector[Event[U]] =
    emissions.map {
      case Right(event) => event
      case Left(error)  => fail(error.message)
    }

  private val regularFilterInput =
    (0 until 10).map(i => tracked(i.toLong, i.toDouble)).toVector
  private val regularSampling = RegularSampling.from(regularFilterInput).toOption.get

  private val samplePreservingFilters = Vector(
    "pad missing"      -> Filter.padMissing[Deg](pad1ms),
    "interpolate gaps" -> Filter.interpolateGaps[Deg](gap2ms),
    "median"           -> Filter.median(
      filterFrame,
      width2,
      WindowObservationPolicy.RequireTracked
    ),
    "Savitzky-Golay" -> Filter.savitzkyGolay(
      filterFrame,
      width2,
      WindowObservationPolicy.RequireTracked,
      regularSampling
    )
  )

  samplePreservingFilters.foreach { case (name, filter) =>
    test(s"$name preserves the exact timestamp sequence and cardinality") {
      val input  = regularFilterInput
      val output = filter.runAll(input)
      assertEquals(output.length, input.length)
      assertEquals(times(output), times(input))
    }
  }

  test("Engbert-Kliegl five-point velocity has the published physical scale") {
    // x(t) = 1000 t deg/s sampled every millisecond, so every interior
    // five-point estimate is exactly 1000 deg/s.
    val ramp       = (0 until 9).map(i => tracked(i.toLong, i.toDouble)).toVector
    val velocities = Kinematics.velocities(ramp).toOption.get

    assert(velocities.nonEmpty)
    velocities.foreach { case (_, velocity) =>
      assertEqualsDouble(velocity.dx, 1000.0, 1e-12)
      assertEqualsDouble(velocity.dy, 0.0, 1e-12)
    }
  }

  test("the regular-grid Engbert-Kliegl estimator refuses irregular timestamps") {
    val irregular = Vector(
      tracked(0, 0.0),
      tracked(1, 1.0),
      tracked(3, 3.0),
      tracked(6, 6.0),
      tracked(10, 10.0)
    )

    assert(
      Kinematics.velocities(irregular) match
        case Left(
              KinematicsError.InvalidSampling(
                ConfigurationError.IrregularSamplingInterval(
                  2,
                  expected,
                  observed
                )
              )
            ) =>
          expected == Span.millis(1) && observed == Span.millis(2)
        case _ => false
    )
  }

  test("canonical I-DT advances a failed minimum-duration window by one sample") {
    val extent = Extent.square[Deg](1.0).toOption.get
    val input  =
      Vector(tracked(0, 0.0)) ++
        (1 until 10).map(i => tracked(i.toLong, 1.0)) ++
        (10 until 40).map(i => tracked(i.toLong, 1.1))

    val fixations = events(Detectors.idt(extent, min20ms, clock).runAll(input))
      .collect { case fixation: Event.Fixation[Deg] => fixation }

    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 1.0, 1e-12)
  }

  test("canonical I-DT excludes an isolated outlier without losing either fixation") {
    val extent = Extent.square[Deg](1.0).toOption.get
    val input  =
      (0 until 30).map(i => tracked(i.toLong, 0.0)).toVector ++
        Vector(tracked(30, 10.0)) ++
        (31 until 61).map(i => tracked(i.toLong, 0.0))

    val fixations = events(Detectors.idt(extent, min20ms, clock).runAll(input))
      .collect { case fixation: Event.Fixation[Deg] => fixation }

    assertEquals(fixations.length, 2)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 0.0, 1e-12)
    assertEqualsDouble(fixations.last.span.onset.toMillis, 31.0, 1e-12)
  }

  test("canonical I-DT expands a qualifying minimum window to its maximal fixation") {
    val extent = Extent.square[Deg](1.0).toOption.get
    val input  = (0 until 41).map(i => tracked(i.toLong, i.toDouble / 50.0)).toVector

    val fixations = events(Detectors.idt(extent, min20ms, clock).runAll(input))
      .collect { case fixation: Event.Fixation[Deg] => fixation }

    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 0.0, 1e-12)
    assertEqualsDouble(fixations.head.span.offset.toMillis, 41.0, 1e-12)
  }

  test("I-VT does not let off-screen outer samples create a saccade") {
    val input = Vector(
      offScreen(0, 0.0),
      tracked(1, 0.0),
      offScreen(2, 10.0),
      tracked(3, 10.0),
      tracked(4, 10.0)
    )

    val saccades = events(Detectors.ivt(ivtThreshold, min1ms, clock).runAll(input))
      .collect { case saccade: Event.Saccade[Deg] => saccade }

    assertEquals(saccades, Vector.empty)
  }

  test("I-VT does not include a missing middle sample in a fixation interval") {
    val input = Vector(
      tracked(0, 0.0),
      lost(1),
      tracked(2, 0.0),
      tracked(3, 0.0),
      tracked(4, 0.0)
    )

    val fixations = events(Detectors.ivt(ivtThreshold, min1ms, clock).runAll(input))
      .collect { case fixation: Event.Fixation[Deg] => fixation }

    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.span.onset.toMillis, 2.0, 1e-12)
  }

  test("a sub-duration I-VT run is not silently bridged into adjacent fixations") {
    val before = (0 until 60).map(i => tracked(i.toLong, 0.0)).toVector
    val twitch = (60 until 65).map(i => tracked(i.toLong, (i - 60).toDouble)).toVector
    val after  = (65 until 125).map(i => tracked(i.toLong, 5.0)).toVector

    val detected =
      events(Detectors.ivt(ivtThreshold, min20ms, clock).runAll(before ++ twitch ++ after))
    val fixations = detected.collect { case fixation: Event.Fixation[Deg] => fixation }
    val saccades  = detected.collect { case saccade: Event.Saccade[Deg] => saccade }

    assertEquals(fixations.length, 2)
    assertEquals(saccades, Vector.empty)
    assert(fixations.head.span.offset.toMicros < fixations.last.span.onset.toMicros)
  }

  test("irregular occupancy mass equals the recording's represented duration") {
    val frame     = Frame.angular("irregular", 20.0, 20.0).toOption.get
    val recording = Recording
      .of(
        frame,
        clock,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray(tracked(0, 0.0), tracked(10, 1.0), tracked(30, 2.0))
      )
      .toOption
      .get

    val occupancy = recording.occupancy.toOption.get
    assertEqualsDouble(occupancy.measure.total, recording.duration.toSeconds, 1e-12)
  }

  test("same nominal frame identity cannot certify conflicting specifications") {
    val smallBounds = Bounds.sized[Deg](10.0, 10.0).toOption.get
    val largeBounds = Bounds.sized[Deg](20.0, 20.0).toOption.get
    val small       = Frame.of(FrameId("shared"), smallBounds, YAxis.Up)
    val large       = Frame.of(FrameId("shared"), largeBounds, YAxis.Up)

    assert(Agreement.frames(small, large).isLeft)
  }

  test("same nominal grid identity cannot certify conflicting specifications") {
    val bounds = Bounds.sized[Deg](10.0, 10.0).toOption.get
    val frame  = Frame.of(FrameId("shared"), bounds, YAxis.Up)

    val coarse = Grid.of(GridId("shared-grid"), frame, 2, 2).toOption.get
    val fine   = Grid.of(GridId("shared-grid"), frame, 4, 4).toOption.get
    assert(Agreement.grids(coarse, fine).isLeft)
  }

  test("clamping into half-open bounds produces an interior point") {
    val bounds  = Bounds.sized[Deg](10.0, 10.0).toOption.get
    val clamped = bounds.clamp(Pt[Deg](10.0, 11.0))

    assert(bounds.contains(clamped), clue(clamped))
  }

  test("finite projected Wasserstein is not exposed as a Metric") {
    val frame = Frame.screen("projection-counterexample", 2, 2).toOption.get
    val grid  = Grid.square(frame, 2).toOption.get

    def spike(index: Int): Mass[Px] =
      Surface
        .mass(
          grid,
          IArray.tabulate(grid.size)(i => if i == index then 1.0 else 0.0),
          Provenance.raw(ContentHash.empty)
        )
        .toOption
        .get

    val topLeft      = spike(0)
    val bottomLeft   = spike(2)
    val oneDirection = ProjectionDirections.of(1).toOption.get
    val projected    = Transport.slicedWasserstein[Px](oneDirection)

    // One horizontal projection cannot distinguish the two vertical masses.
    assertEqualsDouble(projected.compare(topLeft, bottomLeft).toOption.get.value, 0.0, 1e-12)

    val errors = typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val directions = ProjectionDirections.of(1).toOption.get
      val metric: Metric[Mass[Px]] = Transport.slicedWasserstein[Px](directions)
    """)
    assert(errors.nonEmpty, "a finite projection approximation claimed full Metric laws")
  }

  test("microsaccade peak-velocity evidence is non-vacuous") {
    def noise(i: Int): Double =
      val h = (i * 1103515245 + 12345) & 0x7fffffff
      ((h % 2000) - 1000) / 1000.0 * 0.03

    val before =
      (0 until 100).map(i => tracked(i.toLong, noise(i), noise(i + 7000))).toVector
    val micro      = (0 until 8).map(i => tracked(100L + i, 0.06 * i)).toVector
    val trial      = before ++ micro
    val lambda5    = EkMultiplier.of(5.0).toOption.get
    val minSamples = EkMinimumSamples.of(3).toOption.get
    val thresholds = EkThresholds.estimate(trial, lambda5).toOption.get
    val saccades = events(Detectors.engbertKliegl(thresholds, minSamples, clock).runAll(trial))
      .collect { case saccade: Event.Saccade[Deg] => saccade }

    assert(saccades.nonEmpty, clue(thresholds.render))
    assert(saccades.forall(_.peakVelocity.isDefined), clue(saccades))
  }

end TrustRegressionSuite
