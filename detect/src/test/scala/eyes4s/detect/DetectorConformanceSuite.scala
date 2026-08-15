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

/** Cross-platform court for the pinned fixtures in
  * `tools/detector-conformance/reference.json`.
  *
  * Values are embedded because the suite runs on JavaScript as well as the
  * JVM. The checked-in JSON remains the machine-readable source artifact and
  * is regenerated independently with pymovements 0.26.2.
  */
class DetectorConformanceSuite extends munit.FunSuite:

  private final case class ReferenceFixation(
      onsetMillis: Double,
      offsetMillis: Double,
      centreX: Double,
      centreY: Double
  ):
    def durationMillis: Double = offsetMillis - onsetMillis

  private final case class EventErrorReport(
      eventCountError: Int,
      onsetAbsoluteErrorMillis: Vector[Double],
      offsetAbsoluteErrorMillis: Vector[Double],
      durationAbsoluteErrorMillis: Vector[Double],
      centreEuclideanErrorDeg: Vector[Double]
  ):
    def allErrors: Vector[Double] =
      onsetAbsoluteErrorMillis ++ offsetAbsoluteErrorMillis ++
        durationAbsoluteErrorMillis ++ centreEuclideanErrorDeg

  private final case class PhysicalVelocityErrorReport(
      absoluteErrorDegPerSecond: Vector[Double]
  )

  private val clock = ClockId("pymovements-0.26.2")

  private def at(index: Int, x: Double, y: Double = 0.0): Sample[Deg] =
    Sample(Instant.millis(index.toLong), Gaze.Tracked(Pt[Deg](x, y), None))

  private def samples(xs: Vector[Double]): Vector[Sample[Deg]] =
    xs.zipWithIndex.map { case (x, index) => at(index, x) }

  private def events(emissions: Vector[DetectionEmission[Deg]]): Vector[Event[Deg]] =
    emissions.map {
      case Right(event) => event
      case Left(error)  => fail(error.message)
    }

  /** Produce a report only after proving the result is non-empty and cardinality
    * compatible. This ordering prevents `forall` and `zip` from certifying an
    * empty detector output vacuously.
    */
  private def compareFixations(
      obtained: Vector[Event.Fixation[Deg]],
      expected: Vector[ReferenceFixation]
  ): EventErrorReport =
    assert(expected.nonEmpty, "the conformance oracle must contain an event")
    assert(obtained.nonEmpty, "the detector returned no events for a non-empty oracle")
    val countError = math.abs(obtained.length - expected.length)
    assertEquals(countError, 0, "event counts must agree before pairwise metrics are computed")

    val pairs = obtained.zip(expected)
    EventErrorReport(
      countError,
      pairs.map { case (event, reference) =>
        math.abs(event.span.onset.toMillis - reference.onsetMillis)
      },
      pairs.map { case (event, reference) =>
        math.abs(event.span.offset.toMillis - reference.offsetMillis)
      },
      pairs.map { case (event, reference) =>
        math.abs(event.duration.toMillis - reference.durationMillis)
      },
      pairs.map { case (event, reference) =>
        math.hypot(event.centre.x - reference.centreX, event.centre.y - reference.centreY)
      }
    )

  private def assertZero(report: EventErrorReport): Unit =
    assertEquals(report.eventCountError, 0)
    assert(report.allErrors.nonEmpty, "a report with no measured errors is not evidence")
    report.allErrors.foreach(error => assertEqualsDouble(error, 0.0, 1e-12))

  test("I-VT agrees after the oracle's inclusive offsets are mapped to half-open support") {
    val input     = samples(Vector(0.0, 0.0, 0.0, 0.0, 5.0, 10.0, 10.0, 10.0, 10.0, 10.0))
    val threshold = IvtThreshold
      .of(Velocity.degPerSecond(2000.0).toOption.get)
      .toOption
      .get
    val minimum  = MinimumEventDuration.of(Span.millis(2)).toOption.get
    val detected = events(Detectors.ivt(threshold, minimum, clock).runAll(input))

    assert(detected.nonEmpty, "the hand-computable I-VT fixture must detect events")
    assertEquals(
      detected.map {
        case _: Event.Fixation[Deg] => "fixation"
        case _: Event.Saccade[Deg]  => "saccade"
        case _                      => "other"
      },
      Vector("fixation", "saccade", "fixation")
    )

    val report = compareFixations(
      detected.collect { case fixation: Event.Fixation[Deg] => fixation },
      Vector(
        ReferenceFixation(0.0, 3.0, 0.0, 0.0),
        ReferenceFixation(6.0, 10.0, 10.0, 0.0)
      )
    )
    assertZero(report)

    val saccade = detected.collect { case event: Event.Saccade[Deg] => event }.head
    assertEqualsDouble(saccade.span.onset.toMillis, 3.0, 1e-12)
    assertEqualsDouble(saccade.span.offset.toMillis, 6.0, 1e-12)
    assertEqualsDouble(saccade.from.x, 0.0, 1e-12)
    assertEqualsDouble(saccade.to.x, 10.0, 1e-12)
  }

  test("I-DT advances past initial noise to the same stable window as the oracle") {
    val input    = samples(Vector(9.0, 1.0, 1.1, 1.0, 1.1, 1.0))
    val extent   = Extent.square[Deg](0.5).toOption.get
    val minimum  = MinimumEventDuration.of(Span.millis(2)).toOption.get
    val detected = events(Detectors.idt(extent, minimum, clock).runAll(input)).collect {
      case fixation: Event.Fixation[Deg] => fixation
    }

    val report = compareFixations(
      detected,
      Vector(ReferenceFixation(1.0, 6.0, 1.04, 0.0))
    )
    assertZero(report)
  }

  test("I-DT reports its violating-sample policy as a deliberate non-zero deviation") {
    val input    = samples(Vector(0.0, 0.1, 0.0, 0.1, 9.0, 1.0, 1.1, 1.0, 1.1, 1.0))
    val detector = Detectors.idt(
      Extent.square[Deg](0.5).toOption.get,
      MinimumEventDuration.of(Span.millis(2)).toOption.get,
      clock
    )
    val detected = events(detector.runAll(input)).collect {
      case fixation: Event.Fixation[Deg] => fixation
    }

    assert(
      detector.card.deviations.contains(
        DetectorDeviation.ViolatingSampleStartsNextCandidate
      )
    )
    val eyes4sReport = compareFixations(
      detected,
      Vector(
        ReferenceFixation(0.0, 4.0, 0.05, 0.0),
        ReferenceFixation(5.0, 10.0, 1.04, 0.0)
      )
    )
    assertZero(eyes4sReport)

    val oracleReport = compareFixations(
      detected,
      Vector(
        ReferenceFixation(0.0, 5.0, 1.84, 0.0),
        ReferenceFixation(5.0, 10.0, 1.04, 0.0)
      )
    )
    assertEquals(oracleReport.eventCountError, 0)
    assertEquals(oracleReport.onsetAbsoluteErrorMillis, Vector(0.0, 0.0))
    assertEquals(oracleReport.offsetAbsoluteErrorMillis, Vector(1.0, 0.0))
    assertEquals(oracleReport.durationAbsoluteErrorMillis, Vector(1.0, 0.0))
    assertEqualsDouble(oracleReport.centreEuclideanErrorDeg.head, 1.79, 1e-12)
    assertEqualsDouble(oracleReport.centreEuclideanErrorDeg.last, 0.0, 1e-12)
  }

  test("Engbert-Kliegl physical velocities have a non-vacuous zero-error report") {
    val input    = samples(Vector(0.0, 0.0, 0.0, 1.0, 3.0, 6.0, 8.0, 9.0, 9.0))
    val expected = Vector(
      666.6666666666666, 1500.0, 2166.6666666666665, 2166.6666666666665, 1500.0
    )
    val obtained = Kinematics.velocities(input).toOption.get.map(_._2.dx)

    assert(expected.nonEmpty, "the physical-velocity oracle must contain values")
    assert(obtained.nonEmpty, "the estimator returned no physical velocities")
    assertEquals(obtained.length, expected.length)
    val report = PhysicalVelocityErrorReport(
      obtained.zip(expected).map { case (actual, reference) => math.abs(actual - reference) }
    )
    assert(report.absoluteErrorDegPerSecond.nonEmpty)
    report.absoluteErrorDegPerSecond.foreach(error => assertEqualsDouble(error, 0.0, 1e-12))
  }

end DetectorConformanceSuite
