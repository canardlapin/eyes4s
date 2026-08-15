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

import eyes4s.core.Gaze
import eyes4s.detect.SampleClass
import eyes4s.kernel.*

class DetectorValidationSuite extends munit.FunSuite:

  private def scenario(seed: Long = 20260814L): SyntheticTrajectory =
    SyntheticTrajectory.reference(seed).fold(error => fail(error.message), identity)

  private def evaluate(
      synthetic: SyntheticTrajectory,
      labels: Vector[SampleClass],
      events: Vector[ValidationEvent]
  ): DetectorValidation =
    DetectorValidation
      .evaluate(synthetic.latentLabels, labels, synthetic.latentEvents, events)
      .fold(error => fail(error.message), identity)

  test("the deterministic court contains every declared degradation and latent class") {
    val first  = scenario()
    val replay = scenario()

    assertEquals(first.recording.samples.toVector, replay.recording.samples.toVector)
    assertEquals(first.latentLabels, replay.latentLabels)
    assertEquals(first.latentEvents, replay.latentEvents)
    assertEquals(first.clockPairs, replay.clockPairs)

    val required = Set(
      SampleClass.Fixation,
      SampleClass.Saccade,
      SampleClass.Pursuit,
      SampleClass.Blink,
      SampleClass.Missing,
      SampleClass.OffSurface
    )
    assert(required.subsetOf(first.latentLabels.toSet))
    assertEquals(first.dropoutIndices, Set(70))
    assertEquals(first.latentLabels(70), SampleClass.Missing)
    assert(
      first.recording.samples(70).gaze match
        case Gaze.Lost() => true
        case _           => false
    )

    val intervals = first.recording.samples.toVector
      .sliding(2)
      .map { pair =>
        pair.head.t.until(pair.last.t).toMicros
      }
      .toVector
    assert(intervals.distinct.length > 1, "sampling jitter collapsed to a regular grid")
    assertEquals(first.maximumSamplingJitter, Span.micros(80))
    assert(first.maximumNoise.value > 0.0)

    assertEquals(first.clockPairs.head.stimulus.toMicros, 3000L)
    assertEquals(
      first.clockPairs.last.stimulus,
      first.clockDrift(first.clockPairs.last.tracker)
    )
    assert(first.clockPairs.last.stimulus.toMicros > first.clockPairs.last.tracker.toMicros)
  }

  test("a perfect replay has unit classification scores and zero physical errors") {
    val synthetic = scenario()
    val report    = evaluate(synthetic, synthetic.latentLabels, synthetic.latentEvents)

    assert(report.perClass.nonEmpty)
    report.perClass.foreach { metric =>
      assert(metric.truthCount > 0)
      assert(metric.predictedCount > 0)
      assertEqualsDouble(metric.precision, 1.0, 0.0)
      assertEqualsDouble(metric.recall, 1.0, 0.0)
      assertEqualsDouble(metric.f1, 1.0, 0.0)
    }
    assert(report.matchedEventCount > 0)
    assert(report.eventCountBias.nonEmpty)
    assert(report.eventCountBias.forall(_.predictedMinusReference == 0))
    assertEquals(report.onsetMeanAbsoluteError, Span.zero)
    assertEquals(report.offsetMeanAbsoluteError, Span.zero)
    assertEquals(report.durationMeanBias, Span.zero)
    assertEqualsDouble(report.centreMeanError.value, 0.0, 0.0)
    assertEqualsDouble(report.peakVelocityMeanAbsoluteError.value, 0.0, 0.0)
  }

  test("sample metrics expose a controlled false positive and false negative") {
    val synthetic = scenario()
    val changed   = synthetic.latentLabels.updated(0, SampleClass.Saccade)
    val report    = evaluate(synthetic, changed, synthetic.latentEvents)
    val fixation  = report.perClass.find(_.label == SampleClass.Fixation).get
    val saccade   = report.perClass.find(_.label == SampleClass.Saccade).get

    assert(fixation.recall < 1.0)
    assertEqualsDouble(fixation.precision, 1.0, 0.0)
    assert(saccade.precision < 1.0)
    assertEqualsDouble(saccade.recall, 1.0, 0.0)
  }

  test("event metrics retain signed duration, count, centre, and velocity errors") {
    val synthetic = scenario()
    val first     = synthetic.latentEvents.head
      .withOffset(synthetic.latentEvents.head.span.offset + Span.micros(240))
      .fold(error => fail(error.toString), identity)
      .withCentre(Some(Pt[Unit2D.Deg](0.5, 0.0)))
    val saccadeIndex = synthetic.latentEvents.indexWhere(_.peakVelocity.nonEmpty)
    val saccade      = synthetic.latentEvents(saccadeIndex)
    val changedPeak  = Velocity
      .degPerSecond(saccade.peakVelocity.get.value + 125.0)
      .fold(error => fail(error.toString), identity)
    val predicted = synthetic.latentEvents
      .updated(0, first)
      .updated(saccadeIndex, saccade.withPeakVelocity(Some(changedPeak)))
      .dropRight(1)
    val report = evaluate(synthetic, synthetic.latentLabels, predicted)

    assert(report.offsetMeanAbsoluteError.toMicros > 0L)
    assert(report.durationMeanBias.toMicros > 0L)
    assert(report.centreMeanError.value > 0.0)
    assert(report.peakVelocityMeanAbsoluteError.value > 0.0)
    assert(report.eventCountBias.exists(_.predictedMinusReference == -1))
  }

  test("empty prediction denominators are rejected instead of scored vacuously") {
    val synthetic      = scenario()
    val withoutPursuit = synthetic.latentLabels.map {
      case SampleClass.Pursuit => SampleClass.Fixation
      case label               => label
    }
    val result = DetectorValidation.evaluate(
      synthetic.latentLabels,
      withoutPursuit,
      synthetic.latentEvents,
      synthetic.latentEvents
    )

    assertEquals(
      result,
      Left(DetectorValidationError.EmptyPredictionDenominator(SampleClass.Pursuit))
    )
  }

end DetectorValidationSuite
