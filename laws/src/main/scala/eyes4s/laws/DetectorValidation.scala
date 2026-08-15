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

import eyes4s.core.*
import eyes4s.detect.SampleClass
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

/** A validated event observation used by detector validation courts.
  *
  * The spatial and velocity summaries are optional because they are meaningful
  * only for some classes. The interval, distance, and velocity values already
  * carry their local invariants in their domain types.
  */
final class ValidationEvent private (
    val label: SampleClass,
    val span: Interval,
    val centre: Option[Pt[Deg]],
    val peakVelocity: Option[Velocity[Deg]]
) derives CanEqual:

  override def equals(other: Any): Boolean = other match
    case event: ValidationEvent =>
      label == event.label && span == event.span && centre == event.centre &&
      peakVelocity == event.peakVelocity
    case _ => false

  override def hashCode: Int =
    (((label.hashCode * 31 + span.hashCode) * 31 + centre.hashCode) * 31) +
      peakVelocity.hashCode

  def shift(by: Span): ValidationEvent =
    new ValidationEvent(label, span.shift(by), centre, peakVelocity)

  def withOffset(offset: Instant): Either[TimeError, ValidationEvent] =
    Interval
      .of(span.clock, span.onset, offset)
      .map(updated => new ValidationEvent(label, updated, centre, peakVelocity))

  def withCentre(value: Option[Pt[Deg]]): ValidationEvent =
    new ValidationEvent(label, span, value, peakVelocity)

  def withPeakVelocity(value: Option[Velocity[Deg]]): ValidationEvent =
    new ValidationEvent(label, span, centre, value)

object ValidationEvent:
  private[laws] def synthetic(
      label: SampleClass,
      span: Interval,
      centre: Option[Pt[Deg]],
      peakVelocity: Option[Velocity[Deg]]
  ): ValidationEvent = new ValidationEvent(label, span, centre, peakVelocity)

  /** Project a core event into the common validation representation. */
  def fromEvent(event: Event[Deg]): ValidationEvent = event match
    case fixation: Event.Fixation[Deg] =>
      new ValidationEvent(SampleClass.Fixation, fixation.span, Some(fixation.centre), None)
    case saccade: Event.Saccade[Deg] =>
      new ValidationEvent(SampleClass.Saccade, saccade.span, None, saccade.peakVelocity)
    case blink: Event.Blink[Deg] =>
      new ValidationEvent(SampleClass.Blink, blink.span, None, None)
    case pursuit: Event.Pursuit[Deg] =>
      new ValidationEvent(SampleClass.Pursuit, pursuit.span, None, None)

/** Exact affine relationship between tracker and stimulus timestamps. */
final class AffineClockDrift private (val offset: Span, val scale: Double) derives CanEqual:
  def apply(trackerTime: Instant): Instant =
    Instant.micros(offset.toMicros + math.round(trackerTime.toMicros.toDouble * scale))

object AffineClockDrift:
  def of(offset: Span, scale: Double): Either[SyntheticGenerationError, AffineClockDrift] =
    if scale.isFinite && scale > 0.0 then Right(new AffineClockDrift(offset, scale))
    else Left(SyntheticGenerationError.InvalidClockScale(scale))

/** One tracker timestamp paired with its affine stimulus-clock image. */
final case class SyntheticClockPair(tracker: Instant, stimulus: Instant) derives CanEqual

/** A deterministic latent trajectory and every controlled degradation applied
  * to it.
  */
final class SyntheticTrajectory private (
    val seed: Long,
    val recording: Recording[Deg],
    val latentLabels: Vector[SampleClass],
    val latentEvents: Vector[ValidationEvent],
    val clockPairs: Vector[SyntheticClockPair],
    val clockDrift: AffineClockDrift,
    val maximumNoise: Distance[Deg],
    val maximumSamplingJitter: Span,
    val dropoutIndices: Set[Int]
) derives CanEqual

object SyntheticTrajectory:
  private val size           = 110
  private val basePeriodUs   = 1000L
  private val jitterPattern  = Vector(-80L, -40L, 0L, 40L, 80L)
  private val dropoutIndices = Set(70)

  /** Fixed scientific court with seed-controlled noise and jitter phase.
    *
    * The latent path contains fixation, saccade, pursuit, blink, tracker loss,
    * off-surface support, and an isolated dropout. Timestamps are irregular and
    * paired to a stimulus clock with a positive affine drift.
    */
  def reference(seed: Long): Either[SyntheticGenerationError, SyntheticTrajectory] =
    val shift =
      (((seed % jitterPattern.length) + jitterPattern.length) % jitterPattern.length).toInt
    val intervals = Vector.tabulate(size - 1) { index =>
      basePeriodUs + jitterPattern((index + shift) % jitterPattern.length)
    }
    val times   = intervals.scanLeft(0L)(_ + _).map(Instant.micros)
    val labels  = Vector.tabulate(size)(latentClass)
    val samples = labels.indices.map { index =>
      val gaze: Gaze[Deg] = labels(index) match
        case SampleClass.Blink      => Gaze.Blink()
        case SampleClass.Missing    => Gaze.Lost()
        case SampleClass.OffSurface => Gaze.OffScreen(Pt[Deg](25.0, 0.0))
        case _                      =>
          val epsilon = deterministicNoise(seed, index)
          Gaze.Tracked(Pt[Deg](idealX(index) + epsilon, -epsilon), None)
      Sample(times(index), gaze)
    }.toVector

    for
      frame <- Frame
        .angular("synthetic-validation", 40.0, 20.0)
        .left
        .map(SyntheticGenerationError.Frame.apply)
      recording <- Recording
        .of(
          frame,
          ClockId("synthetic-tracker"),
          Rate.Irregular,
          Eye.Left,
          None,
          IArray.from(samples)
        )
        .left
        .map(SyntheticGenerationError.Recording.apply)
      drift <- AffineClockDrift.of(Span.micros(3000L), 1.00025)
      peak  <- Velocity
        .degPerSecond(saccadePeak(times))
        .left
        .map(SyntheticGenerationError.Geometry.apply)
      events       <- buildEvents(recording.clock, times, intervals.last, peak)
      maximumNoise <- Distance
        .deg(math.sqrt(2.0) * 0.01)
        .left
        .map(SyntheticGenerationError.Geometry.apply)
    yield new SyntheticTrajectory(
      seed,
      recording,
      labels,
      events,
      times.map(time => SyntheticClockPair(time, drift(time))),
      drift,
      maximumNoise,
      Span.micros(80L),
      dropoutIndices
    )

  private def latentClass(index: Int): SampleClass =
    if index < 20 then SampleClass.Fixation
    else if index < 25 then SampleClass.Saccade
    else if index < 45 then SampleClass.Fixation
    else if index < 60 then SampleClass.Pursuit
    else if index < 65 then SampleClass.Blink
    else if index == 70 || (index >= 75 && index < 79) then SampleClass.Missing
    else if index < 89 then SampleClass.Fixation
    else if index < 94 then SampleClass.OffSurface
    else SampleClass.Fixation

  private def idealX(index: Int): Double =
    if index < 20 then 0.0
    else if index < 25 then (index - 20).toDouble * 2.5
    else if index < 45 then 10.0
    else if index < 60 then 10.0 + (index - 45).toDouble * 0.25
    else if index < 94 then 13.5
    else 4.0

  private def deterministicNoise(seed: Long, index: Int): Double =
    val positiveSeed = seed & Long.MaxValue
    val bucket       = ((positiveSeed + index.toLong * 37L) % 11L).toInt - 5
    bucket.toDouble * 0.002

  private def saccadePeak(times: Vector[Instant]): Double =
    (20 until 24).map { index =>
      val seconds = times(index).until(times(index + 1)).toSeconds
      math.abs(idealX(index + 1) - idealX(index)) / seconds
    }.max

  private final case class EventDefinition(
      label: SampleClass,
      from: Int,
      until: Int,
      centre: Option[Pt[Deg]],
      peakVelocity: Option[Velocity[Deg]]
  )

  private def buildEvents(
      clock: ClockId,
      times: Vector[Instant],
      finalPeriodMicros: Long,
      saccadePeak: Velocity[Deg]
  ): Either[SyntheticGenerationError, Vector[ValidationEvent]] =
    def centre(from: Int, until: Int): Pt[Deg] =
      val xs = (from until until).map(idealX)
      Pt[Deg](xs.sum / xs.length, 0.0)

    val definitions = Vector(
      EventDefinition(SampleClass.Fixation, 0, 20, Some(centre(0, 20)), None),
      EventDefinition(SampleClass.Saccade, 20, 25, None, Some(saccadePeak)),
      EventDefinition(SampleClass.Fixation, 25, 45, Some(centre(25, 45)), None),
      EventDefinition(SampleClass.Pursuit, 45, 60, None, None),
      EventDefinition(SampleClass.Blink, 60, 65, None, None),
      EventDefinition(SampleClass.Fixation, 65, 70, Some(centre(65, 70)), None),
      EventDefinition(SampleClass.Missing, 70, 71, None, None),
      EventDefinition(SampleClass.Fixation, 71, 75, Some(centre(71, 75)), None),
      EventDefinition(SampleClass.Missing, 75, 79, None, None),
      EventDefinition(SampleClass.Fixation, 79, 89, Some(centre(79, 89)), None),
      EventDefinition(SampleClass.OffSurface, 89, 94, None, None),
      EventDefinition(SampleClass.Fixation, 94, size, Some(centre(94, size)), None)
    )

    definitions.foldLeft[Either[SyntheticGenerationError, Vector[ValidationEvent]]](
      Right(Vector.empty)
    ) { (acc, definition) =>
      for
        built <- acc
        range <- SampleRange
          .of(definition.from, definition.until)
          .left
          .map(SyntheticGenerationError.Support.apply)
        offset =
          if range.until < times.length then times(range.until)
          else times.last + Span.micros(finalPeriodMicros)
        span <- Interval
          .of(clock, times(range.from), offset)
          .left
          .map(SyntheticGenerationError.Time.apply)
      yield built :+ ValidationEvent.synthetic(
        definition.label,
        span,
        definition.centre,
        definition.peakVelocity
      )
    }

enum SyntheticGenerationError derives CanEqual:
  case InvalidClockScale(scale: Double)
  case Frame(underlying: GeometryError)
  case Recording(underlying: RecordingError)
  case Geometry(underlying: GeometryError)
  case Support(underlying: DetectionSupportError)
  case Time(underlying: TimeError)

  def message: String = this match
    case InvalidClockScale(scale) =>
      s"Synthetic affine clock drift requires a finite positive scale, got scale=$scale."
    case Frame(underlying)     => s"Synthetic frame construction failed: $underlying."
    case Recording(underlying) =>
      s"Synthetic recording construction failed: ${underlying.message}"
    case Geometry(underlying) => s"Synthetic metric construction failed: $underlying."
    case Support(underlying)  => s"Synthetic event support construction failed: $underlying."
    case Time(underlying)     => s"Synthetic event interval construction failed: $underlying."

/** Precision, recall, and F1 whose truth and prediction denominators are known
  * to be non-empty.
  */
final class SampleClassificationMetrics private[laws] (
    val label: SampleClass,
    val truthCount: Int,
    val predictedCount: Int,
    val truePositiveCount: Int,
    val precision: Double,
    val recall: Double,
    val f1: Double
) derives CanEqual

/** Signed predicted-minus-reference event count for one class. */
final case class EventCountBias(label: SampleClass, predictedMinusReference: Int)
    derives CanEqual

/** Complete detector evaluation with explicit metric units.
  *
  * A successful value proves that label, matched-event, centred-event, and
  * peak-velocity denominators were all non-empty.
  */
final class DetectorValidation private (
    val perClass: Vector[SampleClassificationMetrics],
    val matchedEventCount: Int,
    val eventCountBias: Vector[EventCountBias],
    val onsetMeanAbsoluteError: Span,
    val offsetMeanAbsoluteError: Span,
    val durationMeanBias: Span,
    val centreMeanError: Distance[Deg],
    val peakVelocityMeanAbsoluteError: Velocity[Deg]
) derives CanEqual

object DetectorValidation:

  def evaluate(
      referenceLabels: Vector[SampleClass],
      predictedLabels: Vector[SampleClass],
      referenceEvents: Vector[ValidationEvent],
      predictedEvents: Vector[ValidationEvent]
  ): Either[DetectorValidationError, DetectorValidation] =
    if referenceLabels.length != predictedLabels.length then
      Left(
        DetectorValidationError
          .LabelCountMismatch(referenceLabels.length, predictedLabels.length)
      )
    else
      for
        classes    <- classificationMetrics(referenceLabels, predictedLabels)
        eventPairs <- matchedPairs(referenceEvents, predictedEvents)
        _          <- eventPairs
          .collectFirst {
            case (reference, predicted) if reference.span.clock != predicted.span.clock =>
              DetectorValidationError.EventClockMismatch(
                reference.label,
                reference.span.clock,
                predicted.span.clock
              )
          }
          .toLeft(())
        centredPairs = eventPairs.flatMap { case (reference, predicted) =>
          (reference.centre, predicted.centre) match
            case (Some(expected), Some(observed)) => Some(expected -> observed)
            case _                                => None
        }
        _ <- Either.cond(
          centredPairs.nonEmpty,
          (),
          DetectorValidationError.NoCentredEventPairs(
            referenceEvents.size,
            predictedEvents.size
          )
        )
        velocityPairs = eventPairs.flatMap { case (reference, predicted) =>
          (reference.peakVelocity, predicted.peakVelocity) match
            case (Some(expected), Some(observed)) => Some(expected -> observed)
            case _                                => None
        }
        _ <- Either.cond(
          velocityPairs.nonEmpty,
          (),
          DetectorValidationError.NoPeakVelocityPairs(
            referenceEvents.size,
            predictedEvents.size
          )
        )
        centreError <- Distance
          .deg(centredPairs.map { case (reference, predicted) =>
            Distance.between(reference, predicted).value
          }.sum / centredPairs.length)
          .left
          .map(error =>
            DetectorValidationError.InvalidNumericMetric("centreMeanErrorDeg", error)
          )
        velocityError <- Velocity
          .degPerSecond(velocityPairs.map { case (reference, predicted) =>
            math.abs(reference.value - predicted.value)
          }.sum / velocityPairs.length)
          .left
          .map(error =>
            DetectorValidationError.InvalidNumericMetric(
              "peakVelocityMeanAbsoluteErrorDegPerSecond",
              error
            )
          )
      yield
        val onsetMae = meanMicros(eventPairs) { case (reference, predicted) =>
          math.abs(predicted.span.onset.toMicros - reference.span.onset.toMicros)
        }
        val offsetMae = meanMicros(eventPairs) { case (reference, predicted) =>
          math.abs(predicted.span.offset.toMicros - reference.span.offset.toMicros)
        }
        val durationBias = meanMicros(eventPairs) { case (reference, predicted) =>
          predicted.span.duration.toMicros - reference.span.duration.toMicros
        }
        val eventClasses = SampleClass.values.filter(label =>
          referenceEvents.exists(_.label == label) || predictedEvents.exists(_.label == label)
        )
        new DetectorValidation(
          classes,
          eventPairs.length,
          eventClasses.map { label =>
            EventCountBias(
              label,
              predictedEvents.count(_.label == label) - referenceEvents.count(_.label == label)
            )
          }.toVector,
          Span.micros(onsetMae),
          Span.micros(offsetMae),
          Span.micros(durationBias),
          centreError,
          velocityError
        )

  private def classificationMetrics(
      reference: Vector[SampleClass],
      predicted: Vector[SampleClass]
  ): Either[DetectorValidationError, Vector[SampleClassificationMetrics]] =
    val present = SampleClass.values.filter(reference.contains)
    present.foldLeft[Either[DetectorValidationError, Vector[SampleClassificationMetrics]]](
      Right(Vector.empty)
    ) { (acc, label) =>
      val truthCount     = reference.count(_ == label)
      val predictedCount = predicted.count(_ == label)
      val truePositive   = reference
        .zip(predicted)
        .count { case (truth, prediction) => truth == label && prediction == label }
      for
        built <- acc
        _     <- Either.cond(
          truthCount > 0,
          (),
          DetectorValidationError.EmptyTruthDenominator(label)
        )
        _ <- Either.cond(
          predictedCount > 0,
          (),
          DetectorValidationError.EmptyPredictionDenominator(label)
        )
      yield
        val precision = truePositive.toDouble / predictedCount
        val recall    = truePositive.toDouble / truthCount
        val f1        =
          if precision + recall == 0.0 then 0.0
          else 2.0 * precision * recall / (precision + recall)
        built :+ new SampleClassificationMetrics(
          label,
          truthCount,
          predictedCount,
          truePositive,
          precision,
          recall,
          f1
        )
    }

  private def matchedPairs(
      reference: Vector[ValidationEvent],
      predicted: Vector[ValidationEvent]
  ): Either[DetectorValidationError, Vector[(ValidationEvent, ValidationEvent)]] =
    val pairs = SampleClass.values.toVector.flatMap { label =>
      val expected = reference.filter(_.label == label).sortBy(_.span.onset.toMicros)
      val observed = predicted.filter(_.label == label).sortBy(_.span.onset.toMicros)
      expected.zip(observed)
    }
    Either.cond(
      pairs.nonEmpty,
      pairs,
      DetectorValidationError.NoMatchedEvents(reference.size, predicted.size)
    )

  private def meanMicros(
      pairs: Vector[(ValidationEvent, ValidationEvent)]
  )(metric: ((ValidationEvent, ValidationEvent)) => Long): Long =
    math.round(pairs.map(metric).map(_.toDouble).sum / pairs.length)

enum DetectorValidationError derives CanEqual:
  case LabelCountMismatch(referenceCount: Int, predictedCount: Int)
  case EmptyTruthDenominator(label: SampleClass)
  case EmptyPredictionDenominator(label: SampleClass)
  case NoMatchedEvents(referenceCount: Int, predictedCount: Int)
  case NoCentredEventPairs(referenceCount: Int, predictedCount: Int)
  case NoPeakVelocityPairs(referenceCount: Int, predictedCount: Int)
  case EventClockMismatch(label: SampleClass, reference: ClockId, predicted: ClockId)
  case InvalidNumericMetric(name: String, underlying: GeometryError)

  def message: String = this match
    case LabelCountMismatch(referenceCount, predictedCount) =>
      s"Sample-label evaluation requires equal operands: referenceCount=$referenceCount, predictedCount=$predictedCount."
    case EmptyTruthDenominator(label) =>
      s"Sample-label recall for label=$label has an empty reference denominator."
    case EmptyPredictionDenominator(label) =>
      s"Sample-label precision for label=$label has an empty predicted denominator."
    case NoMatchedEvents(referenceCount, predictedCount) =>
      s"Event timing evaluation has no matched operands: referenceCount=$referenceCount, predictedCount=$predictedCount."
    case NoCentredEventPairs(referenceCount, predictedCount) =>
      s"Event centre evaluation has no centred operand pairs: referenceCount=$referenceCount, predictedCount=$predictedCount."
    case NoPeakVelocityPairs(referenceCount, predictedCount) =>
      s"Peak-velocity evaluation has no velocity operand pairs: referenceCount=$referenceCount, predictedCount=$predictedCount."
    case EventClockMismatch(label, reference, predicted) =>
      s"Event timing operands for label=$label use different clocks: reference=$reference, predicted=$predicted."
    case InvalidNumericMetric(name, underlying) =>
      s"Detector validation metric '$name' was invalid: $underlying."

end DetectorValidationError
