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

/** Per-axis velocity thresholds for the Engbert–Kliegl detector.
  *
  * ==Why this is a separate value, and not a parameter of the detector==
  *
  * The thresholds are `lambda` times a **median-based** estimate of the
  * velocity distribution's spread, taken over a whole trial. That is not a
  * single-pass quantity: you cannot know the median of a series until you have
  * seen all of it.
  *
  * So Engbert–Kliegl is genuinely a two-pass algorithm, and this type says so.
  * The alternative -- accepting a `lambda` on the detector and computing the
  * median internally -- would either buffer the entire recording, quietly
  * making a "streaming" detector unbounded in memory, or use a running estimate
  * and silently stop being the published method. Neither is a good trade for
  * the appearance of uniformity.
  *
  * Estimate once with [[EkThresholds.estimate]], then detect. On a live stream,
  * estimate from a calibration period and carry the thresholds forward, which
  * is what an online experiment does anyway.
  */
final class EkThresholds private (val etaX: Double, val etaY: Double) derives CanEqual:
  def render: String = f"eta=($etaX%.2f, $etaY%.2f) deg/s"

  override def equals(other: Any): Boolean = other match
    case that: EkThresholds => etaX == that.etaX && etaY == that.etaY
    case _                  => false

  override def hashCode: Int = 31 * etaX.hashCode + etaY.hashCode

  override def toString: String = s"EkThresholds($etaX,$etaY)"

object EkThresholds:

  /** Parse externally supplied per-axis thresholds. */
  def of(etaX: Double, etaY: Double): Either[ConfigurationError, EkThresholds] =
    if etaX.isFinite && etaX > 0.0 && etaY.isFinite && etaY > 0.0 then
      Right(new EkThresholds(etaX, etaY))
    else Left(ConfigurationError.InvalidEkThresholds(etaX, etaY))

  /** Publication-defined median-square spread:
    * `sqrt(median(v^2) - median(v)^2)`.
    *
    * Median rather than mean throughout, which is the point of the method: a
    * few large saccades in the trial do not inflate the threshold that is
    * supposed to detect small movements against the noise floor. The pinned
    * Python toolbox instead takes the median of squared deviations from the
    * median. That source-level difference is recorded on the algorithm card
    * rather than hidden behind a shared algorithm name.
    */
  private[detect] def medianSquareSpread(vs: Vector[Double]): Double =
    if vs.isEmpty then 0.0
    else
      def med(xs: Vector[Double]): Double =
        val a     = xs.sorted
        val upper = a.length / 2
        if a.length % 2 == 0 then (a(upper - 1) + a(upper)) / 2.0
        else a(upper)
      val m2 = med(vs.map(v => v * v))
      val m  = med(vs)
      val d  = m2 - m * m
      if d > 0.0 then math.sqrt(d) else 0.0

  /** Estimate from a whole trial. Failure distinguishes insufficient usable
    * velocities from a degenerate axis, for which no threshold is meaningful.
    */
  def estimate(
      samples: Vector[Sample[Deg]],
      lambda: EkMultiplier
  ): Either[EkEstimationError, EkThresholds] =
    Kinematics
      .velocities(samples)
      .left
      .map(EkEstimationError.InvalidSampling.apply)
      .flatMap { vs =>
        if vs.length < 5 then Left(EkEstimationError.InsufficientVelocities(vs.length, 5))
        else
          val ex = lambda.value * medianSquareSpread(vs.map(_._2.dx))
          val ey = lambda.value * medianSquareSpread(vs.map(_._2.dy))
          EkThresholds
            .of(ex, ey)
            .left
            .map(_ => EkEstimationError.DegenerateVelocitySpread(ex, ey, lambda.value))
      }

end EkThresholds

/** A whole-trial Engbert-Kliegl threshold could not be estimated. */
enum EkEstimationError derives CanEqual:
  case InvalidSampling(underlying: KinematicsError)
  case InsufficientVelocities(available: Int, required: Int)
  case DegenerateVelocitySpread(
      etaXDegPerSecond: Double,
      etaYDegPerSecond: Double,
      lambda: Double
  )

  def message: String = this match
    case InvalidSampling(underlying) =>
      s"Engbert-Kliegl threshold estimation rejected its sample times: ${underlying.message}"
    case InsufficientVelocities(available, required) =>
      s"Engbert-Kliegl threshold estimation needs at least $required usable velocities, got available=$available."
    case DegenerateVelocitySpread(etaX, etaY, lambda) =>
      s"Engbert-Kliegl threshold estimation is degenerate: etaX=$etaX deg/s, etaY=$etaY deg/s, lambda=$lambda."

end EkEstimationError

/** Velocity estimation shared by the detectors that need a profile rather than
  * a single classification.
  */
object Kinematics:

  /** Five-point moving-average velocity, as in Engbert & Kliegl (2003):
    * `v_n = (x_{n+2} + x_{n+1} - x_{n-1} - x_{n-2}) / (6 dt)`.
    *
    * Smoother than a central difference and with the same symmetry, which
    * matters because the threshold is derived from the velocity distribution --
    * a noisier estimator raises its own threshold and detects less.
    *
    * This is a regular-grid estimator. Timestamp spacing is parsed from the
    * complete input before any velocity is returned, so an irregular sequence
    * cannot acquire the published algorithm's name by dividing through a
    * convenient overall span. Returns one entry per interior sample whose five
    * observations are tracked and not interpolated. Samples near a gap simply
    * have no velocity, rather than one computed across the gap.
    */
  def velocities(
      samples: Vector[Sample[Deg]]
  ): Either[KinematicsError, Vector[(Instant, Vec2[Deg])]] =
    if samples.length < 5 then Right(Vector.empty)
    else
      RegularSampling
        .from(samples)
        .left
        .map(KinematicsError.InvalidSampling.apply)
        .map(sampling => velocitiesAtPeriod(samples, sampling.period))

  private def velocitiesAtPeriod(
      samples: Vector[Sample[Deg]],
      period: Span
  ): Vector[(Instant, Vec2[Deg])] =
    val denominator = 6.0 * period.toSeconds
    (2 until samples.length - 2).view.flatMap { n =>
      val window = (n - 2 to n + 2).map(samples.apply)
      val points = window.map { sample =>
        sample.gaze match
          case Gaze.Tracked(position, _)
              if !sample.lineage.contains(SampleOrigin.Interpolated) =>
            Some(position)
          case _ => None
      }
      if points.forall(_.isDefined) then
        val ps = points.flatten
        val dx = (ps(4).x + ps(3).x - ps(1).x - ps(0).x) / denominator
        val dy = (ps(4).y + ps(3).y - ps(1).y - ps(0).y) / denominator
        Some(samples(n).t -> Vec2[Deg](dx, dy))
      else None
    }.toVector

end Kinematics

/** A regular-grid physical velocity could not be computed. */
enum KinematicsError derives CanEqual:
  case InvalidSampling(underlying: ConfigurationError)

  def message: String = this match
    case InvalidSampling(underlying) =>
      s"Engbert-Kliegl kinematics rejected its timestamp sequence: ${underlying.message}"

end KinematicsError

extension (d: Detectors.type)

  /** Engbert–Kliegl microsaccade detection.
    *
    * A movement is detected where the velocity leaves the ellipse defined by
    * the per-axis thresholds -- `(vx/etaX)^2 + (vy/etaY)^2 > 1` -- for at least
    * `minSamples` consecutive samples.
    *
    * The elliptic criterion rather than a single speed threshold is what makes
    * this robust to trackers whose horizontal and vertical noise differ, which
    * is most of them.
    *
    * Unlocks the analyses a fixation-table library cannot reach at all:
    * microsaccade rate signatures, direction bias under covert attention, and
    * the main sequence.
    */
  def engbertKliegl(
      thresholds: EkThresholds,
      minSamples: EkMinimumSamples,
      clock: ClockId
  ): EventDetector[Deg] =
    val requiredSamples = minSamples.value
    val machine         = Machine(
      new Detector[EkState, Sample[Deg], DetectionEmission[Deg]]:
        def init: EkState = EkState(Vector.empty, Vector.empty)

        def step(st: EkState, s: Sample[Deg]): (EkState, Vector[DetectionEmission[Deg]]) =
          val window = (st.window :+ s).takeRight(5)
          if window.length < 5 then (EkState(window, st.run), Vector.empty)
          else
            Kinematics.velocities(window) match
              case Left(error) =>
                (
                  EkState(window, Vector.empty),
                  Vector(Left(DetectionFailure.Kinematics(error)))
                )
              case Right(profile) =>
                val centre   = window(2)
                val velocity = profile.headOption.map(_._2)
                velocity match
                  case Some(value) if {
                        val rx = value.dx / thresholds.etaX
                        val ry = value.dy / thresholds.etaY
                        rx * rx + ry * ry > 1.0
                      } =>
                    val period = window(0).t.until(window(1).t)
                    val point  = EkPoint(centre, value, period)
                    (EkState(window, st.run :+ point), Vector.empty)
                  case _ => (EkState(window, Vector.empty), emit(st.run))

        def flush(st: EkState): Vector[DetectionEmission[Deg]] = emit(st.run)

        private def emit(run: Vector[EkPoint]): Vector[DetectionEmission[Deg]] =
          if run.length < requiredSamples then Vector.empty
          else
            val ps = run.flatMap(_.sample.gaze.position)
            if ps.length < 2 then Vector.empty
            else
              Interval
                .of(clock, run.head.sample.t, run.last.sample.t + run.last.period)
                .toOption match
                case Some(iv) =>
                  val peak = run
                    .map(_.velocity.norm)
                    .maxOption
                    .flatMap(value => Velocity.perSecond[Deg](value).toOption)
                  Vector(
                    Event.Saccade
                      .of(iv, ps.head, ps.last, peak)
                      .left
                      .map(DetectionFailure.EventSummary.apply)
                      .map(event => event: Event[Deg])
                  )
                case None => Vector.empty
    )
    new EventDetector(
      AlgorithmCards.engbertKliegl,
      machine,
      Vector(
        "etaXDegPerSecond" -> Provenance.Param.Num(thresholds.etaX),
        "etaYDegPerSecond" -> Provenance.Param.Num(thresholds.etaY),
        "minimumSamples"   -> Provenance.Param.Num(requiredSamples.toDouble)
      )
    )

private final case class EkState(
    window: Vector[Sample[Deg]],
    run: Vector[EkPoint]
)

private final case class EkPoint(
    sample: Sample[Deg],
    velocity: Vec2[Deg],
    period: Span
)

/** Post-detection cleanup. */
object Merge:

  /** Fuse consecutive source-supported fixations separated by a short gap and
    * a short distance.
    *
    * Detectors split a single fixation whenever a stray sample crosses the
    * threshold, and the fragments are usually a few milliseconds and a fraction
    * of a degree apart. Both criteria are required: a long pause in the same
    * place is two fixations, and so is a quick move to somewhere else.
    *
    * The input is an [[EventSeries]], not a raw event stream. Its constructor
    * has already proved one frame, one clock, one source recording, and exact
    * ordered sample ranges. Merging combines those ranges and reconstructs the
    * fixation summary from source samples, so no clock is relabelled and no
    * stale centre or dispersion is carried forward.
    */
  def adjacentFixations[U <: Unit2D](
      series: EventSeries[U],
      maxGap: MaximumMergeGap,
      maxSeparation: Distance[U]
  ): Either[MergeError, MergeResult[U]] =
    val initial: Either[MergeError, Vector[(Event[U], SampleRange)]] = Right(Vector.empty)
    val merged                                                       = series.events
      .zip(series.support)
      .zipWithIndex
      .foldLeft(initial) { case (acc, ((event, support), index)) =>
        acc.flatMap { built =>
          built.lastOption match
            case Some((left: Event.Fixation[U], leftSupport)) =>
              event match
                case right: Event.Fixation[U] if joinable(left, right, maxGap, maxSeparation) =>
                  fuse(series, left, leftSupport, right, support, index).map { combined =>
                    built.dropRight(1) :+ combined
                  }
                case _ => Right(built :+ (event -> support))
            case _ => Right(built :+ (event -> support))
        }
      }

    for
      values  <- merged
      rebuilt <- EventSeries
        .of(
          series.recording,
          series.source,
          values.map(_._1),
          values.map(_._2)
        )
        .left
        .map(MergeError.SourceSupport(series.source, _))
    yield new MergeResult(
      rebuilt,
      Provenance
        .raw(series.recording.contentHash)
        .andThen(
          Provenance.Step(
            "merge-adjacent-fixations",
            Vector(
              "maximumGapMicros"  -> Provenance.Param.Num(maxGap.span.toMicros.toDouble),
              "maximumSeparation" -> Provenance.Param.Num(maxSeparation.value)
            )
          )
        ),
      series.size - rebuilt.size
    )

  private def joinable[U <: Unit2D](
      left: Event.Fixation[U],
      right: Event.Fixation[U],
      maxGap: MaximumMergeGap,
      maxSeparation: Distance[U]
  ): Boolean =
    val gap = left.span.offset.until(right.span.onset)
    gap.toMicros >= 0 && gap.toMicros <= maxGap.span.toMicros &&
    Distance.between(left.centre, right.centre) <= maxSeparation

  private def fuse[U <: Unit2D](
      series: EventSeries[U],
      left: Event.Fixation[U],
      leftSupport: SampleRange,
      right: Event.Fixation[U],
      rightSupport: SampleRange,
      rightIndex: Int
  ): Either[MergeError, (Event[U], SampleRange)] =
    for
      span <- Interval
        .of(series.clock, left.span.onset, right.span.offset)
        .left
        .map(MergeError.Interval(series.source, rightIndex - 1, rightIndex, _))
      support <- SampleRange
        .of(leftSupport.from, rightSupport.until)
        .left
        .map(MergeError.Range(series.source, rightIndex - 1, rightIndex, _))
      placeholder <- Event.Fixation
        .withoutDispersion(
          span,
          midpoint(left.centre, right.centre),
          left.sampleCount + right.sampleCount
        )
        .left
        .map(MergeError.Event(series.source, rightIndex - 1, rightIndex, _))
    yield (placeholder: Event[U]) -> support

  private def midpoint[U <: Unit2D](left: Pt[U], right: Pt[U]): Pt[U] =
    Pt[U]((left.x + right.x) / 2.0, (left.y + right.y) / 2.0)

end Merge

/** Source-supported event series and the exact fusion operation applied. */
final class MergeResult[U <: Unit2D] private[detect] (
    val eventSeries: EventSeries[U],
    val provenance: Provenance,
    val mergedEventCount: Int
)

/** Adjacent fixation fragments could not be fused without losing evidence. */
enum MergeError derives CanEqual:
  case Interval(
      source: RecordingRef,
      leftEventIndex: Int,
      rightEventIndex: Int,
      underlying: TimeError
  )
  case Range(
      source: RecordingRef,
      leftEventIndex: Int,
      rightEventIndex: Int,
      underlying: DetectionSupportError
  )
  case Event(
      source: RecordingRef,
      leftEventIndex: Int,
      rightEventIndex: Int,
      underlying: CoreError
  )
  case SourceSupport(source: RecordingRef, underlying: DetectionSupportError)

  def message: String = this match
    case Interval(source, left, right, underlying) =>
      s"Recording '$source' could not join fixation events[$left,$right]: ${underlying.message}"
    case Range(source, left, right, underlying) =>
      s"Recording '$source' could not combine fixation support[$left,$right]: ${underlying.message}"
    case Event(source, left, right, underlying) =>
      s"Recording '$source' could not construct merged fixation[$left,$right]: ${underlying.message}"
    case SourceSupport(source, underlying) =>
      s"Recording '$source' rejected merged source support: ${underlying.message}"

end MergeError
