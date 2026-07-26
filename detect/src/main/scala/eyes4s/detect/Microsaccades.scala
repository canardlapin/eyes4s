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
final case class EkThresholds(etaX: Double, etaY: Double) derives CanEqual:
  def render: String = f"eta=($etaX%.2f, $etaY%.2f) deg/s"

object EkThresholds:

  /** Median-based spread, as in the original: `sqrt(median(v^2) - median(v)^2)`.
    *
    * Median rather than mean throughout, which is the point of the method: a
    * few large saccades in the trial do not inflate the threshold that is
    * supposed to detect small movements against the noise floor.
    */
  private def medianSd(vs: Vector[Double]): Double =
    if vs.isEmpty then 0.0
    else
      def med(xs: Vector[Double]): Double =
        val a = xs.sorted
        a(a.length / 2)
      val m2 = med(vs.map(v => v * v))
      val m  = med(vs)
      val d  = m2 - m * m
      if d > 0.0 then math.sqrt(d) else 0.0

  /** Estimate from a whole trial. `None` when there is too little usable data
    * or the eye never moved, in which case no threshold is meaningful.
    */
  def estimate(samples: Vector[Sample[Deg]], lambda: Double): Option[EkThresholds] =
    val vs = Kinematics.velocities(samples)
    if vs.length < 5 then None
    else
      val ex = lambda * medianSd(vs.map(_._2.dx))
      val ey = lambda * medianSd(vs.map(_._2.dy))
      if ex > 0.0 && ey > 0.0 then Some(EkThresholds(ex, ey)) else None

end EkThresholds

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
    * Returns one entry per interior sample that has five usable neighbours.
    * Samples near a gap simply have no velocity, rather than one computed
    * across the gap.
    */
  def velocities(samples: Vector[Sample[Deg]]): Vector[(Instant, Vec2[Deg])] =
    if samples.length < 5 then Vector.empty
    else
      (2 until samples.length - 2).view.flatMap { n =>
        val w  = (n - 2 to n + 2).map(samples.apply)
        val ps = w.flatMap(_.gaze.position)
        if ps.length < 5 then None
        else
          val dt = samples(n - 2).t.until(samples(n + 2).t).toSeconds
          if dt <= 0.0 then None
          else
            // The published weights, over the interval the five samples span.
            val dx = (ps(4).x + ps(3).x - ps(1).x - ps(0).x) / (3.0 * dt)
            val dy = (ps(4).y + ps(3).y - ps(1).y - ps(0).y) / (3.0 * dt)
            Some(samples(n).t -> Vec2[Deg](dx, dy))
      }.toVector

end Kinematics

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
      minSamples: Int,
      clock: ClockId
  ): Machine[Sample[Deg], Event[Deg]] =
    Machine(
      new Detector[EkState, Sample[Deg], Event[Deg]]:
        def init: EkState = EkState(Vector.empty, Vector.empty)

        def step(st: EkState, s: Sample[Deg]): (EkState, Vector[Event[Deg]]) =
          val window = (st.window :+ s).takeRight(5)
          if window.length < 5 then (EkState(window, st.run), Vector.empty)
          else
            val centre = window(2)
            val moving = Kinematics
              .velocities(window)
              .headOption
              .exists { case (_, v) =>
                val rx = v.dx / thresholds.etaX
                val ry = v.dy / thresholds.etaY
                rx * rx + ry * ry > 1.0
              }
            if moving then (EkState(window, st.run :+ centre), Vector.empty)
            else (EkState(window, Vector.empty), emit(st.run))

        def flush(st: EkState): Vector[Event[Deg]] = emit(st.run)

        private def emit(run: Vector[Sample[Deg]]): Vector[Event[Deg]] =
          if run.length < minSamples then Vector.empty
          else
            val ps = run.flatMap(_.gaze.position)
            if ps.length < 2 then Vector.empty
            else
              val tail = Span.micros(
                (run.last.t.toMicros - run.head.t.toMicros) / math.max(run.length - 1, 1)
              )
              Interval.of(clock, run.head.t, run.last.t + tail).toOption match
                case Some(iv) =>
                  val peak = Kinematics
                    .velocities(run)
                    .map(_._2.norm)
                    .maxOption
                    .flatMap(v => Velocity.perSecond[Deg](v).toOption)
                  Vector(Event.Saccade(iv, ps.head, ps.last, peak))
                case None => Vector.empty
    )

private final case class EkState(
    window: Vector[Sample[Deg]],
    run: Vector[Sample[Deg]]
)

/** Post-detection cleanup. */
object Merge:

  /** Fuse consecutive fixations separated by a short gap and a short distance.
    *
    * Detectors split a single fixation whenever a stray sample crosses the
    * threshold, and the fragments are usually a few milliseconds and a fraction
    * of a degree apart. Both criteria are required: a long pause in the same
    * place is two fixations, and so is a quick move to somewhere else.
    *
    * The merged fixation's centre is the duration-weighted mean of the parts,
    * because a fixation's position is where the eye spent its time, not the
    * midpoint of two arbitrary fragments.
    */
  def adjacentFixations[U <: Unit2D](
      maxGap: Span,
      maxSeparation: Distance[U]
  ): Machine[Event[U], Event[U]] =
    Machine(
      new Detector[Option[Event.Fixation[U]], Event[U], Event[U]]:
        def init: Option[Event.Fixation[U]] = None

        def step(
            held: Option[Event.Fixation[U]],
            e: Event[U]
        ): (Option[Event.Fixation[U]], Vector[Event[U]]) =
          (held, e) match
            case (Some(a), f: Event.Fixation[U]) if joinable(a, f) =>
              (Some(fuse(a, f)), Vector.empty)
            case (Some(a), f: Event.Fixation[U]) => (Some(f), Vector(a))
            case (Some(a), other)                => (None, Vector(a, other))
            case (None, f: Event.Fixation[U])    => (Some(f), Vector.empty)
            case (None, other)                   => (None, Vector(other))

        def flush(held: Option[Event.Fixation[U]]): Vector[Event[U]] = held.toVector

        private def joinable(a: Event.Fixation[U], b: Event.Fixation[U]): Boolean =
          val gap = a.span.offset.until(b.span.onset)
          gap.toMicros >= 0 && gap.toMicros <= maxGap.toMicros &&
          Distance.between(a.centre, b.centre) <= maxSeparation

        private def fuse(a: Event.Fixation[U], b: Event.Fixation[U]): Event.Fixation[U] =
          val wa = a.duration.toSeconds
          val wb = b.duration.toSeconds
          val w  = wa + wb
          Event.Fixation(
            Interval.of(a.span.clock, a.span.onset, b.span.offset).toOption.get,
            Pt[U](
              (a.centre.x * wa + b.centre.x * wb) / w,
              (a.centre.y * wa + b.centre.y * wb) / w
            ),
            math.max(a.dispersion, b.dispersion),
            a.sampleCount + b.sampleCount
          )
    )

end Merge
