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

package eyes4s.core

import eyes4s.kernel.*

/** The physical arrangement of observer and display.
  *
  * A thin, named wrapper over the kernel's [[Perspective]]. The projection
  * itself is geometry -- how large a rectangle of known size appears from a
  * known distance -- and belongs in the kernel, where it can serve a camera or
  * a simulated observer as readily as an eye. This is the eye-tracking name for
  * it, with the constructors a bench setup actually uses.
  *
  * That split was forced by `checkKernelPurity` rejecting the name `Viewing` in
  * the kernel, and the rule turned out to be pointing at a real layering
  * question rather than merely being inconvenient (bead `k-warp`).
  */
final case class Viewing(perspective: Perspective) derives CanEqual:
  def distance: Length        = perspective.distance
  def horizontalExtent: Angle = perspective.horizontalExtent
  def verticalExtent: Angle   = perspective.verticalExtent
  def render: String          = s"viewing(${perspective.render})"

object Viewing:

  /** The usual bench description: how far away, and how big the screen is. */
  def of(
      distance: Length,
      screenWidth: Length,
      screenHeight: Length
  ): Either[GeometryError, Viewing] =
    Perspective.of(distance, screenWidth, screenHeight).map(Viewing.apply)

  /** The projection from a display frame to an angular one. */
  def angularWarp[A <: Unit2D, B <: Unit2D](
      v: Viewing,
      display: Frame[A],
      angular: Frame[B]
  ): Warp[A, B] = Warp.tangent(display, angular, v.perspective)

/** How often samples arrive. */
enum Rate derives CanEqual:
  case Fixed(hz: Hz)

  /** Variable-rate devices -- webcam trackers, some mobile units, anything
    * whose samples are timestamped on arrival. Representable rather than
    * excluded, because a large share of real data looks like this.
    */
  case Irregular

  def nominalPeriod: Option[Span] = this match
    case Fixed(hz) => Some(hz.period)
    case Irregular => None

/** A time series of gaze samples in one frame, on one timeline, for one eye.
  *
  * ==Invariants, proven at construction==
  *
  * Non-empty, and strictly increasing in time. The second matters more than it
  * looks: a duplicated or reordered timestamp makes every windowing operation
  * and every velocity estimate quietly wrong, and files with both do occur.
  */
final class Recording[U <: Unit2D] private (
    val frame: Frame[U],
    val clock: ClockId,
    val rate: Rate,
    val eye: Eye,
    val pupilUnit: Option[PupilUnit],
    val samples: IArray[Sample[U]]
):

  def size: Int = samples.length

  def first: Sample[U] = samples(0)
  def last: Sample[U]  = samples(size - 1)

  /** The extent covered, half-open past the final sample by one period so that
    * the last sample occupies time rather than being instantaneous.
    */
  def extent: Interval =
    val tail = rate.nominalPeriod.getOrElse(medianInterval)
    Interval.of(clock, first.t, last.t + tail).toOption.get

  def duration: Span = extent.duration

  /** Median inter-sample interval, which is what an irregular recording has
    * instead of a nominal period.
    */
  def medianInterval: Span =
    if size < 2 then Span.zero
    else
      val gaps = Array.tabulate(size - 1)(i => samples(i).t.until(samples(i + 1).t).toMicros)
      java.util.Arrays.sort(gaps)
      Span.micros(gaps(gaps.length / 2))

  /** Fraction of samples with usable gaze. The headline data-quality number,
    * and one `eyesim` cannot compute at all because it never sees a sample.
    */
  def trackedRatio: Double =
    if size == 0 then 0.0
    else
      var n = 0
      var i = 0
      while i < size do
        if samples(i).gaze.isUsable then n += 1
        i += 1
      n.toDouble / size

  /** Samples falling in a window.
    *
    * The policy argument is deliberately absent: a sample is instantaneous, so
    * there is nothing to straddle. [[Overlap]] applies to events, which have
    * extent. Accepting a policy here would imply a choice that does not exist.
    */
  def within(w: Window, anchor: Instant): Either[CoreError, Recording[U]] =
    for
      iv <- CoreError.widen(w.at(clock, anchor))
      r  <- CoreError.widenRecording(
        Recording.of(frame, clock, rate, eye, pupilUnit, samples.filter(s => iv.contains(s.t)))
      )
    yield r

  /** Move the whole recording to another frame.
    *
    * Every sample is warped, and positions that leave the target frame become
    * [[Gaze.OffScreen]] rather than being dropped -- the classification carries
    * the fact rather than the data disappearing.
    */
  def warp[V <: Unit2D](w: Warp[U, V]): Either[CoreError, Recording[V]] =
    for
      _ <- CoreError.widenGeometry(Agreement.frames(frame, w.from))
      r <- CoreError.widenRecording(
        Recording.of(
          w.to,
          clock,
          rate,
          eye,
          pupilUnit,
          samples.map(s => Sample(s.t, s.gaze.warp(w, w.to)))
        )
      )
    yield r

  /** The occupancy this recording induces: each usable sample weighted by the
    * time it represents.
    *
    * This is the forgetful map made concrete at the sample level. Order is
    * discarded; what remains is where the eye spent its time. Under a fixed
    * rate every sample carries the same period, so the weighting is uniform and
    * the measure is proportional to dwell.
    */
  def occupancy: Either[SurfaceError, PointMeasure[U]] =
    val period = rate.nominalPeriod.getOrElse(medianInterval).toSeconds
    val usable = (0 until size).filter(i => samples(i).gaze.isUsable)
    PointMeasure.of(
      frame,
      IArray.from(usable.map(i => samples(i).gaze.position.get)),
      IArray.from(usable.map(_ => period))
    )

  def render: String =
    f"recording($size samples, ${duration.toSeconds}%.2fs, $eye, ${frame.id}, " +
      f"tracked=${trackedRatio * 100}%.1f%%)"

end Recording

object Recording:

  def of[U <: Unit2D](
      frame: Frame[U],
      clock: ClockId,
      rate: Rate,
      eye: Eye,
      pupilUnit: Option[PupilUnit],
      samples: IArray[Sample[U]]
  ): Either[RecordingError, Recording[U]] =
    if samples.isEmpty then Left(RecordingError.NoSamples)
    else
      var bad = -1
      var i   = 1
      while i < samples.length && bad < 0 do
        if samples(i).t.toMicros <= samples(i - 1).t.toMicros then bad = i
        i += 1
      if bad >= 0 then
        Left(
          RecordingError.NonMonotonic(
            bad,
            samples(bad - 1).t.toMicros,
            samples(bad).t.toMicros
          )
        )
      else Right(new Recording(frame, clock, rate, eye, pupilUnit, samples))

end Recording

/** How two eyes are combined into one signal. */
enum Fusion derives CanEqual:
  /** The midpoint of the two positions. */
  case Mean

  /** Whichever eye is usable, preferring the left when both are. */
  case BestTracked

  case PreferLeft
  case PreferRight

/** Two eyes recorded together, with their samples paired.
  *
  * ==Why this is a separate type== (bead `q-binocular`)
  *
  * Making every [[Sample]] binocular would tax the monocular case, which is the
  * overwhelming majority of analysis. Keeping two independent `Recording`s
  * would be worse: once each eye has been filtered and resampled on its own,
  * they no longer share a sample index, and disparity becomes *unrecoverable*
  * rather than merely absent.
  *
  * So the pairing is preserved here, and analyses that need it -- vergence,
  * binocular coordination -- take this type explicitly. Detectors continue to
  * consume a plain `Recording`, obtained through one of the projections.
  *
  * The type ships before ingest does, deliberately: without it, a binocular
  * file would have to silently lose an eye at the parser.
  */
final class BinocularRecording[U <: Unit2D] private (
    val frame: Frame[U],
    val clock: ClockId,
    val rate: Rate,
    val pupilUnit: Option[PupilUnit],
    val timestamps: IArray[Instant],
    val leftGaze: IArray[Gaze[U]],
    val rightGaze: IArray[Gaze[U]]
):

  def size: Int = timestamps.length

  private def project(g: IArray[Gaze[U]], eye: Eye): Recording[U] =
    Recording
      .of(
        frame,
        clock,
        rate,
        eye,
        pupilUnit,
        IArray.tabulate(size)(i => Sample(timestamps(i), g(i)))
      )
      .toOption
      .get

  def left: Recording[U]  = project(leftGaze, Eye.Left)
  def right: Recording[U] = project(rightGaze, Eye.Right)

  /** One signal from two, by an explicit rule. */
  def cyclopean(f: Fusion): Recording[U] =
    val fused = IArray.tabulate(size) { i =>
      val l = leftGaze(i)
      val r = rightGaze(i)
      f match
        case Fusion.PreferLeft  => if l.isUsable then l else r
        case Fusion.PreferRight => if r.isUsable then r else l
        case Fusion.BestTracked =>
          if l.isUsable then l else if r.isUsable then r else Gaze.Lost[U]()
        case Fusion.Mean =>
          (l.position, r.position) match
            case (Some(a), Some(b)) if l.isUsable && r.isUsable =>
              Gaze.Tracked(
                a.midpoint(b),
                (l.pupil, r.pupil) match
                  case (Some(x), Some(y)) => Some((x + y) / 2.0)
                  case (Some(x), None)    => Some(x)
                  case (None, Some(y))    => Some(y)
                  case _                  => None
              )
            case _ => if l.isUsable then l else if r.isUsable then r else Gaze.Lost[U]()
    }
    project(fused, Eye.Cyclopean)

  /** Left position minus right, where both eyes are usable.
    *
    * On a recording in degrees this is the vergence signal. It is expressed as
    * a displacement rather than named "vergence" because the quantity is only
    * an angle when the frame is angular, and the type says which.
    */
  def disparity: Vector[(Instant, Vec2[U])] =
    (0 until size).view.flatMap { i =>
      (leftGaze(i), rightGaze(i)) match
        case (l, r) if l.isUsable && r.isUsable =>
          Some(timestamps(i) -> r.position.get.vectorTo(l.position.get))
        case _ => None
    }.toVector

  def render: String = s"binocular($size samples, ${frame.id})"

end BinocularRecording

object BinocularRecording:

  def of[U <: Unit2D](
      frame: Frame[U],
      clock: ClockId,
      rate: Rate,
      pupilUnit: Option[PupilUnit],
      timestamps: IArray[Instant],
      leftGaze: IArray[Gaze[U]],
      rightGaze: IArray[Gaze[U]]
  ): Either[RecordingError, BinocularRecording[U]] =
    if timestamps.isEmpty then Left(RecordingError.NoSamples)
    else if leftGaze.length != timestamps.length || rightGaze.length != timestamps.length then
      Left(RecordingError.UnpairedEyes(timestamps.length, leftGaze.length, rightGaze.length))
    else
      var bad = -1
      var i   = 1
      while i < timestamps.length && bad < 0 do
        if timestamps(i).toMicros <= timestamps(i - 1).toMicros then bad = i
        i += 1
      if bad >= 0 then
        Left(
          RecordingError
            .NonMonotonic(bad, timestamps(bad - 1).toMicros, timestamps(bad).toMicros)
        )
      else
        Right(
          new BinocularRecording(frame, clock, rate, pupilUnit, timestamps, leftGaze, rightGaze)
        )

end BinocularRecording

/** Failures constructing a recording. */
enum RecordingError derives CanEqual:
  case NoSamples
  case NonMonotonic(index: Int, previousMicros: Long, currentMicros: Long)
  case UnpairedEyes(timestamps: Int, left: Int, right: Int)

  def message: String = this match
    case NoSamples =>
      "A recording needs at least one sample."
    case NonMonotonic(i, prev, cur) =>
      s"Timestamps must strictly increase; sample $i is at ${cur}us after " +
        s"${prev}us. A duplicated or reordered timestamp makes every window " +
        "and every velocity estimate wrong without reporting anything."
    case UnpairedEyes(t, l, r) =>
      s"Binocular samples must be paired: $t timestamps, $l left, $r right."
