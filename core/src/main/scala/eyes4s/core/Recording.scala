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

  /** The usual bench description with all raw operands in millimetres. */
  def millimetres(
      distance: Double,
      screenWidth: Double,
      screenHeight: Double
  ): Either[GeometryError, Viewing] =
    Perspective.millimetres(distance, screenWidth, screenHeight).map(Viewing.apply)

  /** The projection from a display frame to an angular one. */
  def angularWarp(
      v: Viewing,
      display: Frame[Unit2D.Px],
      angular: Frame[Unit2D.Deg]
  ): Warp[Unit2D.Px, Unit2D.Deg] =
    Warp.tangent(display, angular, v.perspective)

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

/** Maximum absolute timestamp deviation admitted for a declared fixed rate. */
opaque type SamplingTolerance = Span

object SamplingTolerance:
  val Exact: SamplingTolerance                 = Span.zero
  val TimestampQuantisation: SamplingTolerance = Span.micros(1)

  def of(value: Span): Either[RecordingError, SamplingTolerance] =
    if value.isNegative then Left(RecordingError.NegativeSamplingTolerance(value))
    else Right(value)

  extension (tolerance: SamplingTolerance) def toSpan: Span = tolerance

/** Evidence retained after validating a recording's declared sampling model. */
sealed trait SamplingEvidence derives CanEqual:
  def render: String

object SamplingEvidence:
  final case class Fixed private[core] (
      rate: Hz,
      nominalPeriod: Span,
      tolerance: SamplingTolerance,
      maximumDeviation: Span
  ) extends SamplingEvidence:
    def render: String =
      s"fixed(rate=${rate.value}Hz, nominal=${nominalPeriod.render}, " +
        s"tolerance=${tolerance.toSpan.render}, maxDeviation=${maximumDeviation.render})"

  case object Irregular extends SamplingEvidence:
    def render: String = "irregular-timestamps"

/** Which recording channel supplied a contextual validation failure. */
enum RecordingChannel derives CanEqual:
  case Monocular(eye: Eye)
  case LeftEye
  case RightEye

  def render: String = this match
    case Monocular(eye) => s"$eye monocular channel"
    case LeftEye        => "left-eye channel"
    case RightEye       => "right-eye channel"

/** Positional gaze cases named in contextual recording errors. */
enum PositionalGazeState derives CanEqual:
  case Tracked
  case OffScreen

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
    val samplingTolerance: SamplingTolerance,
    val samplingEvidence: SamplingEvidence,
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

  /** Deterministic content identity for derived-artifact provenance. */
  def contentHash: ContentHash =
    val samplingHash = samplingEvidence match
      case fixed: SamplingEvidence.Fixed =>
        ContentHash.combineAll(
          Seq(
            ContentHash.ofString("sampling:fixed"),
            ContentHash.of(IArray(fixed.rate.value)),
            ContentHash.ofString(fixed.nominalPeriod.toMicros.toString),
            ContentHash.ofString(fixed.tolerance.toSpan.toMicros.toString),
            ContentHash.ofString(fixed.maximumDeviation.toMicros.toString)
          )
        )
      case SamplingEvidence.Irregular => ContentHash.ofString("sampling:irregular")
    val frameSpec = frame.spec
    val yAxisHash = frameSpec.yAxis match
      case YAxis.Down => ContentHash.ofString("y-axis:down")
      case YAxis.Up   => ContentHash.ofString("y-axis:up")
    val eyeHash = eye match
      case Eye.Left      => ContentHash.ofString("eye:left")
      case Eye.Right     => ContentHash.ofString("eye:right")
      case Eye.Cyclopean => ContentHash.ofString("eye:cyclopean")
    val pupilUnitHash = pupilUnit match
      case None                      => ContentHash.ofString("pupil-unit:none")
      case Some(PupilUnit.Area)      => ContentHash.ofString("pupil-unit:area")
      case Some(PupilUnit.Diameter)  => ContentHash.ofString("pupil-unit:diameter")
      case Some(PupilUnit.Arbitrary) =>
        ContentHash.ofString("pupil-unit:arbitrary")
    val metadata = ContentHash.combineAll(
      Seq(
        ContentHash.ofString("recording:v2"),
        ContentHash.ofString(frame.id.name),
        ContentHash.of(IArray(frameSpec.xMin, frameSpec.yMin, frameSpec.xMax, frameSpec.yMax)),
        yAxisHash,
        ContentHash.ofString(clock.name),
        eyeHash,
        pupilUnitHash,
        samplingHash
      )
    )
    val sampleHashes = (0 until size).map { index =>
      val sample = samples(index)
      val state  = sample.gaze match
        case Gaze.Tracked(point, pupil) =>
          val pupilHash = pupil match
            case None        => ContentHash.ofString("pupil:none")
            case Some(value) =>
              ContentHash.combine(
                ContentHash.ofString("pupil:some"),
                ContentHash.of(IArray(value))
              )
          ContentHash.combineAll(
            Seq(
              ContentHash.ofString("tracked"),
              ContentHash.of(IArray(point.x, point.y)),
              pupilHash
            )
          )
        case Gaze.OffScreen(point) =>
          ContentHash.combine(
            ContentHash.ofString("off-screen"),
            ContentHash.of(IArray(point.x, point.y))
          )
        case Gaze.Blink() => ContentHash.ofString("blink")
        case Gaze.Lost()  => ContentHash.ofString("lost")
      ContentHash.combineAll(
        Seq(
          ContentHash.ofString(sample.t.toMicros.toString),
          state,
          ContentHash.ofString(sample.lineage.render)
        )
      )
    }
    ContentHash.combineAll(metadata +: sampleHashes)

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
        Recording.of(
          frame,
          clock,
          rate,
          eye,
          pupilUnit,
          samples.filter(s => iv.contains(s.t)),
          samplingTolerance
        )
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
          samples.map(s => s.withProjectedGaze(s.gaze.warp(w))),
          samplingTolerance
        )
      )
    yield r

  /** The occupancy this recording induces under its rate's named default
    * temporal-support policy.
    *
    * This is the forgetful map made concrete at the sample level. Order is
    * discarded; what remains is where the eye spent its time. Under a fixed
    * Fixed-rate recordings use their nominal period. Irregular recordings use
    * Voronoi support with no gap cap and an explicitly named median trailing
    * edge. The returned result records that choice and all censored support.
    */
  def occupancy: Either[OccupancyError, OccupancyResult[U]] =
    occupancy(TemporalSupport.default(rate))

  /** Per-sample support under the same default used by [[occupancy]]. */
  def representedSupport: SampleSupportLedger =
    representedSupport(TemporalSupport.default(rate))

  /** Per-sample support shared by occupancy and downstream temporal measures. */
  def representedSupport(policy: TemporalSupport): SampleSupportLedger =
    val assigned       = Array.fill[Long](size)(0L)
    var policyCensored = 0L

    policy match
      case fixed: TemporalSupport.Fixed =>
        java.util.Arrays.fill(assigned, fixed.period.toMicros)
      case TemporalSupport.Voronoi(maxGap, edge) =>
        var index = 0
        while index + 1 < size do
          val raw      = samples(index).t.until(samples(index + 1).t)
          val retained = maxGap.retain(raw)
          val left     = retained.toMicros / 2L
          assigned(index) += left
          assigned(index + 1) += retained.toMicros - left
          policyCensored += raw.toMicros - retained.toMicros
          index += 1
        val rawEdge      = edgeDuration(edge)
        val retainedEdge = maxGap.retain(rawEdge)
        assigned(size - 1) += retainedEdge.toMicros
        policyCensored += rawEdge.toMicros - retainedEdge.toMicros
      case TemporalSupport.ForwardHold(maxGap, edge) =>
        var index = 0
        while index + 1 < size do
          val raw      = samples(index).t.until(samples(index + 1).t)
          val retained = maxGap.retain(raw)
          assigned(index) += retained.toMicros
          policyCensored += raw.toMicros - retained.toMicros
          index += 1
        val rawEdge      = edgeDuration(edge)
        val retainedEdge = maxGap.retain(rawEdge)
        assigned(size - 1) += retainedEdge.toMicros
        policyCensored += rawEdge.toMicros - retainedEdge.toMicros

    new SampleSupportLedger(
      policy,
      IArray.from(assigned.map(Span.micros)),
      Span.micros(policyCensored)
    )

  /** The occupancy this recording induces under an explicit support policy. */
  def occupancy(policy: TemporalSupport): Either[OccupancyError, OccupancyResult[U]] =
    val support  = representedSupport(policy)
    val assigned = support.toVector

    val usable           = (0 until size).filter(index => samples(index).isUsable)
    val analysableMicros = usable.foldLeft(0L) { (total, index) =>
      total + assigned(index).toMicros
    }
    val excludedMicros = (0 until size).iterator
      .filterNot(index => samples(index).isUsable)
      .foldLeft(0L)((total, index) => total + assigned(index).toMicros)
    PointMeasure
      .of(
        frame,
        IArray.from(usable.map(index => samples(index).position.get)),
        IArray.from(usable.map(index => assigned(index).toSeconds))
      )
      .left
      .map(OccupancyError.Measure(policy, _))
      .map { measure =>
        OccupancyResult.validated(
          measure,
          Span.micros(analysableMicros),
          support.censoredTime + Span.micros(excludedMicros),
          size - usable.length,
          policy
        )
      }

  private def edgeDuration(edge: EdgeSupport): Span = edge match
    case EdgeSupport.Censored         => Span.zero
    case EdgeSupport.PreviousInterval =>
      if size < 2 then Span.zero else samples(size - 2).t.until(samples(size - 1).t)
    case EdgeSupport.MedianInterval => medianInterval
    case fixed: EdgeSupport.Fixed   => fixed.span

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
      samples: IArray[Sample[U]],
      samplingTolerance: SamplingTolerance = SamplingTolerance.TimestampQuantisation
  ): Either[RecordingError, Recording[U]] =
    for
      _ <- validateTimestamps(samples.length, index => samples(index).t)
      _ <- validateGazes(
        frame,
        pupilUnit,
        RecordingChannel.Monocular(eye),
        samples.length,
        index => samples(index).gaze
      )
      evidence <- validateSampling(
        rate,
        samplingTolerance,
        samples.length,
        index => samples(index).t
      )
    yield fromValidated(
      frame,
      clock,
      rate,
      eye,
      pupilUnit,
      samplingTolerance,
      evidence,
      samples
    )

  private[core] def fromValidated[U <: Unit2D](
      frame: Frame[U],
      clock: ClockId,
      rate: Rate,
      eye: Eye,
      pupilUnit: Option[PupilUnit],
      samplingTolerance: SamplingTolerance,
      samplingEvidence: SamplingEvidence,
      samples: IArray[Sample[U]]
  ): Recording[U] =
    new Recording(
      frame,
      clock,
      rate,
      eye,
      pupilUnit,
      samplingTolerance,
      samplingEvidence,
      samples
    )

  private[core] def validateTimestamps(
      size: Int,
      timestampAt: Int => Instant
  ): Either[RecordingError, Unit] =
    if size == 0 then Left(RecordingError.NoSamples)
    else
      var bad = -1
      var i   = 1
      while i < size && bad < 0 do
        if timestampAt(i).toMicros <= timestampAt(i - 1).toMicros then bad = i
        i += 1
      if bad >= 0 then
        Left(
          RecordingError.NonMonotonic(
            bad,
            timestampAt(bad - 1).toMicros,
            timestampAt(bad).toMicros
          )
        )
      else Right(())

  private[core] def validateGazes[U <: Unit2D](
      frame: Frame[U],
      pupilUnit: Option[PupilUnit],
      channel: RecordingChannel,
      size: Int,
      gazeAt: Int => Gaze[U]
  ): Either[RecordingError, Unit] =
    var failure = Option.empty[RecordingError]
    var index   = 0
    while index < size && failure.isEmpty do
      failure = validateGaze(frame, pupilUnit, channel, index, gazeAt(index))
      index += 1
    failure.toLeft(())

  private def validateGaze[U <: Unit2D](
      frame: Frame[U],
      pupilUnit: Option[PupilUnit],
      channel: RecordingChannel,
      index: Int,
      gaze: Gaze[U]
  ): Option[RecordingError] =
    gaze match
      case Gaze.Tracked(point, pupil) =>
        if !point.x.isFinite || !point.y.isFinite then
          Some(
            RecordingError.NonFinitePosition(
              channel,
              index,
              PositionalGazeState.Tracked,
              point.x,
              point.y
            )
          )
        else if !frame.contains(point) then
          Some(
            RecordingError.TrackedOutsideFrame(
              channel,
              index,
              frame.id,
              frame.spec,
              point.x,
              point.y
            )
          )
        else
          pupil match
            case Some(value) if !value.isFinite || value <= 0.0 =>
              Some(RecordingError.InvalidPupil(channel, index, value))
            case Some(value) if pupilUnit.isEmpty =>
              Some(RecordingError.UndeclaredPupilUnit(channel, index, value))
            case _ => None
      case Gaze.OffScreen(point) =>
        if !point.x.isFinite || !point.y.isFinite then
          Some(
            RecordingError.NonFinitePosition(
              channel,
              index,
              PositionalGazeState.OffScreen,
              point.x,
              point.y
            )
          )
        else if frame.contains(point) then
          Some(
            RecordingError.OffScreenInsideFrame(
              channel,
              index,
              frame.id,
              frame.spec,
              point.x,
              point.y
            )
          )
        else None
      case Gaze.Blink() | Gaze.Lost() => None

  private[core] def validateSampling(
      rate: Rate,
      tolerance: SamplingTolerance,
      size: Int,
      timestampAt: Int => Instant
  ): Either[RecordingError, SamplingEvidence] =
    rate match
      case Rate.Irregular => Right(SamplingEvidence.Irregular)
      case Rate.Fixed(hz) =>
        val nominal  = hz.period
        var maximum  = 0L
        var mismatch = Option.empty[(Int, Long, Long, Long)]
        var index    = 1
        while index < size && mismatch.isEmpty do
          val previous  = timestampAt(index - 1).toMicros
          val current   = timestampAt(index).toMicros
          val observed  = current - previous
          val deviation = math.abs(observed - nominal.toMicros)
          if deviation > maximum then maximum = deviation
          if deviation > tolerance.toSpan.toMicros then
            mismatch = Some((index, previous, current, deviation))
          index += 1
        mismatch match
          case Some((badIndex, previous, current, deviation)) =>
            Left(
              RecordingError.FixedRateMismatch(
                badIndex,
                previous,
                current,
                nominal,
                tolerance,
                Span.micros(deviation)
              )
            )
          case None =>
            Right(
              SamplingEvidence.Fixed(
                hz,
                nominal,
                tolerance,
                Span.micros(maximum)
              )
            )

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
    val samplingTolerance: SamplingTolerance,
    val samplingEvidence: SamplingEvidence,
    val timestamps: IArray[Instant],
    val leftGaze: IArray[Gaze[U]],
    val rightGaze: IArray[Gaze[U]]
):

  def size: Int = timestamps.length

  private def project(g: IArray[Gaze[U]], eye: Eye): Recording[U] =
    Recording.fromValidated(
      frame,
      clock,
      rate,
      eye,
      pupilUnit,
      samplingTolerance,
      samplingEvidence,
      IArray.tabulate(size)(i => Sample(timestamps(i), g(i)))
    )

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
      rightGaze: IArray[Gaze[U]],
      samplingTolerance: SamplingTolerance = SamplingTolerance.TimestampQuantisation
  ): Either[RecordingError, BinocularRecording[U]] =
    if timestamps.isEmpty then Left(RecordingError.NoSamples)
    else if leftGaze.length != timestamps.length || rightGaze.length != timestamps.length then
      Left(RecordingError.UnpairedEyes(timestamps.length, leftGaze.length, rightGaze.length))
    else
      for
        _ <- Recording.validateTimestamps(timestamps.length, index => timestamps(index))
        _ <- Recording.validateGazes(
          frame,
          pupilUnit,
          RecordingChannel.LeftEye,
          leftGaze.length,
          index => leftGaze(index)
        )
        _ <- Recording.validateGazes(
          frame,
          pupilUnit,
          RecordingChannel.RightEye,
          rightGaze.length,
          index => rightGaze(index)
        )
        evidence <- Recording.validateSampling(
          rate,
          samplingTolerance,
          timestamps.length,
          index => timestamps(index)
        )
      yield new BinocularRecording(
        frame,
        clock,
        rate,
        pupilUnit,
        samplingTolerance,
        evidence,
        timestamps,
        leftGaze,
        rightGaze
      )

end BinocularRecording

/** Failures constructing a recording. */
enum RecordingError derives CanEqual:
  case NoSamples
  case NonMonotonic(index: Int, previousMicros: Long, currentMicros: Long)
  case UnpairedEyes(timestamps: Int, left: Int, right: Int)
  case NonFinitePosition(
      channel: RecordingChannel,
      index: Int,
      state: PositionalGazeState,
      x: Double,
      y: Double
  )
  case TrackedOutsideFrame(
      channel: RecordingChannel,
      index: Int,
      frameId: FrameId,
      frameSpec: FrameSpec,
      x: Double,
      y: Double
  )
  case OffScreenInsideFrame(
      channel: RecordingChannel,
      index: Int,
      frameId: FrameId,
      frameSpec: FrameSpec,
      x: Double,
      y: Double
  )
  case InvalidPupil(channel: RecordingChannel, index: Int, value: Double)
  case UndeclaredPupilUnit(channel: RecordingChannel, index: Int, value: Double)
  case NegativeSamplingTolerance(tolerance: Span)
  case FixedRateMismatch(
      index: Int,
      previousMicros: Long,
      currentMicros: Long,
      nominalPeriod: Span,
      tolerance: SamplingTolerance,
      deviation: Span
  )

  def message: String = this match
    case NoSamples =>
      "A recording needs at least one sample."
    case NonMonotonic(i, prev, cur) =>
      s"Timestamps must strictly increase; sample $i is at ${cur}us after " +
        s"${prev}us. A duplicated or reordered timestamp makes every window " +
        "and every velocity estimate wrong without reporting anything."
    case UnpairedEyes(t, l, r) =>
      s"Binocular samples must be paired: $t timestamps, $l left, $r right."
    case NonFinitePosition(channel, index, state, x, y) =>
      s"${channel.render} sample[$index] state=$state needs a finite position, got x=$x, y=$y."
    case TrackedOutsideFrame(channel, index, frameId, frameSpec, x, y) =>
      s"${channel.render} sample[$index] is Tracked at ($x, $y), outside " +
        s"frame id=$frameId spec=${frameSpec.render}; classify it OffScreen explicitly."
    case OffScreenInsideFrame(channel, index, frameId, frameSpec, x, y) =>
      s"${channel.render} sample[$index] is OffScreen at ($x, $y), but that point is inside " +
        s"frame id=$frameId spec=${frameSpec.render}; classify it Tracked explicitly."
    case InvalidPupil(channel, index, value) =>
      s"${channel.render} sample[$index] needs a finite positive pupil value, got value=$value."
    case UndeclaredPupilUnit(channel, index, value) =>
      s"${channel.render} sample[$index] has pupil value=$value but the recording pupilUnit is None."
    case NegativeSamplingTolerance(tolerance) =>
      s"Fixed-rate sampling tolerance must be non-negative, got tolerance=${tolerance.render}."
    case FixedRateMismatch(
          index,
          previousMicros,
          currentMicros,
          nominalPeriod,
          tolerance,
          deviation
        ) =>
      s"Fixed-rate sample[$index] at ${currentMicros}us follows ${previousMicros}us with " +
        s"nominalPeriod=${nominalPeriod.render}, tolerance=${tolerance.toSpan.render}, and " +
        s"absoluteDeviation=${deviation.render}; declare irregular sampling or resample explicitly."
