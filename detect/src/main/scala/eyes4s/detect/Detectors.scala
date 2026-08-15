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

import scala.annotation.tailrec

/** A detector failure retained by offline and streaming execution. */
enum DetectionFailure derives CanEqual:
  case EventSummary(underlying: CoreError)
  case Kinematics(underlying: KinematicsError)

  def message: String = this match
    case EventSummary(underlying) => underlying.message
    case Kinematics(underlying)   => underlying.message

/** One detector emission: a coherent event or the exact named failure. */
type DetectionEmission[U <: Unit2D] = Either[DetectionFailure, Event[U]]

/** Event detectors.
  *
  * Every one is an [[EventDetector]] carrying a mandatory [[AlgorithmCard]]
  * beside its `Machine[Sample[U], DetectionEmission[U]]`. The underlying
  * machine chains onto filters and runs offline or streaming unchanged. An
  * incoherent event summary is emitted as a named error rather than thrown or
  * silently discarded.
  *
  * Named in the plural to keep it distinct from `eyes4s.kernel.Detector`, the
  * state-machine trait these are built from. The two would otherwise shadow
  * each other under a wildcard import, and a reader would have to know which
  * `Detector` a given file meant.
  */
object Detectors:

  /** Accumulated samples belonging to one candidate event. */
  private final case class Run[U <: Unit2D](
      samples: Vector[Sample[U]],
      moving: Boolean
  ):
    def span(clock: ClockId, tail: Span): Option[Interval] =
      samples.headOption.flatMap { h =>
        Interval.of(clock, h.t, samples.last.t + tail).toOption
      }

  private final case class IdtPoint[U <: Unit2D](sample: Sample[U], position: Pt[U])

  private final case class IdtBounds(
      xMin: Double,
      xMax: Double,
      yMin: Double,
      yMax: Double
  ):
    def include(other: IdtBounds): IdtBounds =
      IdtBounds(
        math.min(xMin, other.xMin),
        math.max(xMax, other.xMax),
        math.min(yMin, other.yMin),
        math.max(yMax, other.yMax)
      )

    def fits[U <: Unit2D](extent: Extent[U]): Boolean =
      xMax - xMin <= extent.width && yMax - yMin <= extent.height

    def diagonal: Double = math.hypot(xMax - xMin, yMax - yMin)

  private object IdtBounds:
    def point[U <: Unit2D](position: Pt[U]): IdtBounds =
      IdtBounds(position.x, position.x, position.y, position.y)

  private final case class IdtNode[U <: Unit2D](
      point: IdtPoint[U],
      aggregate: IdtBounds
  )

  /** Persistent two-stack queue whose stack aggregates make extrema lookup
    * constant-time. Each point enters and leaves each stack at most once, so a
    * long stationary candidate is linear rather than repeatedly rescanned.
    */
  private final case class IdtQueue[U <: Unit2D](
      front: List[IdtNode[U]],
      back: List[IdtNode[U]],
      size: Int
  ):
    def enqueue(point: IdtPoint[U]): IdtQueue[U] =
      copy(back = IdtQueue.push(point, back), size = size + 1)

    def bounds: Option[IdtBounds] =
      (front.headOption.map(_.aggregate), back.headOption.map(_.aggregate)) match
        case (Some(a), Some(b)) => Some(a.include(b))
        case (Some(a), None)    => Some(a)
        case (None, Some(b))    => Some(b)
        case (None, None)       => None

    def normalised: IdtQueue[U] =
      if front.nonEmpty || back.isEmpty then this
      else
        val transferred = back.foldLeft(List.empty[IdtNode[U]]) { (stack, node) =>
          IdtQueue.push(node.point, stack)
        }
        IdtQueue(transferred, Nil, size)

    def oldest: Option[IdtPoint[U]] = normalised.front.headOption.map(_.point)

    def dropOldest: IdtQueue[U] =
      val queue = normalised
      queue.front match
        case _ :: tail => queue.copy(front = tail, size = size - 1)
        case Nil       => queue

    def toVector: Vector[IdtPoint[U]] =
      front.iterator.map(_.point).toVector ++ back.reverseIterator.map(_.point)

  private object IdtQueue:
    def empty[U <: Unit2D]: IdtQueue[U] = IdtQueue(Nil, Nil, 0)

    def one[U <: Unit2D](point: IdtPoint[U]): IdtQueue[U] = empty.enqueue(point)

    def push[U <: Unit2D](point: IdtPoint[U], stack: List[IdtNode[U]]): List[IdtNode[U]] =
      val own       = IdtBounds.point(point.position)
      val aggregate = stack.headOption.fold(own)(node => own.include(node.aggregate))
      IdtNode(point, aggregate) :: stack

  private final case class IdtState[U <: Unit2D](
      candidate: IdtQueue[U],
      qualified: Boolean
  )

  /** Velocity-threshold identification (Salvucci & Goldberg, 2000).
    *
    * Samples slower than the threshold are fixation; faster are saccade.
    * Consecutive samples of the same class group into an event, and runs
    * shorter than `minDuration` are discarded as noise.
    *
    * ==Degrees, not pixels==
    *
    * The input is `Sample[Deg]`, not `Sample[U]`. A threshold of thirty degrees
    * per second is a claim about the oculomotor system; thirty pixels per
    * second is a different physical claim on every display. Because the
    * *input* is constrained rather than merely the threshold literal, a pixel
    * recording cannot reach this detector without an explicit warp -- and that
    * warp cannot be built without stating the viewing geometry. This is the
    * point at which the library's unit discipline stops being decorative.
    *
    * ==Velocity by central difference==
    *
    * Computed over the samples either side, which is symmetric and therefore
    * does not shift an event's onset the way a backward difference does. The
    * cost is one sample of latency, inherent to the method rather than to this
    * implementation. Classification requires a complete tracked triple;
    * blink, loss, and off-surface observations end the contiguous segment.
    * The valid endpoints of a classified segment inherit their adjacent
    * interior class so they remain explicit event support rather than silently
    * disappearing.
    */
  def ivt(
      threshold: IvtThreshold,
      minDuration: MinimumEventDuration,
      clock: ClockId
  ): EventDetector[Deg] =
    val velocityThreshold = threshold.velocity
    val durationMinimum   = minDuration.span
    val machine           = Machine(
      new Detector[IvtState[Deg], Sample[Deg], DetectionEmission[Deg]]:
        def init: IvtState[Deg] = IvtState(Vector.empty, None, atSegmentStart = true)

        def step(
            st: IvtState[Deg],
            s: Sample[Deg]
        ): (IvtState[Deg], Vector[DetectionEmission[Deg]]) =
          s.gaze match
            case Gaze.Tracked(_, _) =>
              val held = st.pending :+ s
              if held.length < 3 then (st.copy(pending = held), Vector.empty)
              else
                val a = held(held.length - 3)
                val b = held(held.length - 2)
                val c = held(held.length - 1)
                classify(a, b, c) match
                  case Some(moving) =>
                    val classified     = if st.atSegmentStart then Vector(a, b) else Vector(b)
                    val (nextRun, out) = classified.foldLeft(
                      st.run -> Vector.empty[DetectionEmission[Deg]]
                    ) { case ((run, emissions), sample) =>
                      val (updated, emitted) = absorb(run, sample, moving)
                      updated -> (emissions ++ emitted)
                    }
                    (IvtState(held.takeRight(2), nextRun, atSegmentStart = false), out)
                  case None =>
                    val out = finishSegment(st.copy(pending = held))
                    (init, out)
            case _ => (init, finishSegment(st))

        def flush(st: IvtState[Deg]): Vector[DetectionEmission[Deg]] =
          finishSegment(st)

        // All three observations must be tracked. The middle observation is
        // the sample being classified; the outer pair provides its symmetric
        // velocity estimate.
        private def classify(
            a: Sample[Deg],
            b: Sample[Deg],
            c: Sample[Deg]
        ): Option[Boolean] =
          (a.gaze, b.gaze, c.gaze) match
            case (Gaze.Tracked(p, _), Gaze.Tracked(_, _), Gaze.Tracked(q, _)) =>
              val dt = a.t.until(c.t)
              Velocity.over(p.vectorTo(q), dt).map(_.value >= velocityThreshold.value)
            case _ => None

        private def finishSegment(st: IvtState[Deg]): Vector[DetectionEmission[Deg]] =
          if st.atSegmentStart then st.run.toVector.flatMap(emit)
          else
            val withEndpoint = st.pending.lastOption.fold(st.run) { endpoint =>
              absorb(st.run, endpoint, st.run.exists(_.moving))._1
            }
            withEndpoint.toVector.flatMap(emit)

        private def absorb(
            run: Option[Run[Deg]],
            s: Sample[Deg],
            moving: Boolean
        ): (Option[Run[Deg]], Vector[DetectionEmission[Deg]]) =
          run match
            case Some(r) if r.moving == moving =>
              (Some(r.copy(samples = r.samples :+ s)), Vector.empty)
            case Some(r) => (Some(Run(Vector(s), moving)), emit(r))
            case None    => (Some(Run(Vector(s), moving)), Vector.empty)

        private def emit(r: Run[Deg]): Vector[DetectionEmission[Deg]] =
          val usable = r.samples.filter(_.gaze.isUsable)
          if usable.isEmpty then Vector.empty
          else
            val tail = estimateTail(r.samples)
            r.span(clock, tail) match
              case Some(iv) if iv.duration.toMicros >= durationMinimum.toMicros =>
                val ps = usable.flatMap(_.gaze.position)
                if r.moving then
                  Vector(
                    Event.Saccade
                      .of(iv, ps.head, ps.last, peakVelocity = None)
                      .left
                      .map(DetectionFailure.EventSummary.apply)
                      .map(event => event: Event[Deg])
                  )
                else
                  val cx     = ps.map(_.x).sum / ps.length
                  val cy     = ps.map(_.y).sum / ps.length
                  val centre = Pt[Deg](cx, cy)
                  val disp   =
                    math.sqrt(ps.map(p => centre.distanceTo(p)).map(d => d * d).sum / ps.length)
                  Vector(
                    Event.Fixation
                      .of(iv, centre, disp, DispersionMethod.RmsRadius, ps.length)
                      .left
                      .map(DetectionFailure.EventSummary.apply)
                      .map(event => event: Event[Deg])
                  )
              case _ => Vector.empty

        private def estimateTail(ss: Vector[Sample[Deg]]): Span =
          if ss.length < 2 then Span.zero
          else Span.micros((ss.last.t.toMicros - ss.head.t.toMicros) / (ss.length - 1))
    )
    new EventDetector(
      AlgorithmCards.ivt,
      machine,
      Vector(
        "velocityThresholdDegPerSecond" -> Provenance.Param.Num(velocityThreshold.value),
        "minimumEventDurationMicros"    -> Provenance.Param.Num(
          durationMinimum.toMicros.toDouble
        )
      )
    )

  private final case class IvtState[U <: Unit2D](
      pending: Vector[Sample[U]],
      run: Option[Run[U]],
      atSegmentStart: Boolean
  )

  /** Dispersion-threshold identification (Salvucci & Goldberg, 2000).
    *
    * A fixation is a maximal run of samples whose bounding box stays inside
    * `extent` for at least `minDuration`.
    *
    * ==An Extent, not a Sigma==
    *
    * Dispersion here is a bounding-box size, not a standard deviation. They
    * differ by a factor that depends on the sample distribution, so accepting a
    * [[Sigma]] would be a type pun that silently rescales every threshold a
    * user transfers from the literature.
    *
    * The detector advances the candidate start by one sample until a
    * minimum-duration window fits the per-axis extent, then expands that
    * fixation maximally. The sample that first violates a qualified window
    * begins the next candidate. Blink, lost, and off-surface observations break
    * a fixation and are never included.
    *
    * Unlike I-VT this works in any unit: a bounding box in pixels is a
    * well-defined claim about a particular display, which is sometimes exactly
    * what is wanted. The emitted summary records `BoundingBoxDiagonal`; the
    * acceptance threshold remains the named per-axis [[Extent]].
    */
  def idt[U <: Unit2D](
      extent: Extent[U],
      minDuration: MinimumEventDuration,
      clock: ClockId
  ): EventDetector[U] =
    val durationMinimum = minDuration.span
    val machine         = Machine(
      new Detector[IdtState[U], Sample[U], DetectionEmission[U]]:
        def init: IdtState[U] = IdtState(IdtQueue.empty, qualified = false)

        def step(
            state: IdtState[U],
            s: Sample[U]
        ): (IdtState[U], Vector[DetectionEmission[U]]) =
          s.gaze match
            case Gaze.Tracked(position, _) =>
              val point = IdtPoint(s, position)
              if state.qualified then
                val grown = state.candidate.enqueue(point)
                if grown.bounds.exists(_.fits(extent)) then
                  (IdtState(grown, qualified = true), Vector.empty)
                else (IdtState(IdtQueue.one(point), qualified = false), emit(state.candidate))
              else (advance(state.candidate.enqueue(point), s.t), Vector.empty)
            case _ =>
              val output = if state.qualified then emit(state.candidate) else Vector.empty
              (init, output)

        def flush(state: IdtState[U]): Vector[DetectionEmission[U]] =
          if state.qualified then emit(state.candidate) else Vector.empty

        @tailrec
        private def advance(candidate: IdtQueue[U], latest: Instant): IdtState[U] =
          val queue = candidate.normalised
          queue.oldest match
            case Some(oldest)
                if oldest.sample.t.until(latest).toMicros >= durationMinimum.toMicros =>
              if queue.bounds.exists(_.fits(extent)) then IdtState(queue, qualified = true)
              else advance(queue.dropOldest, latest)
            case _ => IdtState(queue, qualified = false)

        private def emit(candidate: IdtQueue[U]): Vector[DetectionEmission[U]] =
          val points = candidate.toVector
          if points.length < 2 then Vector.empty
          else
            val first  = points.head.sample.t
            val last   = points.last.sample.t
            val tail   = Span.micros(first.until(last).toMicros / (points.length - 1))
            val centre = Pt[U](
              points.map(_.position.x).sum / points.length,
              points.map(_.position.y).sum / points.length
            )
            val dispersion = candidate.bounds.fold(0.0)(_.diagonal)
            Interval.of(clock, first, last + tail).toOption match
              case Some(interval) =>
                Vector(
                  Event.Fixation
                    .of(
                      interval,
                      centre,
                      dispersion,
                      DispersionMethod.BoundingBoxDiagonal,
                      points.length
                    )
                    .left
                    .map(DetectionFailure.EventSummary.apply)
                    .map(event => event: Event[U])
                )
              case None => Vector.empty
    )
    new EventDetector(
      AlgorithmCards.idt,
      machine,
      Vector(
        "extentWidth"                -> Provenance.Param.Num(extent.width),
        "extentHeight"               -> Provenance.Param.Num(extent.height),
        "minimumEventDurationMicros" -> Provenance.Param.Num(
          durationMinimum.toMicros.toDouble
        )
      )
    )

end Detectors
