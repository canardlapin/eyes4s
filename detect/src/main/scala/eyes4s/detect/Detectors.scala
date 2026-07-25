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

/** Event detectors.
  *
  * Every one is a `Machine[Sample[U], Event[U]]`, so a detector chains onto
  * filters with the same `andThen` and runs offline or streaming unchanged.
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
    * implementation.
    */
  def ivt(
      threshold: Velocity[Deg],
      minDuration: Span,
      clock: ClockId
  ): Machine[Sample[Deg], Event[Deg]] =
    Machine(
      new Detector[IvtState[Deg], Sample[Deg], Event[Deg]]:
        def init: IvtState[Deg] = IvtState(Vector.empty, None)

        def step(
            st: IvtState[Deg],
            s: Sample[Deg]
        ): (IvtState[Deg], Vector[Event[Deg]]) =
          val held = st.pending :+ s
          if held.length < 3 then (IvtState(held, st.run), Vector.empty)
          else
            val Vector(a, b, c) = held.takeRight(3): @unchecked
            val classified      = classify(a, c)
            val (nextRun, out)  = absorb(st.run, b, classified)
            (IvtState(held.takeRight(2), nextRun), out)

        def flush(st: IvtState[Deg]): Vector[Event[Deg]] =
          // The final buffered samples never get a right-hand neighbour, so
          // they join whatever run is open rather than being dropped.
          val tail    = st.pending.drop(1)
          val withEnd = tail.foldLeft(st.run) { (r, s) =>
            absorb(r, s, r.map(_.moving).getOrElse(false))._1
          }
          withEnd.toVector.flatMap(emit)

        // Central difference uses only the outer samples; the middle one is the
        // sample being classified, not an input to its own velocity.
        private def classify(a: Sample[Deg], c: Sample[Deg]): Boolean =
          (a.gaze.position, c.gaze.position) match
            case (Some(p), Some(q)) =>
              val dt = a.t.until(c.t)
              Velocity.over(p.vectorTo(q), dt) match
                case Some(v) => v.value >= threshold.value
                case None    => false
            case _ => false

        private def absorb(
            run: Option[Run[Deg]],
            s: Sample[Deg],
            moving: Boolean
        ): (Option[Run[Deg]], Vector[Event[Deg]]) =
          run match
            case Some(r) if r.moving == moving =>
              (Some(r.copy(samples = r.samples :+ s)), Vector.empty)
            case Some(r) => (Some(Run(Vector(s), moving)), emit(r))
            case None    => (Some(Run(Vector(s), moving)), Vector.empty)

        private def emit(r: Run[Deg]): Vector[Event[Deg]] =
          val usable = r.samples.filter(_.gaze.isUsable)
          if usable.isEmpty then Vector.empty
          else
            val tail = estimateTail(r.samples)
            r.span(clock, tail) match
              case Some(iv) if iv.duration.toMicros >= minDuration.toMicros =>
                val ps = usable.flatMap(_.gaze.position)
                if r.moving then
                  Vector(Event.Saccade(iv, ps.head, ps.last, peakVelocity = None))
                else
                  val cx     = ps.map(_.x).sum / ps.length
                  val cy     = ps.map(_.y).sum / ps.length
                  val centre = Pt[Deg](cx, cy)
                  val disp   =
                    math.sqrt(ps.map(p => centre.distanceTo(p)).map(d => d * d).sum / ps.length)
                  Vector(Event.Fixation(iv, centre, disp, ps.length))
              case _ => Vector.empty

        private def estimateTail(ss: Vector[Sample[Deg]]): Span =
          if ss.length < 2 then Span.zero
          else Span.micros((ss.last.t.toMicros - ss.head.t.toMicros) / (ss.length - 1))
    )

  private final case class IvtState[U <: Unit2D](
      pending: Vector[Sample[U]],
      run: Option[Run[U]]
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
    * Unlike I-VT this works in any unit: a bounding box in pixels is a
    * well-defined claim about a particular display, which is sometimes exactly
    * what is wanted.
    */
  def idt[U <: Unit2D](
      extent: Extent[U],
      minDuration: Span,
      clock: ClockId
  ): Machine[Sample[U], Event[U]] =
    Machine(
      new Detector[Vector[Sample[U]], Sample[U], Event[U]]:
        def init: Vector[Sample[U]] = Vector.empty

        def step(
            window: Vector[Sample[U]],
            s: Sample[U]
        ): (Vector[Sample[U]], Vector[Event[U]]) =
          if !s.gaze.isUsable then (Vector.empty, close(window))
          else
            val grown = window :+ s
            if fits(grown) then (grown, Vector.empty)
            else (Vector(s), close(window))

        def flush(window: Vector[Sample[U]]): Vector[Event[U]] = close(window)

        private def fits(w: Vector[Sample[U]]): Boolean =
          val ps = w.flatMap(_.gaze.position)
          if ps.isEmpty then true
          else
            val xs = ps.map(_.x)
            val ys = ps.map(_.y)
            (xs.max - xs.min) <= extent.width && (ys.max - ys.min) <= extent.height

        private def close(w: Vector[Sample[U]]): Vector[Event[U]] =
          val ps = w.flatMap(_.gaze.position)
          if ps.isEmpty || w.length < 2 then Vector.empty
          else
            val tail = Span.micros((w.last.t.toMicros - w.head.t.toMicros) / (w.length - 1))
            Interval.of(clock, w.head.t, w.last.t + tail).toOption match
              case Some(iv) if iv.duration.toMicros >= minDuration.toMicros =>
                val centre = Pt[U](ps.map(_.x).sum / ps.length, ps.map(_.y).sum / ps.length)
                val disp   =
                  math.sqrt(ps.map(p => centre.distanceTo(p)).map(d => d * d).sum / ps.length)
                Vector(Event.Fixation(iv, centre, disp, ps.length))
              case _ => Vector.empty
    )

end Detectors
