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

/** Which observed positions may contribute to a centred spatial window. */
enum WindowObservationPolicy derives CanEqual:
  /** Every observation in the window must be tracked and on-surface. */
  case RequireTracked

  /** Off-surface positions may contribute; missing observations still preserve
    * the centre unchanged.
    */
  case IncludeOffScreen

/** Sample-level preprocessing, as composable [[Machine]]s.
  *
  * Each of these is a `Machine[Sample[U], Sample[U]]`, so they chain with each
  * other and with a detector using the same `andThen`, and the whole chain runs
  * offline or streaming without change.
  */
object Filter:

  /** Widen every run of missing data by `pad` on each side.
    *
    * The eyelid occludes the pupil progressively, so the samples immediately
    * before and after a blink are measured from a partly hidden pupil and drift
    * toward the lid. They are reported as tracked and they are wrong -- this is
    * the single most common source of spurious downward saccades in an
    * unfiltered record. Discarding a margin is standard, and the margin has to
    * be a parameter because it depends on the tracker.
    *
    * Requires buffering `pad` worth of samples, since a sample can only be
    * condemned by a blink that has not arrived yet.
    */
  def padMissing[U <: Unit2D](pad: MissingPadding): Machine[Sample[U], Sample[U]] =
    val padding = pad.span
    Machine(
      new Detector[PadState[U], Sample[U], Sample[U]]:
        def init: PadState[U] = PadState(Vector.empty, 0)

        def step(st: PadState[U], s: Sample[U]): (PadState[U], Vector[Sample[U]]) =
          val buf = st.buf :+ s
          val now = s.t.toMicros

          // A sample's fate is settled once nothing within `pad` of it can
          // still arrive.
          val settled = buf.indexWhere(_.t.toMicros > now - padding.toMicros) match
            case -1 => buf.length
            case i  => i

          val out = (st.emitted until settled).map(i => condemn(buf(i), buf)).toVector

          // Keep `pad` of look-BEHIND context beyond what has been emitted.
          // The first version dropped released samples immediately, which meant
          // a sample could be judged without seeing the blink just before it.
          val dropTo = buf.indexWhere(_.t.toMicros >= now - 2 * padding.toMicros) match
            case -1 => 0
            case i  => math.min(i, settled)

          (PadState(buf.drop(dropTo), settled - dropTo), out)

        def flush(st: PadState[U]): Vector[Sample[U]] =
          (st.emitted until st.buf.length).map(i => condemn(st.buf(i), st.buf)).toVector

        /** Condemned when any missing sample lies within `pad` either side. */
        private def condemn(s: Sample[U], context: Vector[Sample[U]]): Sample[U] =
          val nearMissing = context.exists { o =>
            o.gaze.isMissing && math.abs(o.t.toMicros - s.t.toMicros) <= padding.toMicros
          }
          if nearMissing && !s.gaze.isMissing then s.copy(gaze = Gaze.Lost[U]())
          else s
    )

  private final case class PadState[U <: Unit2D](buf: Vector[Sample[U]], emitted: Int)

  /** Linearly interpolate across runs of missing data shorter than `maxGap`.
    *
    * Long gaps are left missing. Interpolating across a two-second loss would
    * invent a trajectory, and the invented samples would be indistinguishable
    * from measured ones downstream -- which is why the limit is required rather
    * than defaulted.
    */
  def interpolateGaps[U <: Unit2D](maxGap: InterpolationGap): Machine[Sample[U], Sample[U]] =
    val gapLimit = maxGap.span
    Machine(
      new Detector[Vector[Sample[U]], Sample[U], Sample[U]]:
        def init: Vector[Sample[U]] = Vector.empty

        def step(
            buf: Vector[Sample[U]],
            s: Sample[U]
        ): (Vector[Sample[U]], Vector[Sample[U]]) =
          if s.gaze.isMissing then (buf :+ s, Vector.empty)
          else
            buf.headOption match
              // Retain this sample as the anchor for a gap that may follow.
              // The first version emitted it immediately and cleared the
              // buffer, so the next gap had no left-hand endpoint to
              // interpolate from and silently stayed missing.
              case None    => (Vector(s), Vector.empty)
              case Some(_) =>
                // buf holds: one usable anchor, then a run of missing samples.
                val anchor  = buf.head
                val missing = buf.tail
                val spanUs  = s.t.toMicros - anchor.t.toMicros
                val filled  =
                  (anchor.gaze.position, s.gaze.position) match
                    case (Some(a), Some(b)) if spanUs <= gapLimit.toMicros && spanUs > 0 =>
                      missing.map { m =>
                        val u = (m.t.toMicros - anchor.t.toMicros).toDouble / spanUs
                        Sample(
                          m.t,
                          Gaze.Tracked(
                            Pt[U](a.x + (b.x - a.x) * u, a.y + (b.y - a.y) * u),
                            None
                          ),
                          SampleLineage.interpolated
                        )
                      }
                    case _ => missing
                (Vector(s), (anchor +: filled))

        def flush(buf: Vector[Sample[U]]): Vector[Sample[U]] = buf
    )

  /** Replace each position with the median of a centred window.
    *
    * Robust to the isolated single-sample spikes video trackers emit, which a
    * mean would smear across the whole window instead of rejecting.
    * [[WindowObservationPolicy]] states whether an off-surface neighbour may
    * contribute. Missing observations always preserve the centre unchanged.
    */
  def median[U <: Unit2D](
      frame: Frame[U],
      halfWidth: WindowHalfWidth,
      policy: WindowObservationPolicy
  ): Machine[Sample[U], Sample[U]] =
    windowed(frame, halfWidth.value, policy, expectedPeriod = None) { ps =>
      val xs = ps.map(_.x).sorted
      val ys = ps.map(_.y).sorted
      Pt[U](xs(xs.length / 2), ys(ys.length / 2))
    }

  /** Savitzky-Golay smoothing of order 2.
    *
    * Fits a quadratic over a centred window and takes its value at the middle,
    * which preserves the height and width of a peak where a moving average
    * flattens it. That matters here: a saccade's velocity peak is the quantity
    * a main-sequence analysis depends on.
    *
    * The order is fixed at two. Supporting an arbitrary order means solving a
    * small least-squares per window size, and the quadratic case has a closed
    * form that can be written down and checked. An order parameter that
    * silently only worked for one value would be worse than none.
    *
    * The closed-form coefficients assume equal timestamp spacing, so callers
    * must supply [[RegularSampling]] parsed from the sequence. Each active
    * window rechecks that period defensively and passes through unchanged if a
    * proof is accidentally reused with different samples.
    */
  def savitzkyGolay[U <: Unit2D](
      frame: Frame[U],
      halfWidth: WindowHalfWidth,
      policy: WindowObservationPolicy,
      sampling: RegularSampling
  ): Machine[Sample[U], Sample[U]] =
    val m     = halfWidth.value
    val denom = (2 * m + 3).toDouble * (2 * m + 1) * (2 * m - 1)
    val coeff = Array.tabulate(2 * m + 1) { k =>
      val i = k - m
      3.0 * (3 * m * m + 3 * m - 1 - 5 * i * i) / denom
    }
    windowed(frame, halfWidth.value, policy, expectedPeriod = Some(sampling.period)) { ps =>
      var x = 0.0
      var y = 0.0
      var i = 0
      while i < ps.length do
        x += coeff(i) * ps(i).x
        y += coeff(i) * ps(i).y
        i += 1
      Pt[U](x, y)
    }

  /** Shared machinery: apply `f` to the positions of a centred window.
    *
    * A disallowed or missing observation passes the centre through untouched.
    * The first and final half-window pass through unchanged, so the filter
    * neither invents edge data nor silently shortens the record.
    */
  private def windowed[U <: Unit2D](
      frame: Frame[U],
      halfWidth: Int,
      policy: WindowObservationPolicy,
      expectedPeriod: Option[Span]
  )(f: Vector[Pt[U]] => Pt[U]): Machine[Sample[U], Sample[U]] =
    val width = 2 * halfWidth + 1
    Machine(
      new Detector[WindowState[U], Sample[U], Sample[U]]:
        def init: WindowState[U] = WindowState(Vector.empty, started = false)

        def step(
            state: WindowState[U],
            s: Sample[U]
        ): (WindowState[U], Vector[Sample[U]]) =
          val next = state.buffer :+ s
          if next.length < width then (state.copy(buffer = next), Vector.empty)
          else
            val leading = if state.started then Vector.empty else next.take(halfWidth)
            (WindowState(next.tail, started = true), leading :+ apply(next))

        def flush(state: WindowState[U]): Vector[Sample[U]] =
          // The trailing samples never see a full window; emit them unchanged
          // rather than dropping them or padding with invented values.
          if state.started then state.buffer.takeRight(halfWidth)
          else state.buffer

        private def apply(w: Vector[Sample[U]]): Sample[U] =
          val centre  = w(halfWidth)
          val regular = expectedPeriod.forall { expected =>
            w.indices.drop(1).forall(i => w(i - 1).t.until(w(i).t) == expected)
          }
          val eligible = regular && (policy match
            case WindowObservationPolicy.RequireTracked   => w.forall(_.gaze.isUsable)
            case WindowObservationPolicy.IncludeOffScreen =>
              w.forall(sample => !sample.gaze.isMissing))
          if !eligible || !centre.gaze.isUsable then centre
          else
            val smoothed = f(w.flatMap(_.gaze.position))
            val gaze     =
              if frame.contains(smoothed) then Gaze.Tracked(smoothed, centre.gaze.pupil)
              else Gaze.OffScreen(smoothed)
            centre.withSmoothedGaze(gaze)
    )

  private final case class WindowState[U <: Unit2D](
      buffer: Vector[Sample[U]],
      started: Boolean
  )

end Filter
