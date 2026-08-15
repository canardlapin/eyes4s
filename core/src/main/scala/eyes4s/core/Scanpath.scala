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

/** How much each fixation counts toward an occupancy measure. */
enum Weight derives CanEqual:
  /** Every fixation counts once. Fixation-count maps. */
  case Uniform

  /** Each fixation counts for as long as it lasted. Dwell maps, and the
    * default for anything claiming to show where attention went.
    */
  case Duration

/** The geometric transition between two consecutive fixations.
  *
  * This is deliberately not an [[Event.Saccade]]. A segmented recording may
  * contain abutting fixations, in which case the intervening transition has no
  * measured temporal extent and cannot honestly inhabit the positive-duration
  * saccade event type.
  */
final class ScanpathTransition[U <: Unit2D] private[core] (
    val span: Interval,
    val from: Pt[U],
    val to: Pt[U]
) derives CanEqual:
  def duration: Span                    = span.duration
  def displacement: Vec2[U]             = from.vectorTo(to)
  def amplitude: Double                 = displacement.norm
  def direction: Angle                  = displacement.angle
  def meanVelocity: Option[Velocity[U]] = Velocity.over(displacement, duration)

/** An ordered sequence of fixations in one frame, on one timeline.
  *
  * ==n fixations, n-1 transitions==
  *
  * Stated in the types rather than papered over. `eyesim` appends saccade
  * columns to its fixation table and pads the final row with zeros, so every
  * consumer must remember to drop it -- `multi_match` does exactly that, with
  * `x[1:(nrow(x)-1),]`, and a consumer that forgets inherits a fabricated
  * zero-length saccade at the end of every path. Here [[transitions]] simply
  * returns one fewer element than there are fixations, without falsely
  * claiming that an abutting boundary is a measured saccade event.
  *
  * ==The trajectory side of the duality==
  *
  * A scanpath keeps its order, so it supports the measures a [[PointMeasure]]
  * cannot: path length, transition structure, alignment, recurrence. Going the
  * other way is [[occupancy]], which is explicitly lossy.
  */
final class Scanpath[U <: Unit2D] private (
    val frame: Frame[U],
    val clock: ClockId,
    val fixations: IArray[Event.Fixation[U]],
    val transitions: Vector[ScanpathTransition[U]],
    val extent: Interval,
    private val sourceSeries: Option[EventSeries[U]]
):

  def n: Int = fixations.length

  def first: Event.Fixation[U] = fixations(0)
  def last: Event.Fixation[U]  = fixations(n - 1)

  /** Source identity and exact half-open sample ranges, when this scanpath was
    * constructed from a detection result rather than detached event values.
    */
  def source: Option[RecordingRef]               = sourceSeries.map(_.source)
  def sampleSupport: Option[Vector[SampleRange]] = sourceSeries.map(_.support)

  /** Total distance travelled, in frame units.
    *
    * Order-dependent, and therefore unavailable from [[occupancy]] -- the
    * clearest single illustration of why the two representations are separate
    * types rather than two views of one.
    */
  def pathLength: Double =
    var d = 0.0
    var i = 0
    while i < n - 1 do
      d += fixations(i).centre.distanceTo(fixations(i + 1).centre)
      i += 1
    d

  /** Total time spent fixating. Not the same as the extent, which includes the
    * transitions between them.
    */
  def dwellTotal: Span =
    var acc = Span.zero
    var i   = 0
    while i < n do
      acc = acc + fixations(i).duration
      i += 1
    acc

  /** Fixations selected by a window, under an explicit straddling policy. */
  def within(
      w: Window,
      anchor: Instant,
      policy: Overlap
  ): Either[CoreError, Scanpath[U]] =
    for
      iv          <- CoreError.widen(w.at(clock, anchor))
      keptIndices <- CoreError.widen(
        fixations.indices.foldLeft[Either[TimeError, Vector[Int]]](
          Right(Vector.empty)
        ) { (acc, index) =>
          for
            ks  <- acc
            hit <- policy.selects(fixations(index).span, iv)
          yield if hit then ks :+ index else ks
        }
      )
      sp <- sourceSeries match
        case None =>
          CoreError.widenScanpath(
            Scanpath.of(frame, clock, IArray.from(keptIndices.map(fixations(_))))
          )
        case Some(series) =>
          for
            selected <- CoreError.widenDetectionSupport(
              EventSeries.of(
                series.recording,
                series.source,
                keptIndices.map(index => series.events(index)),
                keptIndices.map(index => series.support(index))
              )
            )
            path <- Scanpath.fromEvents(selected)
          yield path
    yield sp

  def warp[V <: Unit2D](w: Warp[U, V]): Either[CoreError, Scanpath[V]] =
    sourceSeries match
      case Some(series) =>
        for
          moved  <- series.warp(w)
          result <- Scanpath.fromEvents(moved)
        yield result
      case None =>
        for
          _     <- CoreError.widenGeometry(Agreement.frames(frame, w.from))
          moved <- CoreError.widenScanpath(
            fixations.indices.foldLeft[
              Either[ScanpathError, Vector[Event.Fixation[V]]]
            ](Right(Vector.empty)) { (acc, index) =>
              val fixation = fixations(index)
              for
                built  <- acc
                centre <- w(fixation.centre).toRight(
                  ScanpathError.UnmappableFixation(
                    index,
                    w.from.id,
                    w.to.id,
                    fixation.centre.x,
                    fixation.centre.y
                  )
                )
              yield built :+ Event.Fixation.transformed(
                fixation,
                centre,
                w.from.id,
                w.to.id
              )
            }
          )
          result <- CoreError.widenScanpath(
            Scanpath.of(
              w.to,
              clock,
              IArray.from(moved)
            )
          )
        yield result

  /** The measure this path induces, forgetting the order.
    *
    * Under [[Weight.Duration]] the mass at each position is the time spent
    * there, so the result is a dwell map.
    */
  def occupancy(weight: Weight = Weight.Duration): Either[SurfaceError, PointMeasure[U]] =
    PointMeasure.of(
      frame,
      IArray.tabulate(n)(i => fixations(i).centre),
      IArray.tabulate(n) { i =>
        weight match
          case Weight.Uniform  => 1.0
          case Weight.Duration => fixations(i).duration.toSeconds
      }
    )

  def render: String = s"scanpath($n fixations, ${frame.id})"

end Scanpath

object Scanpath:

  /** Fixations must be non-empty and ordered, and must not overlap in time.
    *
    * Overlap is rejected rather than tolerated because two fixations occupying
    * the same instant make the sequence ambiguous: the transition between them
    * has negative duration, and any window selection depends on iteration order.
    */
  def of[U <: Unit2D](
      frame: Frame[U],
      clock: ClockId,
      fixations: IArray[Event.Fixation[U]]
  ): Either[ScanpathError, Scanpath[U]] =
    if fixations.isEmpty then Left(ScanpathError.NoFixations)
    else
      val wrongClock = (0 until fixations.length).find(i => fixations(i).span.clock != clock)
      wrongClock match
        case Some(i) =>
          Left(ScanpathError.WrongClock(i, clock.name, fixations(i).span.clock.name))
        case None =>
          val bad = (1 until fixations.length).find { i =>
            fixations(i).span.onset.toMicros < fixations(i - 1).span.offset.toMicros
          }
          bad match
            case Some(i) =>
              Left(
                ScanpathError.OutOfOrder(
                  i,
                  fixations(i - 1).span.render,
                  fixations(i).span.render
                )
              )
            case None =>
              for
                transitions <- buildTransitions(clock, fixations)
                extent      <- buildExtent(clock, fixations)
              yield new Scanpath(frame, clock, fixations, transitions, extent, None)

  /** Build from a detector's output, keeping only the fixations. */
  def fromEvents[U <: Unit2D](
      frame: Frame[U],
      clock: ClockId,
      events: Vector[Event[U]]
  ): Either[ScanpathError, Scanpath[U]] =
    of(
      frame,
      clock,
      IArray.from(events.collect { case f: Event.Fixation[U] => f })
    )

  /** Build from a source-supported event series, preserving the exact samples
    * needed to reconstruct fixation summaries after a general warp.
    */
  def fromEvents[U <: Unit2D](series: EventSeries[U]): Either[CoreError, Scanpath[U]] =
    val paired = series.events.zip(series.support).collect {
      case (fixation: Event.Fixation[U], range) => (fixation, range)
    }
    for
      path <- CoreError.widenScanpath(
        of(series.frame, series.clock, IArray.from(paired.map(_._1)))
      )
      fixationSeries <- CoreError.widenDetectionSupport(
        EventSeries
          .of(
            series.recording,
            series.source,
            paired.map(pair => pair._1: Event[U]),
            paired.map(_._2)
          )
      )
    yield new Scanpath(
      path.frame,
      path.clock,
      path.fixations,
      path.transitions,
      path.extent,
      Some(fixationSeries)
    )

  private def buildTransitions[U <: Unit2D](
      clock: ClockId,
      fixations: IArray[Event.Fixation[U]]
  ): Either[ScanpathError, Vector[ScanpathTransition[U]]] =
    (0 until fixations.length - 1).foldLeft[
      Either[ScanpathError, Vector[ScanpathTransition[U]]]
    ](Right(Vector.empty)) { (acc, index) =>
      val from = fixations(index)
      val to   = fixations(index + 1)
      for
        built <- acc
        span  <- Interval
          .of(clock, from.span.offset, to.span.onset)
          .left
          .map(ScanpathError.InvalidTransitionSpan(index, _))
      yield built :+ new ScanpathTransition(span, from.centre, to.centre)
    }

  private def buildExtent[U <: Unit2D](
      clock: ClockId,
      fixations: IArray[Event.Fixation[U]]
  ): Either[ScanpathError, Interval] =
    Interval
      .of(clock, fixations(0).span.onset, fixations(fixations.length - 1).span.offset)
      .left
      .map(ScanpathError.InvalidExtent.apply)

end Scanpath

enum ScanpathError derives CanEqual:
  case NoFixations
  case OutOfOrder(index: Int, previous: String, current: String)
  case WrongClock(index: Int, expected: String, actual: String)
  case InvalidTransitionSpan(index: Int, underlying: TimeError)
  case InvalidExtent(underlying: TimeError)
  case UnmappableFixation(
      index: Int,
      from: FrameId,
      to: FrameId,
      x: Double,
      y: Double
  )

  def message: String = this match
    case NoFixations =>
      "A scanpath needs at least one fixation."
    case OutOfOrder(i, prev, cur) =>
      s"Fixation $i at $cur begins before the previous one ended at $prev. " +
        "Overlapping fixations give the transition between them a negative " +
        "duration and make window selection depend on iteration order."
    case WrongClock(i, exp, act) =>
      s"Fixation $i is on clock '$act' but the scanpath is on '$exp'."
    case InvalidTransitionSpan(index, underlying) =>
      s"Fixations $index and ${index + 1} cannot form a scanpath transition: " +
        underlying.message
    case InvalidExtent(underlying) =>
      s"The ordered fixations cannot form a scanpath extent: ${underlying.message}"
    case UnmappableFixation(i, from, to, x, y) =>
      s"Fixation $i at ($x, $y) in frame '$from' has no finite image in frame '$to'. " +
        "The scanpath was not shortened; choose a warp defined over every fixation " +
        "or handle this failure explicitly."
