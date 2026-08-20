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

package eyes4s.kernel

/** One instantaneous value on the clock carried by its enclosing timeline. */
final case class Mark[+A](at: Instant, value: A)

/** A neutral, clock-bound sequence of instantaneous marks.
  *
  * Construction orders marks by timestamp. Equal timestamps are legal and
  * retain input order, so simultaneous messages remain deterministic.
  */
final class Timeline[+A] private (
    val clock: ClockId,
    val marks: Vector[Mark[A]]
):
  def size: Int         = marks.size
  def isEmpty: Boolean  = marks.isEmpty
  def nonEmpty: Boolean = marks.nonEmpty

  def map[B](f: A => B): Timeline[B] =
    new Timeline(clock, marks.map(mark => Mark(mark.at, f(mark.value))))

  override def equals(other: Any): Boolean = other match
    case that: Timeline[?] => clock == that.clock && marks == that.marks
    case _                 => false

  override def hashCode: Int = 31 * clock.hashCode + marks.hashCode

  override def toString: String = s"Timeline($clock,${marks.size} marks)"

object Timeline:
  def of[A](clock: ClockId, marks: Vector[Mark[A]]): Either[TimelineError, Timeline[A]] =
    if clock.name.trim.isEmpty then Left(TimelineError.BlankClock(clock.name))
    else Right(new Timeline(clock, stableTimeOrder(marks)))

  def empty[A](clock: ClockId): Either[TimelineError, Timeline[A]] = of(clock, Vector.empty)

  private def stableTimeOrder[A](values: Vector[Mark[A]]): Vector[Mark[A]] =
    values.zipWithIndex
      .sortWith { case ((left, leftIndex), (right, rightIndex)) =>
        val byTime = java.lang.Long.compare(left.at.toMicros, right.at.toMicros)
        byTime < 0 || (byTime == 0 && leftIndex < rightIndex)
      }
      .map(_._1)

/** Scheduled stimulus timing. It cannot satisfy an API requiring realized
  * timing without an explicit, provenance-bearing operation at a higher layer.
  */
final class PlannedTimeline[+A] private (val timeline: Timeline[A]):
  def clock: ClockId                        = timeline.clock
  def marks: Vector[Mark[A]]                = timeline.marks
  def map[B](f: A => B): PlannedTimeline[B] = new PlannedTimeline(timeline.map(f))

  override def equals(other: Any): Boolean = other match
    case that: PlannedTimeline[?] => timeline == that.timeline
    case _                        => false

  override def hashCode: Int = timeline.hashCode

object PlannedTimeline:
  def from[A](timeline: Timeline[A]): PlannedTimeline[A] = new PlannedTimeline(timeline)

  def of[A](
      clock: ClockId,
      marks: Vector[Mark[A]]
  ): Either[TimelineError, PlannedTimeline[A]] = Timeline.of(clock, marks).map(from)

/** Timing observed on a source clock. This is the only timeline kind that may
  * support claims about realized experiment timing.
  */
final class ObservedTimeline[+A] private (val timeline: Timeline[A]):
  def clock: ClockId                         = timeline.clock
  def marks: Vector[Mark[A]]                 = timeline.marks
  def map[B](f: A => B): ObservedTimeline[B] = new ObservedTimeline(timeline.map(f))

  override def equals(other: Any): Boolean = other match
    case that: ObservedTimeline[?] => timeline == that.timeline
    case _                         => false

  override def hashCode: Int = timeline.hashCode

object ObservedTimeline:
  def from[A](timeline: Timeline[A]): ObservedTimeline[A] = new ObservedTimeline(timeline)

  def of[A](
      clock: ClockId,
      marks: Vector[Mark[A]]
  ): Either[TimelineError, ObservedTimeline[A]] = Timeline.of(clock, marks).map(from)

enum TimelineError derives CanEqual:
  case BlankClock(value: String)

  def message: String = this match
    case BlankClock(value) => s"Timeline clock='$value' is blank."

end TimelineError
