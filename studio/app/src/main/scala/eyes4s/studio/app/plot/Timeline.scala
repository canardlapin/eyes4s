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

package eyes4s.studio.app.plot

import eyes4s.studio.app.text.{TimelineText, TimelineTextId}
import eyes4s.studio.core.backend.{TrialKey, TrialTemporalExtent}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

/** Why a trial's timeline was refused. Every case names the trial and the
  * fixation that failed.
  */
enum TimelineError derives CanEqual:

  /** Fixation `fixation` begins before the trial's clock starts. */
  case NegativeOnset(trial: TrialKey, fixation: Int, onsetMs: Long)
  case OtherExtent(trial: TrialKey, extentTrial: TrialKey)

  /** Fixation `fixation` lasts no time. */
  case DurationNotPositive(trial: TrialKey, fixation: Int, durationMs: Long)

  /** Fixation `fixation` is listed more than once. */
  case DuplicateFixation(trial: TrialKey, fixation: Int)

  /** Fixation `fixation` begins before the fixation before it. */
  case OutOfOrder(trial: TrialKey, fixation: Int, onsetMs: Long, previousOnsetMs: Long)

  def message: String = this match
    case OtherExtent(t, e) => s"Timeline of ${t.label} has a temporal extent for ${e.label}."
    case NegativeOnset(t, f, on) =>
      s"Timeline of ${t.label}: fixation $f begins at $on ms, before the trial."
    case DurationNotPositive(t, f, d) =>
      s"Timeline of ${t.label}: fixation $f lasts $d ms."
    case DuplicateFixation(t, f)    => s"Timeline of ${t.label}: fixation $f is listed twice."
    case OutOfOrder(t, f, on, prev) =>
      s"Timeline of ${t.label}: fixation $f begins at $on ms, before the previous one at $prev ms."

/** One fixation of a trial's timeline, as the data serve it: its onset from
  * the trial's start and its duration, in milliseconds.
  */
final case class TimelineFixation(index: FixationIndex, onsetMs: Long, durationMs: Long)
    derives CanEqual

/** A trial's fixation intervals in onset order (ticket S4.5e). */
final case class Timeline private (
    trial: TrialKey,
    fixations: Vector[TimelineFixation],
    extent: TrialTemporalExtent
) derives CanEqual:

  /** The ref of each fixation, in order. */
  def refs: Vector[StudioRef] = fixations.map(f => StudioRef.Fixation(trial, f.index))

object Timeline:

  /** The timeline of `trial`: onsets not before zero and in order, positive
    * durations, each fixation once.
    */
  def of(
      trial: TrialKey,
      fixations: Vector[TimelineFixation],
      extent: Option[TrialTemporalExtent] = None
  ): Either[TimelineError, Timeline] =
    def first[A](as: Vector[A])(bad: A => Option[TimelineError]) =
      as.iterator.flatMap(bad).nextOption().toLeft(())
    for
      _ <- extent
        .filter(_.trial != trial)
        .map(e => TimelineError.OtherExtent(trial, e.trial))
        .toLeft(())
      _ <- first(fixations)(f =>
        Option.when(f.onsetMs < 0)(TimelineError.NegativeOnset(trial, f.index.value, f.onsetMs))
      )
      _ <- first(fixations)(f =>
        Option.when(f.durationMs <= 0)(
          TimelineError.DurationNotPositive(trial, f.index.value, f.durationMs)
        )
      )
      _ <- first(fixations.map(_.index).diff(fixations.map(_.index).distinct))(i =>
        Some(TimelineError.DuplicateFixation(trial, i.value))
      )
      _ <- first(fixations.zip(fixations.drop(1)))((a, b) =>
        Option.when(b.onsetMs < a.onsetMs)(
          TimelineError.OutOfOrder(trial, b.index.value, b.onsetMs, a.onsetMs)
        )
      )
    yield Timeline(trial, fixations, extent.getOrElse(TrialTemporalExtent.LegacyMissing(trial)))

  /** The timeline as a value source: one row per fixation, in onset order,
    * with its onset and duration as the data serve them.
    */
  def source(
      timeline: Timeline,
      columns: TimelineColumns
  ): Either[PlotSourceError, PlotSource] =
    PlotSource(
      TimelineText(TimelineTextId.Caption, timeline.trial.label),
      Vector(
        PlotColumn(
          columns.fixation,
          TimelineText(TimelineTextId.FixationHeader),
          ColumnFormat.Label
        ),
        PlotColumn(columns.onset, TimelineText(TimelineTextId.OnsetHeader), ColumnFormat.Count),
        PlotColumn(
          columns.duration,
          TimelineText(TimelineTextId.DurationHeader),
          ColumnFormat.Count
        )
      ),
      timeline.fixations
        .zip(timeline.refs)
        .map((f, ref) =>
          PlotRow(
            ref,
            Vector(
              PlotValue.Text(f.index.value.toString),
              PlotValue.Number(f.onsetMs.toDouble),
              PlotValue.Number(f.durationMs.toDouble)
            )
          )
        )
    )

/** The columns of a timeline's value source. */
final case class TimelineColumns(fixation: ColumnId, onset: ColumnId, duration: ColumnId)
    derives CanEqual

object TimelineColumns:

  /** The timeline's brush: a fixation is picked iff its interval
    * [onset, onset + duration) overlaps the brushed span.
    */
  def brushRule(columns: TimelineColumns): BrushRule =
    BrushRule.Overlaps(columns.onset, columns.duration)

  val standard: Either[PlotSourceError, TimelineColumns] =
    for
      fixation <- ColumnId.of("fixation")
      onset    <- ColumnId.of("onset")
      duration <- ColumnId.of("duration")
    yield TimelineColumns(fixation, onset, duration)

/** A brushed span of a numeric column, half-open: it holds `from` and every
  * value up to but not including `until`. Made from two different ends in
  * either order, as a drag gives them; a zero-length drag is no span, so
  * it selects nothing.
  */
final case class HalfOpenSpan private (from: Double, until: Double) derives CanEqual:

  /** Whether `value` lies in the span. */
  def holds(value: Double): Boolean = from <= value && value < until

object HalfOpenSpan:

  /** The span between `a` and `b`, whichever is first; none when they are
    * equal or either is not finite.
    */
  def between(a: Double, b: Double): Option[HalfOpenSpan] =
    Option.when(a.isFinite && b.isFinite && a != b)(
      HalfOpenSpan(math.min(a, b), math.max(a, b))
    )

/** How a brush picks rows from its span (ticket S4.5e; bead decision on
  * the timeline's brush).
  */
enum BrushRule derives CanEqual:

  /** A row is the half-open interval [start, start + length) of its values
    * in `start` and `length`; it is picked iff that interval and the span
    * intersect: `start < until && from < start + length`. Touching at an
    * endpoint is not overlap, and an interval that contains the whole span
    * is picked. On a timeline, the bars a brush visibly touches are exactly
    * the fixations it selects.
    */
  case Overlaps(start: ColumnId, length: ColumnId)

  /** A row is its value in `column`; it is picked iff the span holds it. */
  case Holds(column: ColumnId)

/** What a brush selects: the rows of a source that `rule` picks from
  * `span`, in row order. A row without the values the rule reads is not
  * picked. Nothing is computed but the interval's end and the comparisons.
  */
object PlotBrush:

  def rows(source: PlotSource, rule: BrushRule, span: HalfOpenSpan): Vector[StudioRef] =
    def picked(i: Int): Boolean = rule match
      case BrushRule.Overlaps(start, length) =>
        (for
          s <- source.number(i, start)
          l <- source.number(i, length)
        yield s < span.until && span.from < s + l).getOrElse(false)
      case BrushRule.Holds(column) => source.number(i, column).exists(span.holds)
    source.rows.indices.toVector.filter(picked).map(source.rows(_).ref)
