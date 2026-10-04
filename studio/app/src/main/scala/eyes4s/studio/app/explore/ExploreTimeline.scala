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

package eyes4s.studio.app.explore

import eyes4s.studio.app.plot.{
  HalfOpenSpan,
  PlotSource,
  PlotSourceError,
  Timeline,
  TimelineColumns,
  TimelineError,
  TimelineFixation
}
import eyes4s.studio.app.text.{ExploreText, ExploreTextId, Format}
import eyes4s.studio.core.backend.{AdmittedFixation, TrialFixations, TrialKey}

/** A playback speed of the timeline (Explore.dc.html, timeline). */
enum PlaybackSpeed(val factor: Double) derives CanEqual:
  case Half extends PlaybackSpeed(0.5)
  case One  extends PlaybackSpeed(1.0)
  case Two  extends PlaybackSpeed(2.0)

/** A user action or clock fact of Explore's timeline. */
enum TimelineIntent derives CanEqual:
  case Play, Pause

  /** Move the playhead to the next fixation's onset, or the previous one's. */
  case StepForward, StepBack
  case SetSpeed(speed: PlaybackSpeed)

  /** `elapsedMs` of wall-clock time passed while playing. */
  case Tick(elapsedMs: Double)

  /** A brush on the timeline: the onset span it covers, or none (cleared). */
  case Brushed(span: Option[HalfOpenSpan])

/** Explore's timeline (ticket S6.3; Explore.dc.html, timeline): the shown
  * trial's fixation intervals as bars, a playhead with play, pause, step and
  * speed, and a brush.
  *
  * The intervals are the backend's (protocol 1.6 `trialFixations`, the trial
  * view's read), each onset and duration to the millisecond. The brush
  * selects the fixations whose intervals overlap it (S4.5e's
  * `BrushRule.Overlaps`), through the selection every view shares; it is
  * view state only and never changes the analysis or the document. The
  * playhead is a time in the trial, from its start to the end of its last
  * fixation; playback stops there.
  */
final case class ExploreTimeline(
    trial: Option[TrialKey],
    playheadMs: Double,
    playing: Boolean,
    speed: PlaybackSpeed,
    brush: Option[HalfOpenSpan]
) derives CanEqual

object ExploreTimeline:

  val empty: ExploreTimeline = ExploreTimeline(None, 0.0, false, PlaybackSpeed.One, None)

  /** The columns of the timeline's source. */
  val columns: Either[PlotSourceError, TimelineColumns] = TimelineColumns.standard

  /** A trial's timeline from its admitted fixations, to the millisecond, and
    * the fixations too short to draw at that resolution.
    *
    * Each fixation's onset and end are rounded, and its duration is the
    * difference, so fixations that meet in the data meet on the timeline:
    * rounding onset and duration apart can open or close a millisecond
    * between them, and a brush would then pick a fixation it does not
    * touch. A fixation whose rounded end is its rounded onset is left out
    * and reported, as the trial view reports a mark too short to draw.
    */
  def timeline(fixations: TrialFixations): Either[TimelineError, TimelineRead] =
    val rounded = fixations.fixations.map { f =>
      val onset = math.round(f.onsetMs)
      f -> TimelineFixation(f.ref.index, onset, math.round(f.onsetMs + f.durationMs) - onset)
    }
    val (drawn, skipped) = rounded.partition(_._2.durationMs > 0)
    Timeline.of(fixations.trial, drawn.map(_._2)).map(TimelineRead(_, skipped.map(_._1)))

  /** The timeline of the trial view's fixations, when it has read them; or
    * why they make no timeline.
    */
  def shown(view: ExploreTrialView): Either[String, Option[TimelineRead]] =
    view.fixations.toOption.collect { case BackendAnswer.Answered(f) => f } match
      case None    => Right(None)
      case Some(f) => timeline(f).map(Some(_)).left.map(_.message)

  /** The trial's end: the end of its last fixation. */
  def endMs(timeline: Timeline): Double =
    timeline.fixations.map(f => (f.onsetMs + f.durationMs).toDouble).maxOption.getOrElse(0.0)

  /** Follow the trial view's trial: a new trial starts at its beginning,
    * paused and unbrushed; the speed stays.
    */
  def sync(timeline: ExploreTimeline, trial: Option[TrialKey]): ExploreTimeline =
    if trial == timeline.trial then timeline
    else empty.copy(trial = trial, speed = timeline.speed)

  /** The Elm-style update over the shown trial's timeline (if read). */
  def update(
      state: ExploreTimeline,
      shown: Option[Timeline],
      intent: TimelineIntent
  ): ExploreTimeline =
    import TimelineIntent.*
    val end = shown.fold(0.0)(endMs)
    intent match
      case Play =>
        // Playing from the end starts again.
        val from = if state.playheadMs >= end then 0.0 else state.playheadMs
        state.copy(playing = shown.isDefined && end > 0.0, playheadMs = from)
      case Pause           => state.copy(playing = false)
      case SetSpeed(s)     => state.copy(speed = s)
      case Brushed(span)   => state.copy(brush = span)
      case Tick(elapsedMs) =>
        if !state.playing || !elapsedMs.isFinite || elapsedMs <= 0.0 then state
        else
          val next = state.playheadMs + elapsedMs * state.speed.factor
          if next >= end then state.copy(playheadMs = end, playing = false)
          else state.copy(playheadMs = next)
      case StepForward =>
        shown
          .flatMap(_.fixations.map(_.onsetMs.toDouble).find(_ > state.playheadMs))
          .fold(state)(at => state.copy(playheadMs = at, playing = false))
      case StepBack =>
        shown
          .flatMap(_.fixations.map(_.onsetMs.toDouble).findLast(_ < state.playheadMs))
          .fold(state)(at => state.copy(playheadMs = at, playing = false))

/** A trial's timeline, and the admitted fixations too short to draw on it. */
final case class TimelineRead(timeline: Timeline, skipped: Vector[AdmittedFixation])
    derives CanEqual

/** What the timeline's toolbar and plot show. */
final case class ExploreTimelineVM(
    playLabel: String,
    stepBack: String,
    stepForward: String,
    speeds: Vector[(PlaybackSpeed, String, Boolean)],
    status: String,
    disclaimer: String,
    source: Option[PlotSource],
    playheadMs: Option[Double],
    brush: Option[HalfOpenSpan],
    note: Option[String],
    skipped: Vector[String],
    enabled: Boolean
) derives CanEqual

object ExploreTimelineVM:
  import ExploreTextId.*

  private def t(id: ExploreTextId, args: String*): String = ExploreText(id, args*)

  /** Seconds to two places, as the board writes them ("2.16"). */
  def seconds(ms: Double): String = Format.decimal(ms / 1000.0, 2)

  def speedLabel(s: PlaybackSpeed): String = s match
    case PlaybackSpeed.Half => t(SpeedHalf)
    case PlaybackSpeed.One  => t(SpeedOne)
    case PlaybackSpeed.Two  => t(SpeedTwo)

  def of(
      state: ExploreTimeline,
      shown: Either[String, Option[TimelineRead]]
  ): ExploreTimelineVM =
    val read     = shown.toOption.flatten
    val timeline = read.map(_.timeline)
    val source   = for
      tl   <- timeline
      cols <- ExploreTimeline.columns.toOption
      src  <- Timeline.source(tl, cols).toOption
    yield src
    val playhead = t(PlayheadStatus, seconds(state.playheadMs))
    val brushed  =
      state.brush.map(b => t(BrushStatus, seconds(b.from), seconds(b.until))).toVector
    ExploreTimelineVM(
      if state.playing then t(Pause) else t(Play),
      t(StepBack),
      t(StepForward),
      PlaybackSpeed.values.toVector.map(s => (s, speedLabel(s), s == state.speed)),
      (playhead +: brushed).mkString(" · "),
      t(BrushDisclaimer),
      source,
      timeline.map(_ => state.playheadMs),
      state.brush,
      shown.left.toOption.map(why => t(TimelineNotRead, state.trial.fold("")(_.label), why)),
      read.toVector
        .flatMap(_.skipped)
        .map(f => t(SkippedMark, f.ref.index.value.toString, f.durationMs.toString)),
      timeline.isDefined
    )
