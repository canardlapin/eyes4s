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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.explore.*
import eyes4s.studio.app.plot.{HalfOpenSpan, PlotSource, TimelineColumns}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.selection.ViewId
import eyes4s.studio.desktop.plot.{PlotBrushAdapter, PlotTwin}
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.plot.TimelinePlot
import javafx.animation.AnimationTimer
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ToggleButton, ToggleGroup}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** Explore's timeline on the desktop (ticket S6.3; Explore.dc.html,
  * timeline): S4.5e's timeline plot and table twin over the trial view's
  * fixations, with the playhead, play, pause, step and speed, and the brush.
  * Brushing selects through the plot's input, as every view's selection
  * does, and draws its span; it changes no document value. The toolbar binds
  * an [[ExploreTimelineVM]]; a playing timeline is advanced by an animation
  * clock ([[TimelineIntent.Tick]]). FX thread only.
  */
final class ExploreTimelineHost(
    model: () => AppModel,
    app: Intent => Unit,
    trialView: () => ExploreTrialView
):
  private var state    = ExploreTimeline.empty
  private var disposed = false

  // The trial view's fixations as a timeline, read again only when the trial
  // view changes (not on every frame of playback).
  private var shownRead: Either[String, Option[TimelineRead]] = Right(None)

  private val columns: TimelineColumns =
    ExploreTimeline.columns.fold(e => throw IllegalStateException(e.message), identity)

  private def builder(brush: Option[HalfOpenSpan]): TimelinePlot =
    TimelinePlot(columns, brush, ExploreTimelineVM.of(state, shownRead).playheadMs)

  private def view(id: String): ViewId =
    ViewId.of(id).fold(e => throw IllegalStateException(e.toString), identity)

  val twin: PlotTwin = PlotTwin
    .attach(
      builder(None),
      view("explore.timeline"),
      view("explore.timeline.table"),
      model().selection,
      app
    )
    .fold(e => throw IllegalStateException(e.message), identity)

  val brush: PlotBrushAdapter = PlotBrushAdapter.attach(
    twin,
    TimelineColumns.brushRule(columns),
    builder,
    span => brushed(span)
  )

  // --- the toolbar ----------------------------------------------------------------
  val play: Button =
    button(() => if state.playing then TimelineIntent.Pause else TimelineIntent.Play)
  val stepBack: Button                         = button(() => TimelineIntent.StepBack)
  val stepForward: Button                      = button(() => TimelineIntent.StepForward)
  private val group                            = ToggleGroup()
  val speeds: Map[PlaybackSpeed, ToggleButton] = PlaybackSpeed.values.toVector.map { s =>
    val b = ToggleButton()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll("timeline-speed", "mono", "t11")
    b.setToggleGroup(group)
    b.setOnAction(_ => dispatch(TimelineIntent.SetSpeed(s)))
    s -> b
  }.toMap
  val status: Label     = label("timeline-status", "mono", "t11")
  val disclaimer: Label = label("timeline-disclaimer", "t11")
  val note: Label       = label("timeline-note", "t11")
  private val spacer    = Region()
  HBox.setHgrow(spacer, Priority.ALWAYS)
  private val bar = HBox(
    (Vector[javafx.scene.Node](play, stepBack, stepForward) ++
      PlaybackSpeed.values.toVector.map(speeds) ++
      Vector(status, spacer, disclaimer))*
  )
  bar.setAlignment(Pos.CENTER_LEFT)
  bar.getStyleClass.add("timeline-bar")
  VBox.setVgrow(twin.plotNode, Priority.ALWAYS)

  /** The Timeline pane: the toolbar over the plot. */
  val node: VBox = VBox(bar, note, twin.plotNode)
  node.getStyleClass.add("timeline-panel")
  Option(getClass.getClassLoader.getResource(ExploreTimelineHost.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The Table pane: the timeline's rows. */
  def tableNode: javafx.scene.Node = twin.tableNode

  private var shownSource: Option[PlotSource]                       = None
  private var drawn: Option[(Option[Double], Option[HalfOpenSpan])] = None
  private var drawnTheme: Option[eyes4s.studio.app.tokens.Theme]    = None

  // A playing timeline is advanced by the frames' elapsed time.
  private var running = false
  private val clock   = new AnimationTimer:
    private var last            = -1L
    def handle(now: Long): Unit =
      if last >= 0 then dispatch(TimelineIntent.Tick((now - last) / 1e6))
      last = now
      if !state.playing then
        last = -1L
        halt()

  private def halt(): Unit =
    clock.stop()
    running = false

  /** Whether the playback clock is running. */
  def clockRunning: Boolean = running

  /** The timeline's state now. */
  def timeline: ExploreTimeline = state

  /** The view-model now shown. */
  def vm: ExploreTimelineVM = ExploreTimelineVM.of(state, shownRead)

  private def shown: Option[eyes4s.studio.app.plot.Timeline] =
    shownRead.toOption.flatten.map(_.timeline)

  /** The toolbar's focus stops after the pane's: its buttons, the selected
    * speed (Tab visits a toggle group's selected choice only), then the plot,
    * one stop named by its spoken summary.
    */
  def focusStops: Vector[FocusStop] =
    val v = vm
    if !v.enabled then Vector.empty
    else
      Vector(
        FocusStop(A11yRole.Button, v.playLabel),
        FocusStop(A11yRole.Button, v.stepBack),
        FocusStop(A11yRole.Button, v.stepForward)
      ) ++ v.speeds.collect { case (_, label, true) =>
        FocusStop(A11yRole.ToggleButton, label)
      } ++
        Option(twin.plotHost.getAccessibleText).map(FocusStop(A11yRole.Region, _))

  /** Follow the model (its selection) and the trial view (its trial and
    * fixations).
    */
  def sync(m: AppModel): Unit =
    if !disposed then
      twin.project(m.selection)
      // A brush lasts while the selection is what it selected: a click, a
      // clear (Escape) or another view's selection ends it.
      if state.brush.isDefined && m.selection.selected != brushedRefs then
        dispatch(TimelineIntent.Brushed(None))
      render()

  /** The trial view changed: its trial or its fixations. */
  def refresh(): Unit =
    if !disposed then
      shownRead = ExploreTimeline.shown(trialView())
      state = ExploreTimeline.sync(state, trialView().trial)
      render()

  def dispatch(intent: TimelineIntent): Unit =
    if !disposed then
      state = ExploreTimeline.update(state, shown, intent)
      if state.playing && !running then
        running = true
        clock.start()
      render()

  def dispose(): Unit =
    if !disposed then
      disposed = true
      state = state.copy(playing = false)
      halt()
      brush.dispose()
      twin.dispose()

  // The adapter has drawn the span it brushed: keep it, without drawing again.
  private def brushed(span: Option[HalfOpenSpan]): Unit =
    if !disposed then
      state = ExploreTimeline.update(state, shown, TimelineIntent.Brushed(span))
      drawn = Some((ExploreTimelineVM.of(state, shownRead).playheadMs, span))
      render()

  /** The fixations the kept brush selects. */
  private def brushedRefs: Vector[eyes4s.studio.core.selection.StudioRef] =
    (for
      span <- state.brush
      src  <- shownSource
    yield eyes4s.studio.app.plot.PlotBrush.rows(src, TimelineColumns.brushRule(columns), span))
      .getOrElse(Vector.empty)

  private def render(): Unit =
    val v = vm
    play.setText(v.playLabel)
    play.setAccessibleText(v.playLabel)
    stepBack.setText(v.stepBack)
    stepBack.setAccessibleText(v.stepBack)
    stepForward.setText(v.stepForward)
    stepForward.setAccessibleText(v.stepForward)
    v.speeds.foreach { (s, label, on) =>
      val b = speeds(s)
      b.setText(label)
      b.setAccessibleText(label)
      b.setSelected(on)
    }
    Vector(play, stepBack, stepForward).foreach(_.setDisable(!v.enabled))
    speeds.values.foreach(_.setDisable(!v.enabled))
    status.setText(v.status)
    disclaimer.setText(v.disclaimer)
    val notes = v.note.toVector ++ v.skipped
    note.setText(notes.mkString("\n"))
    note.setVisible(notes.nonEmpty)
    note.setManaged(notes.nonEmpty)
    val theme = ExploreTimelineHost.theme(model())
    if v.source != shownSource || (v.source.isDefined && !drawnTheme.contains(theme)) then
      shownSource = v.source
      drawnTheme = Some(theme)
      v.source match
        case Some(src) =>
          // One build: clear, set the builder (the brush and the playhead)
          // on nothing, then show the source with it.
          twin.clear()
          brush.restore(v.brush)
          twin.show(src, theme)
        case None => twin.clear()
    else if v.source.isDefined && drawn != Some((v.playheadMs, v.brush)) then
      if drawn.exists(_._2 == v.brush) then brush.refresh()
      else brush.restore(v.brush)
    drawn = Some((v.playheadMs, v.brush))

  private def button(intent: () => TimelineIntent): Button =
    val b = Button()
    b.setMnemonicParsing(false)
    b.getStyleClass.addAll("timeline-button", "t11")
    b.setOnAction(_ => dispatch(intent()))
    b

  private def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

object ExploreTimelineHost:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-timeline.css"

  def theme(m: AppModel): eyes4s.studio.app.tokens.Theme =
    ExploreTrialViewVM.appearance(m)._1
