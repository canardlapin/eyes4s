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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.Intent
import eyes4s.studio.app.compare.{
  PanelRole,
  PanelSide,
  PanelOverlay,
  PanelOverlays,
  TrialPanelVM,
  TrialPanelsVM
}
import eyes4s.studio.app.text.{PanelText, PanelTextId}
import eyes4s.studio.app.compare.{ContentFixation, PanelContent}
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.desktop.plot.TableTwinView
import eyes4s.studio.desktop.maps.MapRasterStore
import eyes4s.studio.desktop.trial.{MapRequest, StimulusSource, TrialView}
import eyes4s.studio.viz.trial.{
  MarkStyle,
  RememberedImage,
  TrialFixation,
  TrialExtent,
  TrialRole,
  TrialScene,
  TrialSceneInput
}
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ToggleButton}
import javafx.scene.layout.{HBox, Priority, Region, StackPane, VBox}

/** Compare's query and reference trial panels on the desktop (ticket S8.2;
  * Main.dc.html, panels). Each panel shows its role pill and title, its
  * toolbar and its readout above the stage: the query panel the underlay
  * toggle and the query's contrast, the reference panel the way back to the
  * matched reference, the matched reference's identity and the inspected
  * pair's score. It only binds [[TrialPanelsVM]]. Use on the JavaFX thread.
  */
final class TrialPanelsView(
    app: Intent => Unit,
    underlay: Boolean => Unit,
    retried: () => Unit,
    stimuli: StimulusSource,
    overlayChanged: (PanelSide, PanelOverlay, Boolean) => Unit = (_, _, _) => (),
    maps: MapRasterStore = TrialView.sharedMaps
):

  private final class Panel(styleClass: String, side: PanelSide):
    private val controlContext =
      if side == PanelSide.Query then PanelTextId.Query else PanelTextId.Reference
    private var overlays  = PanelOverlays()
    val view              = TrialView(stimuli, TrialView.sharedLoader, maps)
    val said              = Label()
    var drawn             = Option.empty[TrialSceneInput]
    val pill              = Label()
    val title             = Label()
    val readout           = Label()
    val count             = Label()
    val note              = Label()
    val tools             = HBox(4.0)
    val stage             = StackPane()
    val points            = ToggleButton(PanelText(PanelTextId.Points))
    val order             = ToggleButton(PanelText(PanelTextId.Order))
    val map               = ToggleButton(PanelText(PanelTextId.Map))
    private val mapStatus = Label()
    private var request   = Option.empty[MapRequest]
    private var mapReason = Option.empty[String]
    Vector(points -> PanelOverlay.Points, order -> PanelOverlay.Order, map -> PanelOverlay.Map)
      .foreach { (b, which) =>
        b.getStyleClass.add("tog")
        b.setSelected(true)
        b.setAccessibleText(
          PanelText(PanelTextId.OverlayControl, PanelText(controlContext), b.getText)
        )
        b.setOnAction(_ => overlayChanged(side, which, b.isSelected))
        tools.getChildren.add(b)
      }
    mapStatus.getStyleClass.addAll("panel-note", "t11")
    mapStatus.setWrapText(true)
    private def refresh(): Unit =
      drawn.foreach(view.show)
      view.showMap(if overlays.map then request else None)
      refreshMapStatus()

    private def refreshMapStatus(): Unit =
      val reason = mapReason.orElse(
        view.mapRefused.map(e => PanelText(PanelTextId.MapUnavailable, e.message))
      )
      mapStatus.setText(if overlays.map then reason.getOrElse("") else "")
      mapStatus.setVisible(mapStatus.getText.nonEmpty)
      mapStatus.setManaged(mapStatus.isVisible)
    view.mapRefusedProperty.addListener((_, _, _) =>
      refreshMapStatus()
      offerRetry()
    )

    def showMap(value: (Option[MapRequest], Option[String])): Unit =
      request = value._1
      mapReason = value._2
      refresh()
    pill.getStyleClass.add("panel-pill")
    title.getStyleClass.addAll("panel-title", "t12")
    readout.getStyleClass.addAll("panel-readout", "mono", "t11")
    note.getStyleClass.addAll("panel-note", "t11")
    stage.getStyleClass.add("stage")
    said.getStyleClass.addAll("panel-note", "t11")
    said.setWrapText(true)
    stage.getChildren.addAll(view, said)
    VBox.setVgrow(stage, Priority.ALWAYS)
    private val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    private val head = HBox(6.0, pill, title)
    head.setAlignment(Pos.CENTER_LEFT)
    count.getStyleClass.addAll("lbl", "mono")
    private val bar = HBox(8.0, tools, spacer, count)
    bar.setAlignment(Pos.CENTER_LEFT)
    bar.getStyleClass.add("panel-toolbar")
    readout.getStyleClass.add("panel-toolbar")
    val node: VBox = VBox(4.0, head, bar, readout, note, mapStatus, stage)
    node.getStyleClass.addAll("trial-panel", styleClass)
    Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
      .foreach(url => node.getStylesheets.add(url.toExternalForm))

    // Draws `input` unless it is already drawn; nothing clears the view
    // while it is drawn but a reason.
    def draw(input: Either[String, TrialSceneInput]): Unit =
      input match
        case Right(in) =>
          if !drawn.contains(in) then
            drawn = Some(in)
            refresh()
          said.setText("")
        case Left(why) =>
          if drawn.nonEmpty then
            drawn = None
            view.clear()
          said.setText(why)
      said.setVisible(said.getText.nonEmpty)

    def show(vm: Option[TrialPanelVM], empty: Option[String]): Unit =
      vm match
        case Some(p) =>
          val changed = overlays != p.overlays
          overlays = p.overlays
          points.setSelected(overlays.points)
          order.setSelected(overlays.order)
          map.setSelected(overlays.map)
          if changed then refresh()
          pill.setText(p.role.label)
          pill.getStyleClass.setAll("panel-pill", pillClass(p.role))
          title.setText(p.title)
          count.setText(p.count)
          readout.setText(p.readout.text)
          readout.setAccessibleText(p.readout.text)
          node.setAccessibleText(s"${p.role.label}: ${p.title}")
        case None =>
          pill.setText("")
          title.setText(empty.getOrElse(""))
          count.setText("")
          readout.setText("")
          readout.setAccessibleText(null)
          node.setAccessibleText(empty.orNull)
      pill.setVisible(vm.isDefined)
      pill.setManaged(vm.isDefined)
      Vector(points, order, map).foreach { b =>
        b.setVisible(vm.isDefined)
        b.setManaged(vm.isDefined)
      }

  private def pillClass(role: PanelRole): String = role match
    case PanelRole.Query   => "pill-query"
    case PanelRole.Matched => "pill-matched"
    case PanelRole.Control => "pill-control"

  private val query     = Panel("query-panel", PanelSide.Query)
  private val reference = Panel("reference-panel", PanelSide.Reference)

  private val underlayToggle = ToggleButton(PanelText(PanelTextId.UnderlayShort))
  underlayToggle.getStyleClass.add("tog")
  underlayToggle.setAccessibleText(PanelText(PanelTextId.Underlay))
  underlayToggle.setOnAction(_ => underlay(underlayToggle.isSelected))
  query.tools.getChildren.add(underlayToggle)

  private val back = Button()
  back.getStyleClass.add("tog")
  reference.tools.getChildren.add(back)

  // Asks again for what failed: the panels' answers and the views' stimuli.
  private val retry = Button(PanelText(PanelTextId.Retry))
  retry.getStyleClass.add("tog")
  retry.setAccessibleText(PanelText(PanelTextId.Retry))
  retry.setOnAction { _ =>
    retried()
    query.view.retry()
    reference.view.retry()
  }
  query.tools.getChildren.add(retry)
  private var failed               = false
  private var mapsFailed           = false
  def mapFailed(on: Boolean): Unit =
    mapsFailed = on
    offerRetry()
  Vector(query, reference).foreach(
    _.view.stimulusFailures.addListener((_, _, _) => offerRetry())
  )

  private def offerRetry(): Unit =
    val show = failed || mapsFailed ||
      Vector(query, reference).exists(_.view.mapRefused.nonEmpty) ||
      Vector(query, reference).exists(!_.view.stimulusFailures.get.isEmpty)
    retry.setVisible(show)
    retry.setManaged(show)

  /** The query panel's content. */
  def queryNode: VBox = query.node

  /** The reference panel's content. */
  def referenceNode: VBox = reference.node

  /** The stages, where the trial views go. */
  def queryStage: StackPane     = query.stage
  def referenceStage: StackPane = reference.stage

  /** The trial views on the query's and the reference's stages. */
  def queryView: TrialView     = query.view
  def referenceView: TrialView = reference.view

  /** Each panel's fixation count, as shown. */
  def counts: (String, String) = (query.count.getText, reference.count.getText)

  /** Toggles the underlay, as the user clicks it. */
  def toggleUnderlay(): Unit = underlayToggle.fire()

  /** Map replies are bound to each panel by CompareMapHost. */
  def showMaps(
      q: (Option[MapRequest], Option[String]),
      r: (Option[MapRequest], Option[String])
  ): Unit =
    query.showMap(q)
    reference.showMap(r)

  /** The actual panel toolbar controls, for keyboard routes and native checks. */
  def queryToggles: Vector[ToggleButton]     = Vector(query.points, query.order, query.map)
  def referenceToggles: Vector[ToggleButton] =
    Vector(reference.points, reference.order, reference.map)

  /** Why a panel's stage draws nothing, if it says so. */
  def stageNotes: (String, String) = (query.said.getText, reference.said.getText)

  /** Disposes both trial views. */
  def dispose(): Unit =
    query.view.dispose()
    reference.view.dispose()

  /** What each panel shows: role pill, title and readout. */
  def shown: Vector[(String, String, String)] =
    Vector(query, reference).map(p => (p.pill.getText, p.title.getText, p.readout.getText))

  /** Retry, if it is offered; and pressing it, as the user does. */
  def retryOffered: Boolean = retry.isVisible
  def pressRetry(): Unit    = retry.fire()

  /** The query panel's note on the remembered image, if it shows one. */
  def rememberedNote: String = query.note.getText

  /** The way back to the matched reference, if it is offered. */
  def backOffered: Option[String] = Option.when(back.isVisible)(back.getText)

  /** The matched reference the reference panel names. */
  def matchedNote: String = reference.note.getText

  /** Clicks the way back, as the user does. */
  def pressBack(): Unit = back.fire()

  /** Binds `vm`, the stages drawn on the `stage` surround. */
  def render(vm: TrialPanelsVM, theme: Theme, stage: StageVariant = StageVariant.Dark): Unit =
    query.show(vm.query, vm.empty)
    reference.show(vm.reference, None)
    val remembered = vm.remembered.fold(RememberedImage.Absent)(r =>
      if r.shown then RememberedImage.Shown(r.asset) else RememberedImage.Hidden(r.asset)
    )
    val q = vm.query.map(TrialPanelsView.input(_, theme, remembered, stage))
    val r = vm.reference.map(TrialPanelsView.input(_, theme, RememberedImage.Absent, stage))
    // Both stages share one covering extent, so the two trials are drawn at
    // one scale (the S4.3a review's rule for side-by-side trials).
    val shared = TrialScene.sharedExtent((q ++ r).flatMap(_.toOption).toVector)
    def covered(in: Either[String, TrialSceneInput]) = in.map(i =>
      shared.fold(i)(e => i.copy(options = i.options.copy(extent = TrialExtent.Covering(e))))
    )
    // A panel with nothing to show clears its stage, so no earlier trial stays.
    query.draw(q.fold(Left(vm.empty.getOrElse("")))(covered))
    reference.draw(r.fold(Left(""))(covered))
    query.note.setText(vm.rememberedNote.getOrElse(""))
    failed = vm.retry.isDefined
    offerRetry()
    underlayToggle.setSelected(vm.underlay)
    underlayToggle.setVisible(vm.query.isDefined)
    underlayToggle.setManaged(vm.query.isDefined)
    val extras = vm.extras
    reference.note.setText(extras.fold("")(_.matched))
    extras.flatMap(_.back) match
      case Some((label, intent)) =>
        back.setText(label)
        back.setAccessibleText(label)
        back.setOnAction(_ => app(intent))
        back.setVisible(true)
        back.setManaged(true)
      case None =>
        back.setVisible(false)
        back.setManaged(false)

object TrialPanelsView:

  /** The trial role a panel's marks are drawn in (DESIGN_SPEC section 5). */
  def roleOf(role: PanelRole): TrialRole = role match
    case PanelRole.Query   => TrialRole.Query
    case PanelRole.Matched => TrialRole.Matched
    case PanelRole.Control => TrialRole.Control

  /** The scene input of panel `p`, or why its stage draws nothing. */
  def input(
      p: TrialPanelVM,
      theme: Theme,
      remembered: RememberedImage,
      stage: StageVariant = StageVariant.Dark
  ): Either[String, TrialSceneInput] =
    p.content match
      case PanelContent.Reading          => Left(PanelText(PanelTextId.ReadingTrial))
      case PanelContent.Unavailable(why) => Left(why)
      case PanelContent.Shown(c)         =>
        fixations(c.fixations).map(fs =>
          TrialSceneInput(
            c.display,
            c.screen,
            fs,
            MarkStyle.Role(roleOf(p.role)),
            theme,
            stage,
            options = eyes4s.studio.viz.trial
              .TrialSceneOptions(points = p.overlays.points, order = p.overlays.order),
            remembered = remembered
          )
        )

  private def fixations(all: Vector[ContentFixation]): Either[String, Vector[TrialFixation]] =
    all.foldLeft[Either[String, Vector[TrialFixation]]](Right(Vector.empty)) { (acc, f) =>
      acc.flatMap(got =>
        TrialFixation
          .of(f.trial, f.index, f.screenX, f.screenY, f.durationMs, f.placement)
          .map(got :+ _)
          .left
          .map(_.message)
      )
    }
