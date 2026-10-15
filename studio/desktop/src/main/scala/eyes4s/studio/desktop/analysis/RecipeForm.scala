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

package eyes4s.studio.desktop.analysis

import eyes4s.plan.{FieldId, FormValues, FormView, RawValue}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.analysis.{AnalysisSelection, DescriptorEditor, RecipeRead}
import eyes4s.studio.app.text.{RecipeText, RecipeTextId as T}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.{RecipeEditRequest, RecipeFormResult}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.desktop.shell.DisabledReason
import javafx.application.Platform
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.VBox
import javafx.scene.text.{Text, TextFlow}

trait RecipeInputs:
  def read(request: RecipeEditRequest, done: Either[String, RecipeFormResult] => Unit): Unit

object RecipeInputs:
  def of(session: StudioSession): RecipeInputs = new RecipeInputs:
    def read(request: RecipeEditRequest, done: Either[String, RecipeFormResult] => Unit): Unit =
      session.run(session.backend.recipeForm(request)) {
        case Left(error)   => done(Left(Option(error.getMessage).getOrElse(error.toString)))
        case Right(result) => done(result.left.map(_.message))
      }

/** The native recipe form, served and parsed by the backend. */
final class RecipeForm(model: () => AppModel, app: Intent => Unit, inputs: RecipeInputs):
  private var panel                                     = DescriptorEditor()
  private var disposed                                  = false
  private var rendered: Option[FormView]                = None
  private var renderedTarget: Option[RecipeEditRequest] = None
  private var scaleId: Option[FieldId]                  = None
  private var scalesRaw: RawValue                       = RawValue.Absent
  private var lock                                      = false
  private var shown                                     = true
  private var observers                                 = Vector.empty[Option[String] => Unit]
  def followEditing(f: Option[String] => Unit): Unit    =
    observers :+= f
    f(editingReason)
  def editingReason: Option[String] =
    Option.when(
      panel.target.nonEmpty && (panel.pending || panel.error.nonEmpty ||
        panel.result.exists(r => r.problems.nonEmpty) || panel.command(model()).nonEmpty)
    )(problem.getText)
  def setShown(value: Boolean): Unit =
    shown = value
    node.setVisible(shown && panel.target.nonEmpty)
    node.setManaged(node.isVisible)
  val descriptor =
    DescriptorForm(raw => edit(scaleId.fold(raw)(id => raw.updated(id, scalesRaw))))
  val scales = ScaleChips(raw =>
    scalesRaw = raw
    scaleId.foreach(id => edit(descriptor.values.updated(id, raw)))
  )
  val apply: Button = Button(RecipeText(T.Apply))
  apply.getStyleClass.add("studio-button")
  apply.setId("recipe-apply")
  apply.setAccessibleText(RecipeText(T.Apply))
  val problem = Label()
  problem.setWrapText(true)
  problem.getStyleClass.add("t11")
  DisabledReason.attach(apply, problem)
  DisabledReason.attach(descriptor.node, problem)
  DisabledReason.attach(scales.node, problem)
  private val sentence = TextFlow()
  sentence.getStyleClass.add("recipe-sentence")
  private val conventions = Label()
  conventions.setWrapText(true)
  conventions.getStyleClass.add("t11")
  val node = VBox(8, sentence, scales.node, descriptor.node, conventions, problem, apply)
  node.getStyleClass.add("recipe-form")
  Option(getClass.getClassLoader.getResource("eyes4s/studio/desktop/studio-recipe.css"))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))
  def state: DescriptorEditor             = panel
  def ready: Boolean                      = panel.form.nonEmpty && !panel.pending
  private def edit(raw: FormValues): Unit = if !disposed && !lock then
    val (next, request) = panel.edit(raw)
    panel = next
    render(rebuild = false)
    perform(request)

  /** Change raw fields through the same validation path as individual controls. */
  def editValues(raw: FormValues): Unit                  = edit(raw)
  private def perform(request: Option[RecipeRead]): Unit = request.foreach { read =>
    inputs.read(
      read.request,
      answer =>
        Platform.runLater(() =>
          if !disposed then
            val before = panel
            panel = panel.read(read, answer)
            if panel != before then
              render(rebuild = rendered != panel.form || renderedTarget != panel.target)
        )
    )
  }
  apply.setOnAction { _ =>
    if !disposed && !lock then
      panel.command(model()).foreach { command =>
        app(Intent.Dispatch(command))
        if model().document.draft.nonEmpty then
          app(
            Intent.Navigate(
              eyes4s.studio.app.nav
                .Location(Perspective.Analysis, AppModel.draftTrail(model().document))
            )
          )
      }
  }
  def sync(value: AppModel, editable: Boolean): Unit = if !disposed then
    lock = !editable
    val (next, request) = panel.sync(value)
    val changed         = next.target != panel.target
    panel = next
    render(rebuild = changed)
    perform(request)
  private def render(rebuild: Boolean): Unit =
    node.setVisible(shown && panel.target.nonEmpty)
    node.setManaged(node.isVisible)
    descriptor.node.setDisable(lock || panel.form.isEmpty)
    scales.node.setDisable(lock || panel.form.isEmpty)
    panel.form.zip(panel.values).foreach { (form, values) =>
      scaleId = form.fields.find(_.id.value == "scales").map(_.id)
      scalesRaw = scaleId.fold[RawValue](RawValue.Absent)(values.get)
      val baseline = AnalysisSelection
        .selected(model())
        .toVector
        .flatMap(_.seedRecipe.scales.values.map(_.degrees))
        .toSet
      val chips = scales.show(scalesRaw, baseline, panel.result)
      if rebuild || rendered.isEmpty then
        descriptor.show(form, values, if chips then Set("scales") else Set.empty)
        rendered = Some(form)
        renderedTarget = panel.target
    }
    if panel.target.isEmpty || (panel.form.isEmpty && rebuild) then
      descriptor.node.getChildren.clear()
      rendered = None
      sentence.getChildren.clear()
    panel.result.foreach { served =>
      sentence.getChildren.setAll(served.sentence.map { token =>
        val text = Text(RecipeText(T.Served, token.text))
        text.getStyleClass.add("t12")
        token.role.foreach(role => text.getStyleClass.add(s"recipe-token-${role.toLowerCase}"))
        text
      }*)
      sentence.setAccessibleText(served.sentence.map(_.text).mkString)
      conventions.setText(served.conventions.map(s => RecipeText(T.Served, s)).mkString(" · "))
    }
    val message =
      if lock then RecipeText(T.Locked)
      else if panel.pending then
        RecipeText(if panel.form.isEmpty then T.Loading else T.Checking)
      else
        panel.error.getOrElse(panel.result.fold(RecipeText(T.Loading)) { result =>
          if result.problems.nonEmpty then
            result.problems.map(p => RecipeText(T.Served, p.message)).mkString("\n")
          else RecipeText(if panel.command(model()).nonEmpty then T.Unapplied else T.Applied)
        })
    problem.setText(message)
    apply.setDisable(lock || panel.command(model()).isEmpty)
    observers.foreach(_(editingReason))
  def focusStops: Vector[FocusStop] =
    if !node.isVisible then Vector.empty
    else
      scales.focusStops ++ descriptor.focusStops ++
        Option.when(!apply.isDisabled)(FocusStop(A11yRole.Button, apply.getAccessibleText))
  def dispose(): Unit = disposed = true
