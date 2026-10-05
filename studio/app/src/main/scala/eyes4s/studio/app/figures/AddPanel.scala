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

package eyes4s.studio.app.figures

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.compare.{PanelFocus, TrialPanels}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*

/** A panel a figure can gain (bead bd-01M44PBFHM4CWTXJAKQYMVV7RY): one of
  * the five templates of the Figures board ([[PanelTemplate]]).
  */
enum NewPanel derives CanEqual:
  /** A: the encoding trial of Compare's pair (its reference). */
  case EncodingGaze

  /** B: the retrieval trial of Compare's query. */
  case RetrievalGaze

  /** C: Compare's query's density maps, at its scale. */
  case DensityMaps

  /** D: every participant's D by reporting group, at one scale. */
  case ParticipantD

  /** E: the grand means of D at every scale of the run. */
  case ScaleProfile

  def label: String = this match
    case EncodingGaze  => "Encoding gaze"
    case RetrievalGaze => "Retrieval gaze"
    case DensityMaps   => "Density maps"
    case ParticipantD  => "Participant D"
    case ScaleProfile  => "Scale profile"

/** One choice of the Add panel control: enabled, or why not. */
final case class AddPanelChoiceVM(
    kind: NewPanel,
    label: String,
    enabled: Boolean,
    why: Option[String]
) derives CanEqual:
  /** As the menu writes it: the label, and why when it is disabled. */
  def text: String = why.fold(label)(w => s"$label · $w")

/** The Add panel control of a shown figure. */
final case class AddPanelVM(label: String, choices: Vector[AddPanelChoiceVM]) derives CanEqual

/** What Add panel adds. A panel chooses only its scale and its trial
  * selection (DESIGN_SPEC section 12), and they come from where the user
  * is, never from a computed value:
  *
  *   - the trials and the scale of the gaze and density panels are those of
  *     Compare's trail, its last query or pair ([[TrialPanels.focusOf]]);
  *   - a one-scale panel takes the trail's scale, else the selected panel's,
  *     else the first scale of the figure's run;
  *   - the letter is the first one the figure does not use, A to Z, and the
  *     panel goes last.
  *
  * The panel is added by the document's own `AddPanel` command, so it is
  * undoable like any other edit.
  */
object AddPanel:

  /** The letters a panel may take, in the order Add panel uses them. */
  val Letters: Vector[String] = ('A' to 'Z').toVector.map(_.toString)

  /** The control's label. */
  val Label: String = "Add panel"

  /** The control for `figure`, whose selected panel is `selected`. */
  def view(model: AppModel, figure: FigureSpec, selected: Option[PanelLetter]): AddPanelVM =
    AddPanelVM(
      Label,
      NewPanel.values.toVector.map { kind =>
        spec(model, figure, selected, kind) match
          case Right(_)  => AddPanelChoiceVM(kind, kind.label, true, None)
          case Left(why) => AddPanelChoiceVM(kind, kind.label, false, Some(why))
      }
    )

  /** The command adding a `kind` panel to `figure`, or why there is none. */
  def command(
      model: AppModel,
      figure: FigureSpec,
      selected: Option[PanelLetter],
      kind: NewPanel
  ): Either[String, Command.AddPanel] =
    spec(model, figure, selected, kind).map(Command.AddPanel(figure.id, figure.panels.size, _))

  /** The panel a `kind` choice adds to `figure`, or why it cannot. */
  def spec(
      model: AppModel,
      figure: FigureSpec,
      selected: Option[PanelLetter],
      kind: NewPanel
  ): Either[String, PanelSpec] =
    val focus = TrialPanels.focusOf(model)
    for
      letter <- nextLetter(figure)
      panel  <- kind match
        case NewPanel.EncodingGaze =>
          focus
            .flatMap(_.reference)
            .map((_, reference) =>
              PanelSpec(
                letter,
                kind.label,
                PanelScale.Unscaled,
                PanelSelection.Trial(reference)
              )
            )
            .toRight(AddPanelText.NoPair)
        case NewPanel.RetrievalGaze =>
          focus
            .map(f =>
              PanelSpec(letter, kind.label, PanelScale.Unscaled, PanelSelection.Trial(f.query))
            )
            .toRight(AddPanelText.NoQuery)
        case NewPanel.DensityMaps =>
          for
            f     <- focus.toRight(AddPanelText.NoQuery)
            sigma <- sigmaOf(model, f).toRight(AddPanelText.noScale(f))
          yield PanelSpec(
            letter,
            kind.label,
            PanelScale.At(sigma),
            PanelSelection.QueryWithReferences(f.query)
          )
        case NewPanel.ParticipantD =>
          oneScale(model, figure, selected, focus)
            .map(sigma =>
              PanelSpec(
                letter,
                grouped(model, figure, kind.label),
                PanelScale.At(sigma),
                PanelSelection.AllQueries
              )
            )
        case NewPanel.ScaleProfile =>
          Right(
            PanelSpec(
              letter,
              grouped(model, figure, kind.label),
              PanelScale.AllScales,
              PanelSelection.AllQueries
            )
          )
    yield panel

  private def nextLetter(figure: FigureSpec): Either[String, PanelLetter] =
    val used = figure.panels.map(_.letter.value).toSet
    Letters
      .find(l => !used.contains(l))
      .toRight(AddPanelText.full(figure.id))
      .flatMap(l => PanelLetter.of(l).left.map(_.message))

  // The sigma of the trail's scale index, in the trail's own run.
  private def sigmaOf(model: AppModel, focus: PanelFocus): Option[Sigma] =
    for
      run   <- model.document.run(focus.run)
      spec  <- model.document.analysis(run.analysis)
      sigma <- spec.recipe.scales.values.lift(focus.scale.value)
    yield sigma

  private def oneScale(
      model: AppModel,
      figure: FigureSpec,
      selected: Option[PanelLetter],
      focus: Option[PanelFocus]
  ): Either[String, Sigma] =
    val fromPanel = selected
      .flatMap(l => figure.panels.find(_.letter == l))
      .collect { case PanelSpec(_, _, PanelScale.At(s), _) => s }
    val fromRun = for
      run   <- model.document.run(figure.run)
      spec  <- model.document.analysis(run.analysis)
      sigma <- spec.recipe.scales.values.headOption
    yield sigma
    focus
      .flatMap(sigmaOf(model, _))
      .orElse(fromPanel)
      .orElse(fromRun)
      .toRight(AddPanelText.noRunScale(figure))

  // "Participant D by response" when the figure's spec groups.
  private def grouped(model: AppModel, figure: FigureSpec, title: String): String =
    model.document.reporting
      .find(_.id == figure.reporting)
      .flatMap(_.groupBy)
      .fold(title)(c => s"$title by ${c.label}")

/** Add panel's English text. */
object AddPanelText:
  val NoQuery: String = "Open a query in Compare to add its panel."
  val NoPair: String  = "Open a pair in Compare to add its encoding trial."

  def full(figure: FigureId): String =
    s"${figure.label} already has a panel for every letter, A to Z."

  def noScale(focus: PanelFocus): String =
    s"${focus.run.label} has no scale ${focus.scale.value} in the project."

  def noRunScale(figure: FigureSpec): String =
    s"${figure.run.label}, which ${figure.id.label} is bound to, has no scales in the project."
