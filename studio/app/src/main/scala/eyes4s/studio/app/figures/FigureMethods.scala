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

import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.core.backend.ReportView
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.FigureId
import eyes4s.studio.core.figures.{FigureSource, MethodsFacts}

/** What the author does in the methods.md pane (ticket S9.4). */
enum MethodsIntent derives CanEqual:
  /** The run's admission and query facts arrived, or why they did not. */
  case FactsRead(run: RunId, answer: Either[String, MethodsFacts])

  /** The author's text, whole. */
  case Edit(text: String)
  case Regenerate
  case ShowDiff

  /** After a regeneration found new text: keep the author's edits (the diff
    * is then against the new text), or take the new text and drop them.
    */
  case KeepEdits
  case UseGenerated

/** The methods.md pane and its "Diff vs generated" pane: the heading, the
  * text (or why there is none yet), the diff, and the choice a regeneration
  * against edits waits on.
  */
final case class MethodsVM(
    heading: String,
    text: Either[String, String],
    diffCaption: String,
    diff: Vector[DiffLine],
    showDiff: String,
    regenerate: String,
    choice: Option[(String, String)],
    status: Option[String]
) derives CanEqual

/** A failed generation attempt follows current availability; an author
  * action's message remains until another action replaces it.
  */
private[figures] enum MethodsStatus derives CanEqual:
  case GenerationUnavailable
  case Message(text: String)

/** The methods text of every figure: the facts read per run, the author's
  * drafts per figure, and the pane's last message.
  */
final case class FigureMethods private (
    facts: Map[RunId, Either[String, MethodsFacts]],
    drafts: Map[FigureId, MethodsDraft],
    private[figures] val status: Option[MethodsStatus]
) derives CanEqual

object FigureMethods:
  val empty: FigureMethods = FigureMethods(Map.empty, Map.empty, None)

  /** The figure's methods as generated now, or why they cannot be. */
  def generated(
      m: FigureMethods,
      source: FigureSource,
      summary: Option[SummaryAnswer],
      report: Option[Either[String, ReportView]]
  ): Either[String, GeneratedMethods] =
    val run = source.run.id
    for
      result <- summary match
        case None                            => Left(MethodsCopy.reading(run))
        case Some(SummaryAnswer.Answered(r)) => Right(r)
        case Some(SummaryAnswer.Refused(e))  => Left(e.message)
        case Some(SummaryAnswer.Failed(why)) => Left(why)
      facts     <- m.facts.getOrElse(run, Left(MethodsCopy.reading(run)))
      evaluated <- report.getOrElse(Left(MethodsCopy.reading(run)))
      generated <- MethodsText.generate(source, result, facts, evaluated).left.map(_.message)
    yield generated

  /** Which pane to bring forward after an intent. */
  enum Show derives CanEqual:
    case Text, Diff

  /** The next state, and the pane to bring forward, if any: the diff when
    * asked for or when a regeneration waits on the author, the text when the
    * author takes the generated text.
    */
  def update(
      m: FigureMethods,
      source: Option[FigureSource],
      summary: Option[SummaryAnswer],
      report: Option[Either[String, ReportView]],
      intent: MethodsIntent
  ): (FigureMethods, Option[Show]) =
    import MethodsIntent.*
    def current = source.toRight(MethodsCopy.NoFigure).flatMap(generated(m, _, summary, report))
    def draft   = source.flatMap(s => m.drafts.get(s.figure.id))
    def set(d: Option[MethodsDraft]) = source.fold(m)(s =>
      m.copy(drafts = d.fold(m.drafts - s.figure.id)(m.drafts.updated(s.figure.id, _)))
    )
    intent match
      case FactsRead(run, answer) => (m.copy(facts = m.facts.updated(run, answer)), None)
      case ShowDiff               => (m, Some(Show.Diff))
      case Edit(text)             =>
        val next = (draft, current) match
          case (Some(d), _) =>
            set(Some(d.copy(edited = text)).filter(d => d.isEdited || d.pending.isDefined))
              .copy(status = None)
          case (None, Right(g)) if text != g.text =>
            set(Some(MethodsDraft(g.text, text, None))).copy(status = None)
          // Nothing to edit until the text is generated.
          case _ => m
        (next, None)
      case Regenerate =>
        (draft, current) match
          case (_, Left(_)) =>
            (m.copy(status = Some(MethodsStatus.GenerationUnavailable)), None)
          case (Some(d), Right(g)) if d.isEdited =>
            if g.text == d.base then
              (
                set(Some(d.copy(pending = None)))
                  .copy(status = Some(MethodsStatus.Message(MethodsCopy.Unchanged))),
                None
              )
            else
              (
                set(Some(d.copy(pending = Some(g.text))))
                  .copy(status = Some(MethodsStatus.Message(MethodsCopy.Changed))),
                Some(Show.Diff)
              )
          case (_, Right(g)) =>
            (
              set(None).copy(status = Some(MethodsStatus.Message(MethodsCopy.regenerated(g)))),
              None
            )
      case KeepEdits =>
        draft.flatMap(d => d.pending.map(p => d.copy(base = p, pending = None))) match
          case Some(d) =>
            (
              set(Some(d).filter(_.isEdited))
                .copy(status = Some(MethodsStatus.Message(MethodsCopy.Kept))),
              None
            )
          case None => (m, None)
      case UseGenerated =>
        draft.flatMap(_.pending) match
          case Some(_) =>
            (
              set(None).copy(status = Some(MethodsStatus.Message(MethodsCopy.Replaced))),
              Some(Show.Text)
            )
          case None => (m, None)

  def view(
      m: FigureMethods,
      source: FigureSource,
      summary: Option[SummaryAnswer],
      report: Option[Either[String, ReportView]]
  ): MethodsVM =
    val now             = generated(m, source, summary, report)
    val draft           = m.drafts.get(source.figure.id)
    val text            = draft.map(d => Right(d.edited)).getOrElse(now.map(_.text))
    val (caption, diff) = draft match
      case Some(MethodsDraft(_, edited, Some(pending))) =>
        (MethodsCopy.PendingCaption, TextDiff(edited, pending))
      case Some(d) => (MethodsCopy.EditsCaption, TextDiff(d.base, d.edited))
      case None    =>
        (MethodsCopy.EditsCaption, now.toOption.toVector.flatMap(g => TextDiff(g.text, g.text)))
    val edits = draft.fold(0)(d => TextDiff.changed(TextDiff(d.base, d.edited)))
    val stale = draft.exists(d => d.pending.isEmpty && now.exists(_.text != d.base))
    MethodsVM(
      MethodsCopy.heading(source, edits, stale),
      text,
      caption,
      diff,
      "Show diff",
      "Regenerate",
      draft.flatMap(_.pending).map(_ => ("Keep my edits", "Use the generated text")),
      m.status.flatMap {
        case MethodsStatus.GenerationUnavailable => now.left.toOption
        case MethodsStatus.Message(text)         => Some(text)
      }
    )

object MethodsCopy:
  val NoFigure: String = "There is no figure to write methods for."

  def reading(run: RunId): String = s"Reading ${run.label}'s results for the methods…"

  /** "Generated from run 7 and reporting spec “By retrieval response” · 1
    * sentence edited by you".
    */
  def heading(source: FigureSource, edits: Int, stale: Boolean): String =
    val base =
      s"Generated from ${source.run.id.label} and reporting spec “${source.reporting.name}”"
    val edited =
      if edits == 0 then ""
      else if edits == 1 then " · 1 sentence edited by you"
      else s" · $edits sentences edited by you"
    val since = if stale then " · regenerate to compare with new generated text" else ""
    base + edited + since

  def regenerated(g: GeneratedMethods): String =
    s"Regenerated from ${g.run.label} and reporting spec “${g.reporting}”."

  val Unchanged: String = "The generated text has not changed; your edits are kept."
  val Changed: String   =
    "The generated text has changed. Your edits are kept until you choose: keep them, or " +
      "use the generated text."
  val Kept: String     = "Your edits are kept; the diff is now against the new generated text."
  val Replaced: String = "Your edits were replaced by the generated text."

  val EditsCaption: String   = "Generated text → your text"
  val PendingCaption: String = "Your text → newly generated text"
