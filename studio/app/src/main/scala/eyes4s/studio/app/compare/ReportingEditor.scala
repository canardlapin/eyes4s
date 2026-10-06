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

package eyes4s.studio.app.compare

import eyes4s.studio.app.Intent
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{Format, ReportingText, ReportingTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.{ReportRole, ReportView, ResultSummary, RunId}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{
  Covariate,
  MinimumPerGroup,
  Perspective,
  ReportingContrast,
  ReportingFilter,
  ReportingId,
  ReportingSpec,
  ReportingWeight,
  Share,
  StudioDocument,
  ValueSet,
  WeightChoice
}
import eyes4s.studio.core.selection.{DesignCount, QueryCount, ScaleIndex, StudioRef}

/** An edit of the reporting spec Compare shows, or a change of spec. */
enum ReportingIntent derives CanEqual:
  case GroupBy(covariate: Option[Covariate])

  /** The board's ">25% of duration outside the window" filter, on or off. */
  case OutsideFilter(on: Boolean)

  /** Keep only the queries whose `attribute` is one of `values` (a
    * hit-only filter), or drop that attribute's filter when `values` is
    * empty.
    */
  case Keep(attribute: Covariate, values: Vector[String])

  case Minimum(on: Boolean)
  case Weight(weighting: ReportingWeight)

  /** Explicit ordered subtraction, or no level contrast. */
  case SetContrast(contrast: Option[ReportingContrast])

  /** A form submission; parsed before it becomes a document command. */
  case ContrastOperands(minuend: String, subtrahend: String)

  /** Show another saved spec. */
  case Choose(reporting: ReportingId)

  /** Save as…: open the name field, edit it, save a copy under it, or cancel. */
  case OpenSaveAs
  case Name(text: String)
  case ConfirmSaveAs
  case CancelSaveAs

/** One choice of a control and whether it is the one chosen. */
final case class ReportingChoice[A](value: A, label: String, chosen: Boolean) derives CanEqual

/** A text that states numbers, and the refs they trace to. */
final case class ReportingLine(text: String, refs: Vector[StudioRef]) derives CanEqual

/** A filter toggle: its label, whether it is on, and its note. */
final case class ReportingToggle(label: String, on: Boolean, note: ReportingLine)
    derives CanEqual

/** One saved spec: its name, its run, view and figure bindings, and the
  * intent that shows it.
  */
final case class SavedSpecVM(
    id: ReportingId,
    name: String,
    detail: String,
    current: Boolean,
    choose: Intent
) derives CanEqual

/** The Save as… name field while it is open. */
final case class SaveAsVM(name: String, label: String, save: String, cancel: String)
    derives CanEqual

/** The explicitly ordered contrast form. Blank fields do not invent a
  * direction for a legacy grouping; Apply validates both entered operands.
  */
final case class ReportingContrastVM(
    title: String,
    enabled: Boolean,
    minuendLabel: String,
    subtrahendLabel: String,
    minuend: String,
    subtrahend: String,
    apply: String,
    clear: String
) derives CanEqual

/** Compare's reporting editor (Results.dc.html, reporting inspector). */
final case class ReportingEditorVM(
    status: Option[String],
    title: String,
    kind: String,
    reuses: ReportingLine,
    groupLabel: String,
    groups: Vector[ReportingChoice[Option[Covariate]]],
    groupValues: String,
    filterTitle: String,
    contributing: ReportingLine,
    outside: ReportingToggle,
    keeps: Vector[String],
    minimum: ReportingToggle,
    weightTitle: String,
    weightUnit: String,
    weights: Vector[ReportingChoice[ReportingWeight]],
    estimandTitle: String,
    estimand: String,
    savedTitle: String,
    saveAs: String,
    saving: Option[SaveAsVM],
    saved: Vector[SavedSpecVM],
    error: Option[String],
    contrast: ReportingContrastVM
) derives CanEqual

/** Compare's reporting editor (ticket S8.7; Results.dc.html, reporting): how
  * the shown run's query scores are grouped, filtered and weighted. Every
  * edit is one `PutReporting` command ("Reporting · no rerun"): it changes
  * the document's spec and nothing else, so no job starts and the run's pair
  * scores and control sets stand as computed. The run's numbers come from
  * its served summary; the minimum's preview names the served
  * participant-group cells it would drop. The only state is the Save as…
  * field. Pure; the host dispatches the intents it returns.
  */
final case class ReportingEditor(saving: Option[String], error: Option[String]) derives CanEqual

object ReportingEditor:
  import ReportingTextId.*

  val empty: ReportingEditor = ReportingEditor(None, None)

  /** The board's outside-window threshold: a quarter of fixation duration. */
  val OutsideShare: Double = 0.25

  /** The board's minimum, used when the spec has none of its own. */
  val DefaultMinimum: Int = 3

  /** The spec Compare shows, from the document. */
  def spec(doc: StudioDocument, reporting: Option[ReportingId]): Option[ReportingSpec] =
    reporting.flatMap(id => doc.reporting.find(_.id == id))

  /** Applies a user action to the spec `reporting` names: the new editor
    * state and the intents for the app (a `PutReporting`, or a navigation to
    * another spec's summary).
    */
  def update(
      s: ReportingEditor,
      doc: StudioDocument,
      reporting: Option[ReportingId],
      intent: ReportingIntent
  ): (ReportingEditor, Vector[Intent]) =
    def put(
        edit: ReportingSpec => Either[eyes4s.studio.core.document.DocumentError, ReportingSpec]
    ) =
      spec(doc, reporting) match
        case None    => (s, Vector.empty)
        case Some(r) =>
          edit(r) match
            case Left(e)                  => (s.copy(error = Some(e.message)), Vector.empty)
            case Right(next) if next == r => (s.copy(error = None), Vector.empty)
            case Right(next)              =>
              (s.copy(error = None), Vector(Intent.Dispatch(Command.PutReporting(next))))
    def rebuilt(
        r: ReportingSpec,
        id: Option[ReportingId] = None,
        name: Option[String] = None,
        groupBy: Option[Option[Covariate]] = None,
        filters: Option[Vector[ReportingFilter]] = None,
        minimum: Option[Option[MinimumPerGroup]] = None,
        weighting: Option[ReportingWeight] = None,
        contrast: Option[Option[ReportingContrast]] = None
    ) =
      ReportingSpec.of(
        id.getOrElse(r.id),
        name.getOrElse(r.name),
        groupBy.getOrElse(r.groupBy),
        filters.getOrElse(r.filters),
        minimum.getOrElse(r.minimumPerGroup),
        weighting.getOrElse(r.weighting),
        contrast.getOrElse(r.contrast)
      )
    intent match
      case ReportingIntent.GroupBy(c) =>
        put(r => rebuilt(r, groupBy = Some(c), contrast = Option.when(c != r.groupBy)(None)))
      case ReportingIntent.SetContrast(c) => put(r => rebuilt(r, contrast = Some(c)))
      case ReportingIntent.ContrastOperands(minuend, subtrahend) =>
        put(r =>
          ReportingContrast
            .of(minuend, subtrahend)
            .flatMap(c => rebuilt(r, contrast = Some(Some(c))))
        )
      case ReportingIntent.OutsideFilter(on) =>
        put { r =>
          val others = r.filters.filter {
            case ReportingFilter.OutsideWindowAtMost(_) => false
            case _                                      => true
          }
          if !on then rebuilt(r, filters = Some(others))
          else
            Share
              .of(OutsideShare)
              .flatMap(sh =>
                rebuilt(r, filters = Some(others :+ ReportingFilter.OutsideWindowAtMost(sh)))
              )
        }
      case ReportingIntent.Keep(attribute, values) =>
        put { r =>
          val others = r.filters.filter {
            case ReportingFilter.Keep(a, _) => a != attribute
            case _                          => true
          }
          if values.isEmpty then rebuilt(r, filters = Some(others))
          else
            ValueSet
              .of(attribute, values)
              .flatMap(vs =>
                rebuilt(r, filters = Some(others :+ ReportingFilter.Keep(attribute, vs)))
              )
        }
      case ReportingIntent.Minimum(on) =>
        put { r =>
          if !on then rebuilt(r, minimum = Some(None))
          else if r.minimumPerGroup.isDefined then Right(r)
          else
            MinimumPerGroup.of(DefaultMinimum).flatMap(m => rebuilt(r, minimum = Some(Some(m))))
        }
      case ReportingIntent.Weight(w)  => put(r => rebuilt(r, weighting = Some(w)))
      case ReportingIntent.Choose(id) =>
        if reporting.contains(id) || !doc.reporting.exists(_.id == id) then (s, Vector.empty)
        else (s.copy(saving = None), Vector(show(id)))
      case ReportingIntent.OpenSaveAs =>
        spec(doc, reporting).fold((s, Vector.empty[Intent]))(r =>
          (s.copy(saving = Some(ReportingText(DefaultCopyName, r.name))), Vector.empty)
        )
      case ReportingIntent.Name(text) =>
        (s.saving.fold(s)(_ => s.copy(saving = Some(text))), Vector.empty)
      case ReportingIntent.CancelSaveAs  => (s.copy(saving = None, error = None), Vector.empty)
      case ReportingIntent.ConfirmSaveAs =>
        (spec(doc, reporting), s.saving) match
          case (Some(r), Some(name)) =>
            val copy =
              if name.trim.isEmpty then Left(ReportingText(BlankName))
              else
                (for
                  id   <- freshId(doc, name)
                  next <- rebuilt(r, id = Some(id), name = Some(name.trim))
                yield next).left.map(_.message)
            copy match
              case Left(why)   => (s.copy(error = Some(why)), Vector.empty)
              case Right(next) =>
                (
                  ReportingEditor.empty,
                  Vector(Intent.Dispatch(Command.PutReporting(next)), show(next.id))
                )
          case _ => (s, Vector.empty)

  /** Compare's summary of spec `id`. */
  def show(id: ReportingId): Intent =
    Intent.Navigate(Location(Perspective.Compare, Vector(Place.Summary(id))))

  /** An id for a new spec named `name`, distinct from the document's: its
    * ASCII letters and digits, lower case, runs of anything else one `-`.
    */
  private def freshId(
      doc: StudioDocument,
      name: String
  ): Either[eyes4s.studio.core.document.DocumentError, ReportingId] =
    val dashed = name.trim.toLowerCase
      .map(c => if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then c else '-')
      .replaceAll("-+", "-")
      .stripPrefix("-")
      .stripSuffix("-")
    val base  = if dashed.isEmpty then "spec" else dashed
    val taken = doc.reporting.map(_.id.value).toSet
    val slug  = Iterator
      .from(1)
      .map(i => if i == 1 then base else s"$base-$i")
      .find(c => !taken.contains(c))
      .getOrElse(base)
    ReportingId.of(slug)

  /** The controls inside the editor's own focus stop, in Tab order. */
  def focusStops(vm: ReportingEditorVM): Vector[FocusStop] =
    if vm.status.isDefined then Vector.empty
    else
      def chosen[A](label: String, cs: Vector[ReportingChoice[A]]) =
        cs.find(_.chosen)
          .orElse(cs.headOption)
          .map(c => FocusStop(A11yRole.RadioButton, s"$label: ${c.label}"))
      chosen(vm.groupLabel, vm.groups).toVector ++
        (if vm.contrast.enabled then
           Vector(
             FocusStop(A11yRole.TextField, vm.contrast.minuendLabel),
             FocusStop(A11yRole.TextField, vm.contrast.subtrahendLabel),
             FocusStop(A11yRole.Button, vm.contrast.apply),
             FocusStop(A11yRole.Button, vm.contrast.clear)
           )
         else Vector.empty) ++
        Vector(
          FocusStop(A11yRole.CheckBox, vm.outside.label),
          FocusStop(A11yRole.CheckBox, vm.minimum.label)
        ) ++
        chosen(vm.weightTitle, vm.weights) ++
        Vector(FocusStop(A11yRole.Button, vm.saveAs)) ++
        vm.saving.toVector.flatMap(sv =>
          Vector(
            FocusStop(A11yRole.TextField, sv.label),
            FocusStop(A11yRole.Button, sv.save),
            FocusStop(A11yRole.Button, sv.cancel)
          )
        ) ++
        vm.saved
          .filterNot(_.current)
          .map(v => FocusStop(A11yRole.Button, s"${v.name}, ${v.detail}"))

  /** The editor's view-model: the spec `reporting` names in `doc`, read
    * against the shown run's served summary at `scale`.
    */
  def vm(
      s: ReportingEditor,
      doc: StudioDocument,
      reporting: Option[ReportingId],
      run: Option[RunId],
      summary: Option[ResultSummary],
      scale: Option[ScaleIndex],
      report: Option[ReportView] = None
  ): ReportingEditorVM =
    val r        = spec(doc, reporting)
    val shownRun = run.orElse(summary.map(_.run))
    val result   = summary.filter(sm => shownRun.contains(sm.run))
    val recipe   =
      shownRun.flatMap(doc.run).flatMap(rr => doc.analysis(rr.analysis)).map(_.recipe)
    val focal    = recipe.fold("")(_.phases.focal.label.toLowerCase)
    val runLabel = shownRun.fold("")(_.label)
    val reuses   =
      result.fold(ReportingLine(ReportingText(ReusesUnknown, runLabel), Vector.empty))(sm =>
        ReportingLine(
          ReportingText(Reuses, sm.run.label, Format.count(sm.pairRows)),
          Vector(StudioRef.DesignTally(sm.revision, DesignCount.EligiblePairs))
        )
      )
    // Only the saved grouping and native report's levels are shown. A
    // summary carries no scientific grouping or scale-specific estimates.
    val evaluated = report.filter(v =>
      shownRun.contains(v.run) &&
        reporting.contains(v.reporting) && scale.exists(_.value == v.scale)
    )
    val covariates = r.flatMap(_.groupBy).toVector
    val groupBy    = r.flatMap(_.groupBy)
    val groups     = covariates.map(c =>
      ReportingChoice(Option(c), ReportingText(GroupOption, c.label), groupBy.contains(c))
    ) :+ ReportingChoice(Option.empty[Covariate], ReportingText(NoGrouping), groupBy.isEmpty)
    val groupValues = evaluated.toVector
      .flatMap(
        _.cells
          .collect {
            case c if c.role == ReportRole.Difference => c.group.map(_.label)
          }
          .flatten
      )
      .distinct
      .mkString(" · ")
    val contributing = result.fold(
      ReportingLine(ReportingText(AllContributingUnknown, focal), Vector.empty)
    )(sm =>
      ReportingLine(
        ReportingText(AllContributing, focal, Format.count(sm.contrasts.contributing)),
        Vector(StudioRef.QueryTally(sm.run, QueryCount.Contributing))
      )
    )
    val outsideOn = r.exists(_.filters.exists {
      case ReportingFilter.OutsideWindowAtMost(_) => true
      case _                                      => false
    })
    val share = r
      .flatMap(_.filters.collectFirst { case ReportingFilter.OutsideWindowAtMost(sh) =>
        sh.value
      })
      .getOrElse(OutsideShare)
    val outside = ReportingToggle(
      ReportingText(Outside, Format.percent(share)),
      outsideOn,
      ReportingLine(ReportingText(if outsideOn then OutsideOn else OutsideOff), Vector.empty)
    )
    val keeps = r.toVector.flatMap(_.filters.collect { case ReportingFilter.Keep(a, vs) =>
      ReportingText(KeepFilter, a.label, vs.values.mkString(", "))
    })
    val minimumN  = r.flatMap(_.minimumPerGroup).fold(DefaultMinimum)(_.queries)
    val minimumOn = r.exists(_.minimumPerGroup.isDefined)
    val minimum   = ReportingToggle(
      ReportingText(Minimum, minimumN.toString),
      minimumOn,
      dropped(evaluated, minimumN, minimumOn)
    )
    val weighting = r.fold(ReportingWeight.ParticipantMeans)(_.weighting)
    val weights   = Vector(
      ReportingChoice(
        ReportingWeight.ParticipantMeans,
        ReportingText(EqualParticipants),
        weighting == ReportingWeight.ParticipantMeans
      ),
      ReportingChoice(
        ReportingWeight.PooledQueries,
        ReportingText(PooledQueries),
        weighting == ReportingWeight.PooledQueries
      )
    )
    val estimand = recipe.fold(ReportingText(Confound)) { rc =>
      val what = rc.weighting match
        case WeightChoice.Duration => ReportingText(EstimandDuration)
        case WeightChoice.Uniform  => ReportingText(EstimandCount)
      s"$what ${ReportingText(Confound)}"
    }
    val saved = doc.reporting.map { sp =>
      val figures = doc.figures.filter(_.reporting == sp.id).map(_.id.label)
      val used    =
        if figures.isEmpty then ReportingText(NotUsed)
        else ReportingText(UsedBy, figures.mkString(", "))
      val current = reporting.contains(sp.id)
      SavedSpecVM(
        sp.id,
        sp.name,
        if current then ReportingText(SavedCurrent, runLabel, used)
        else ReportingText(SavedOther, runLabel, used),
        current,
        show(sp.id)
      )
    }
    ReportingEditorVM(
      Option.when(r.isEmpty)(ReportingText(NoSpec)),
      r.fold("")(_.name),
      ReportingText(Kind),
      reuses,
      ReportingText(GroupBy),
      groups,
      groupValues,
      ReportingText(FilterTitle),
      contributing,
      outside,
      keeps,
      minimum,
      ReportingText(WeightTitle),
      ReportingText(WeightUnit),
      weights,
      ReportingText(EstimandTitle),
      estimand,
      ReportingText(SavedTitle),
      ReportingText(SaveAs),
      s.saving.map(n =>
        SaveAsVM(n, ReportingText(SaveAsName), ReportingText(Save), ReportingText(Cancel))
      ),
      saved,
      s.error,
      ReportingContrastVM(
        ReportingText(ContrastTitle),
        r.exists(_.groupBy.nonEmpty),
        ReportingText(ContrastMinuend),
        ReportingText(ContrastSubtrahend),
        r.flatMap(_.contrast).fold("")(_.minuend),
        r.flatMap(_.contrast).fold("")(_.subtrahend),
        ReportingText(ContrastApply),
        ReportingText(ContrastClear)
      )
    )

  /** Exclusions are the library report's findings. An unevaluated edit,
    * or a minimum that is off, never fabricates a numerical preview.
    */
  private def dropped(report: Option[ReportView], n: Int, on: Boolean): ReportingLine =
    if !on then ReportingLine(ReportingText(MinimumOff), Vector.empty)
    else
      report match
        case None => ReportingLine(ReportingText(MinimumAfterEvaluation), Vector.empty)
        case Some(view) if view.dropped.isEmpty =>
          ReportingLine(ReportingText(MinimumNone, n.toString), Vector.empty)
        case Some(view) =>
          val cells  = view.dropped
          val counts = cells
            .flatMap(_.group)
            .distinct
            .map { group =>
              ReportingText(
                CellGroup,
                cells.count(_.group.contains(group)).toString,
                group.label
              )
            }
            .mkString(", ")
          val named = cells
            .map(c => ReportingText(Cell, c.participant, c.queries.toString))
            .mkString(", ")
          ReportingLine(ReportingText(MinimumDrops, counts, named), cells.map(_.ref))
