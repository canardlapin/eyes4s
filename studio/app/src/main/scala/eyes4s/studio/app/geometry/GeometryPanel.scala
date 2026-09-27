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

package eyes4s.studio.app.geometry

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{GeometryText, GeometryTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{AdmissionSummary, DatasetRevision, TrialKey}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.geometry.{CorrectionLedger, SourcePositions}
import eyes4s.studio.core.importing.{GeometryField, GeometryFields}
import eyes4s.studio.core.selection.StudioRef

/** Something the panel loads for the revision it shows. */
enum Loading[+A] derives CanEqual:
  /** There is nothing to load: no revision is shown. */
  case Idle
  case Waiting
  case Ready(value: A)

  /** The platform or the backend refused; `reason` names what failed. */
  case Failed(reason: String)

  def toOption: Option[A] = this match
    case Ready(a) => Some(a)
    case _        => None

/** How "Mark trial as wrong orientation…" corrects: a flip about the
  * screen's centre line (eyes4s `Correction.FlipX`, `FlipY`).
  */
enum OrientationFix derives CanEqual:
  case FlipX, FlipY

  def correction: CoordinateCorrection = this match
    case FlipX => CoordinateCorrection.FlipX
    case FlipY => CoordinateCorrection.FlipY

/** Which trials a marked correction covers. */
enum OrientationScope derives CanEqual:
  case ThisTrial, ThisParticipant

/** The open "Mark trial as wrong orientation…" form. */
final case class OrientationForm(trial: TrialKey, fix: OrientationFix, scope: OrientationScope)
    derives CanEqual:
  def target: Either[DocumentError, CorrectionTarget] = scope match
    case OrientationScope.ThisTrial       => Right(CorrectionTarget.Trial(trial))
    case OrientationScope.ThisParticipant =>
      ParticipantId.of(trial.participant).map(CorrectionTarget.Participant(_))

/** A change the panel makes to the shown revision ("Dataset · re-admit"). */
enum GeometryChange derives CanEqual:
  case SetGeometry(geometry: Geometry)
  case SetOffScreen(policy: OffScreenChoice)
  case AddRule(rule: CorrectionRule)
  case RemoveRule(index: Int)

/** Why a change was not made; nothing changed. Each case names its operands. */
enum GeometryRefusal derives CanEqual:
  /** The change leaves `dataset` as it is. */
  case NoChange(dataset: DatasetRevision)
  case NoRule(dataset: DatasetRevision, index: Int, rules: Int)

  /** The rule would cover `trial` together with rule `existing` (0-based):
    * eyes4s refuses an admission whose rules overlap.
    */
  case Overlaps(trial: TrialKey, existing: Int)
  case Invalid(error: DocumentError)

  def message: String = this match
    case NoChange(d)     => s"The change leaves ${d.label} as it is."
    case NoRule(d, i, n) => s"${d.label} has $n correction rules; there is no rule ${i + 1}."
    case Overlaps(trial, rule) =>
      s"the new rule and rule ${rule + 1} would both cover ${trial.label}; eyes4s " +
        "refuses overlapping rules."
    case Invalid(e) => e.message

/** Which records' positions a [[SourcePositions]] holds: the fixation
  * source's bytes read under a mapping. A revision that changes neither
  * keeps the positions already read.
  */
final case class PositionsKey(
    dataset: DatasetRevision,
    source: ByteDigest,
    mapping: ColumnMapping
) derives CanEqual

object PositionsKey:
  def of(spec: DatasetRevisionSpec): Option[PositionsKey] =
    spec.sources.fixations.map(s => PositionsKey(spec.id, s.bytes, spec.mapping))

  /** The same records: another revision may share them. */
  def same(a: PositionsKey, b: PositionsKey): Boolean =
    a.source == b.source && a.mapping == b.mapping

/** A user action or platform fact the panel's view dispatches. */
enum GeometryIntent derives CanEqual:
  case EditField(field: GeometryField, text: String)

  /** Enter, or focus leaving a field: apply the typed geometry. */
  case CommitFields
  case ChooseOffScreen(policy: OffScreenChoice)

  /** A thumbnail was chosen: the trial "Mark trial as wrong orientation…" acts on. */
  case MarkTrial(trial: TrialKey)
  case OpenOrientation
  case ChooseFix(fix: OrientationFix)
  case ChooseScope(scope: OrientationScope)
  case RecordOrientation
  case CancelOrientation
  case RemoveRule(index: Int)

  /** The platform read and parsed the fixation source for `key`. */
  case PositionsRead(key: PositionsKey, result: Either[String, SourcePositions])

  /** The backend's admission summary asked for `dataset`. */
  case CountsRead(dataset: DatasetRevision, result: Either[String, AdmissionSummary])

/** What the platform or the app must do after a panel update. */
enum GeometryEffect derives CanEqual:
  /** Dispatch an app intent: a document command, in order. */
  case App(intent: Intent)

  /** Read `spec`'s fixation source from the project and parse it under its
    * mapping (off the UI thread); the answer is [[GeometryIntent.PositionsRead]].
    */
  case ReadPositions(key: PositionsKey, spec: DatasetRevisionSpec)

  /** Ask the backend for `dataset`'s admission summary; the answer is
    * [[GeometryIntent.CountsRead]].
    */
  case RequestCounts(dataset: DatasetRevision)

/** The Data perspective's geometry panel (ticket S5.5; Data.dc.html,
  * geometry, tagged "Dataset · re-admit"). It shows the selected dataset
  * revision's declared geometry, checks placement against its own fixation
  * records, records corrections as admission rules, and sets the off-screen
  * policy.
  *
  * Every change is one "Dataset · re-admit" edit: a pending revision is
  * edited in place (a geometry edit is S5.2's single `ReviseDataset`, so one
  * undo restores it); a verifying or admitted revision is re-imported as a
  * new pending revision carrying the change, in one command ([[commands]]),
  * since admitted data never changes under a run, and the Data selection
  * follows it ([[follow]]).
  * Corrections are rules in the revision's admission choice: applying them
  * is eyes4s's, to a derived view, and never rewrites a source coordinate.
  */
final case class GeometryPanel(
    shown: Option[DatasetRevisionSpec],
    fields: GeometryFields,
    positionsKey: Option[PositionsKey],
    positions: Loading[SourcePositions],
    counts: Loading[AdmissionSummary],
    marked: Option[TrialKey],
    orientation: Option[OrientationForm],
    problem: Option[String]
) derives CanEqual:
  /** The trial and record the app selection names, for the thumbnails and
    * the worked example.
    */
  def focus(model: AppModel): (Option[TrialKey], Option[Int]) =
    GeometryPanel.focus(model)

object GeometryPanel:

  val empty: GeometryPanel =
    GeometryPanel(
      None,
      GeometryFields.blank,
      None,
      Loading.Idle,
      Loading.Idle,
      None,
      None,
      None
    )

  private val none: Vector[GeometryEffect] = Vector.empty

  /** The dataset revision the Data perspective has selected: the last
    * dataset of its trail that the document holds, else the latest revision.
    */
  def selected(model: AppModel): Option[DatasetRevisionSpec] =
    val document = model.document
    model.navigation
      .trail(Perspective.Data)
      .reverseIterator
      .collectFirst { case Place.Dataset(id) if document.dataset(id).isDefined => id }
      .flatMap(document.dataset)
      .orElse(document.datasets.lastOption)

  /** The trial and record the app's selection names, if any. */
  def focus(model: AppModel): (Option[TrialKey], Option[Int]) =
    model.selection.selected
      .collectFirst {
        case StudioRef.SourceRecord(t, _, SourceRole.Fixations, n) => (Some(t), Some(n.value))
        case StudioRef.SourceRecord(t, _, _, _)                    => (Some(t), None)
        case StudioRef.Fixation(t, _)                              => (Some(t), None)
        case StudioRef.Trial(t)                                    => (Some(t), None)
      }
      .getOrElse((None, None))

  /** Follow the model: a newly selected or changed revision resets the
    * typed fields and asks for what it lacks (its records, when its source
    * or mapping differs from those read, and its counts, when its id
    * differs).
    */
  def sync(panel: GeometryPanel, model: AppModel): (GeometryPanel, Vector[GeometryEffect]) =
    val now = selected(model)
    if now == panel.shown then (panel, none)
    else
      now match
        case None       => (empty, none)
        case Some(spec) =>
          val key        = PositionsKey.of(spec)
          val keepRecord = (panel.positionsKey, key) match
            case (Some(a), Some(b)) => PositionsKey.same(a, b)
            case _                  => false
          val (positions, read) =
            if keepRecord then (panel.positions, none)
            else
              key.fold[(Loading[SourcePositions], Vector[GeometryEffect])](
                (Loading.Failed("the revision has no fixation source"), none)
              )(k => (Loading.Waiting, Vector(GeometryEffect.ReadPositions(k, spec))))
          val sameId          = panel.shown.exists(_.id == spec.id)
          val (counts, count) =
            if sameId then (panel.counts, none)
            else (Loading.Waiting, Vector(GeometryEffect.RequestCounts(spec.id)))
          val sameTrials = keepRecord
          (
            GeometryPanel(
              Some(spec),
              GeometryFields.of(spec.geometry),
              key,
              positions,
              counts,
              panel.marked.filter(_ => sameTrials),
              panel.orientation.filter(_ => sameTrials),
              None
            ),
            read ++ count
          )

  /** The Elm-style update: pure; effects are data. */
  def update(
      panel: GeometryPanel,
      model: AppModel,
      intent: GeometryIntent
  ): (GeometryPanel, Vector[GeometryEffect]) =
    import GeometryIntent.*
    intent match
      case EditField(field, text) =>
        (panel.copy(fields = panel.fields.set(field, text), problem = None), none)
      case CommitFields =>
        panel.fields.parse match
          case Left(e)  => (panel.copy(problem = Some(e.message)), none)
          case Right(g) =>
            if panel.shown.exists(_.geometry == g) then (panel.copy(problem = None), none)
            else change(panel, model, GeometryChange.SetGeometry(g))
      case ChooseOffScreen(policy) => change(panel, model, GeometryChange.SetOffScreen(policy))
      case MarkTrial(trial)        =>
        (
          panel.copy(
            marked = Some(trial),
            orientation = panel.orientation.map(_.copy(trial = trial))
          ),
          none
        )
      case OpenOrientation =>
        panel.marked match
          case Some(trial) =>
            val form = OrientationForm(trial, OrientationFix.FlipX, OrientationScope.ThisTrial)
            (panel.copy(orientation = Some(form), problem = None), none)
          case None => (panel, none)
      case ChooseFix(fix) =>
        (panel.copy(orientation = panel.orientation.map(_.copy(fix = fix))), none)
      case ChooseScope(scope) =>
        (panel.copy(orientation = panel.orientation.map(_.copy(scope = scope))), none)
      case CancelOrientation => (panel.copy(orientation = None), none)
      case RecordOrientation =>
        panel.orientation match
          case None       => (panel, none)
          case Some(form) =>
            form.target match
              case Left(e)       => (panel.copy(problem = Some(e.message)), none)
              case Right(target) =>
                val (next, effects) = change(
                  panel,
                  model,
                  GeometryChange.AddRule(CorrectionRule(target, form.fix.correction))
                )
                if effects.isEmpty then (next, effects)
                else (next.copy(orientation = None), effects)
      case RemoveRule(index)          => change(panel, model, GeometryChange.RemoveRule(index))
      case PositionsRead(key, result) =>
        if !panel.positionsKey.contains(key) then (panel, none)
        else
          val loaded = result.fold(Loading.Failed(_), Loading.Ready(_))
          (panel.copy(positions = loaded), none)
      case CountsRead(dataset, result) =>
        panel.shown match
          case Some(spec) if dataset == spec.id || spec.parent.contains(dataset) =>
            result match
              case Right(summary) => (panel.copy(counts = Loading.Ready(summary)), none)
              // A draft the backend has not admitted: its parent's counts,
              // labelled as the parent's.
              case Left(_)
                  if dataset == spec.id && spec.parent.isDefined && !spec.decision.isAdmitted =>
                (panel, spec.parent.map(GeometryEffect.RequestCounts(_)).toVector)
              case Left(reason) => (panel.copy(counts = Loading.Failed(reason)), none)
          case _ => (panel, none)

  /** Make `c` to the model's live copy of the shown revision. */
  private def change(
      panel: GeometryPanel,
      model: AppModel,
      c: GeometryChange
  ): (GeometryPanel, Vector[GeometryEffect]) =
    val live = panel.shown.flatMap(s => model.document.dataset(s.id))
    live match
      case None       => (panel, none)
      case Some(spec) =>
        val checked = c match
          case GeometryChange.AddRule(rule) => overlap(spec, rule, panel.positions).toLeft(())
          case _                            => Right(())
        checked.flatMap(_ => commands(spec, c)) match
          case Left(GeometryRefusal.NoChange(_)) => (panel.copy(problem = None), none)
          case Left(refusal @ GeometryRefusal.Overlaps(_, _)) =>
            val text = GeometryText(GeometryTextId.RuleOverlaps, refusal.message)
            (panel.copy(problem = Some(text)), none)
          case Left(refusal) => (panel.copy(problem = Some(refusal.message)), none)
          case Right(cs)     =>
            (
              panel.copy(problem = None),
              cs.map(cmd => GeometryEffect.App(Intent.Dispatch(cmd)))
            )

  /** A rule overlapping an existing one on some trial of the source, as
    * eyes4s's own `correctionFor` finds it: eyes4s would refuse the
    * admission, so the panel does not record it.
    */
  private def overlap(
      spec: DatasetRevisionSpec,
      rule: CorrectionRule,
      positions: Loading[SourcePositions]
  ): Option[GeometryRefusal] =
    val choice = spec.admission.copy(corrections = spec.admission.corrections :+ rule)
    CorrectionLedger.policy(spec.id, choice) match
      case Left(_)       => None
      case Right(policy) =>
        val trials =
          positions.toOption.fold(Vector.empty[TrialKey])(_.trials) ++ (rule.target match
            case CorrectionTarget.Trial(k) => Vector(k)
            case _                         => Vector.empty)
        trials.iterator
          .map(t => t -> policy.correctionFor(t, _.participant))
          .collectFirst { case (t, Left((a, _))) => GeometryRefusal.Overlaps(t, a) }

  /** The document command that makes `c` to `spec`: a pending revision is
    * edited in place; a verifying or admitted one is re-imported as the next
    * revision carrying the changed geometry or admission choice. Every
    * change is one command, so one undo.
    */
  def commands(
      spec: DatasetRevisionSpec,
      c: GeometryChange
  ): Either[GeometryRefusal, Vector[Command]] =
    val id        = spec.id
    val rules     = spec.admission.corrections
    val unchanged = c match
      case GeometryChange.SetGeometry(g)  => g == spec.geometry
      case GeometryChange.SetOffScreen(p) => p == spec.admission.offScreen
      case GeometryChange.AddRule(r)      => rules.contains(r)
      case GeometryChange.RemoveRule(_)   => false
    val index = c match
      case GeometryChange.RemoveRule(i) if !rules.indices.contains(i) =>
        Left(GeometryRefusal.NoRule(id, i, rules.size))
      case _ => Right(())
    for
      _ <- Either.cond(!unchanged, (), GeometryRefusal.NoChange(id))
      _ <- index
    yield spec.decision match
      case AdmissionDecision.Pending =>
        c match
          // S5.2's one command for the four import fields: one undo.
          case GeometryChange.SetGeometry(g) =>
            Vector(Command.ReviseDataset(id, spec.mapping, spec.units, g, spec.attributes))
          case GeometryChange.SetOffScreen(p) => Vector(Command.SetOffScreenPolicy(id, p))
          case GeometryChange.AddRule(r) => Vector(Command.AddCorrection(id, rules.size, r))
          case GeometryChange.RemoveRule(i) => Vector(Command.RemoveCorrection(id, i))
      case AdmissionDecision.Verifying(_) | AdmissionDecision.Admitted(_, _) =>
        val (geometry, admission) = c match
          case GeometryChange.SetGeometry(g)  => (g, spec.admission)
          case GeometryChange.SetOffScreen(p) => (spec.geometry, spec.admission.copy(offScreen = p))
          case GeometryChange.AddRule(r)      =>
            (spec.geometry, spec.admission.copy(corrections = rules :+ r))
          case GeometryChange.RemoveRule(i) =>
            (spec.geometry, spec.admission.copy(corrections = rules.patch(i, Nil, 1)))
        Vector(
          Command.ImportSources(
            Some(id),
            spec.sources,
            spec.mapping,
            spec.units,
            geometry,
            spec.attributes,
            Some(admission)
          )
        )

  /** The id the next dataset revision gets (the reducer's rule). */
  def nextRevision(document: StudioDocument): DatasetRevision =
    DatasetRevision(document.datasets.lastOption.fold(1)(_.id.number + 1))

  /** Where the Data selection goes after a change turned `before` into
    * `after`: to the revision it created, if it created one. A revision
    * edited in place keeps the selection.
    */
  def follow(before: StudioDocument, after: StudioDocument): Option[Intent] =
    val known = before.datasets.map(_.id).toSet
    after.datasets
      .map(_.id)
      .filterNot(known)
      .lastOption
      .map(id => Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(id)))))
