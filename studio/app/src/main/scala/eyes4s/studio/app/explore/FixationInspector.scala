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

import eyes4s.plan.MapPlacement
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.text.{Format, InspectorText, InspectorTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.backend.{
  AdmittedFixation,
  AnalysisRevision,
  RunId,
  ScaleSource,
  TrialFixations,
  TrialKey
}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, Sigma}
import cats.Monad
import cats.data.EitherT
import eyes4s.studio.core.backend.{PageError, PageRequest}
import eyes4s.studio.core.navigation.{NavigationError, StudyNavigator, UsedBy, UsedByRole}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** Which runs' pairs use a trial, and the first pairs of each role: the
  * navigator's used-by counts and pages (UI-G A1), for the inspector's links.
  */
final case class UsedByAnswer(
    counts: UsedBy,
    asMatched: Vector[StudioRef],
    asControl: Vector[StudioRef],
    asQuery: Vector[StudioRef]
) derives CanEqual

/** Why the used-by links could not be read; each case names the map. */
enum UsedByError derives CanEqual:
  case Navigation(map: StudioRef, error: NavigationError)
  case Paging(map: StudioRef, error: PageError)

  def message: String = this match
    case Navigation(_, e) => e.message
    case Paging(m, e)     => s"The pairs using $m could not be paged: ${e.message}"

enum InspectorIntent derives CanEqual:
  case FixationsRead(
      revision: AnalysisRevision,
      trial: TrialKey,
      ask: Int,
      result: Either[String, BackendAnswer[TrialFixations]]
  )
  case RecordRead(
      revision: AnalysisRevision,
      record: Int,
      ask: Int,
      result: Either[String, BackendAnswer[SourceRecordPage]]
  )
  case UsedByRead(
      map: StudioRef,
      ask: Int,
      result: Either[String, BackendAnswer[UsedByAnswer]]
  )
  case DisplaysRead(ask: Int, result: Either[String, DisplaySource])
  case ShowRaw(on: Boolean)

enum InspectorEffect derives CanEqual:
  case ReadFixations(revision: AnalysisRevision, trial: TrialKey, ask: Int)

  /** Read the one record `record` (a data record, from 1). */
  case ReadRecord(revision: AnalysisRevision, record: Int, ask: Int)

  /** Read which pairs of the run use the trial's map `map`. */
  case ReadUsedBy(map: StudioRef, ask: Int)
  case ReadDisplays(dataset: DatasetRevisionSpec, ask: Int)

/** The scale the inspector's used-by links are read at, and its σ. */
final case class UsedByScale(index: ScaleIndex, sigma: Sigma) derives CanEqual

/** A labelled value of the inspector. */
final case class InspectorLine(label: String, value: String) derives CanEqual

/** A link of the inspector: its words and where it goes. */
final case class InspectorLink(label: String, go: Intent) derives CanEqual

/** What the fixation inspector shows (ticket S6.5; Explore.dc.html, right). */
final case class FixationInspectorVM(
    status: Option[String],
    title: String,
    fixation: Vector[InspectorLine],
    frameNote: Option[String],
    source: Vector[InspectorLine],
    raw: Option[String],
    usedByTitle: String,
    usedBy: Vector[InspectorLink],
    usedByNote: Option[String],
    trial: Vector[InspectorLine]
) derives CanEqual

/** Explore's fixation inspector (ticket S6.5): the selected fixation, under
  * the shown run's analysis revision. Its timing and placement are the
  * trial's admitted fixations (protocol 1.6); its coordinates in each frame
  * and its verbatim record come from its source record; its used-by links
  * from the navigator; its trial's display from the revision's registry.
  * Nothing is computed but counts of what was served. `ask` numbers the
  * reads of the shown fixation; an answer to an earlier ask is ignored.
  * Pure; a host performs the effects.
  */
final case class FixationInspector(
    revision: Option[AnalysisRevision],
    run: Option[RunId],
    dataset: Option[DatasetRevisionSpec],
    focus: Option[StudioRef.Fixation],
    scale: Option[UsedByScale],
    ask: Int,
    fixations: Loading[BackendAnswer[TrialFixations]],
    record: Loading[BackendAnswer[SourceRecordPage]],
    usedBy: Loading[BackendAnswer[UsedByAnswer]],
    displays: Loading[DisplaySource],
    raw: Boolean
) derives CanEqual

object FixationInspector:

  /** Which pairs of the run use the trial of `map`: the navigator's counts,
    * and the first pair of each role that has any (for its link).
    */
  def readUsedBy[F[_]: Monad](
      navigator: StudyNavigator[F]
  )(map: StudioRef): F[Either[UsedByError, UsedByAnswer]] =
    type Step[A] = EitherT[F, UsedByError, A]
    def nav[A](f: F[Either[NavigationError, A]]): Step[A] =
      EitherT(f).leftMap(UsedByError.Navigation(map, _))
    def first(role: UsedByRole, n: Int): Step[Vector[StudioRef]] =
      if n == 0 then EitherT.rightT(Vector.empty)
      else
        PageRequest.first(1) match
          case Left(e)     => EitherT.leftT(UsedByError.Paging(map, e))
          case Right(page) => nav(navigator.usedBy(map, role, page)).map(_.entries)
    (for
      counts  <- nav(navigator.usedByCounts(map))
      matched <- first(UsedByRole.AsMatched, counts.asMatched)
      control <- first(UsedByRole.AsControl, counts.asControl)
      query   <- first(UsedByRole.AsQuery, counts.asQuery)
    yield UsedByAnswer(counts, matched, control, query)).value

  val empty: FixationInspector = FixationInspector(
    None,
    None,
    None,
    None,
    None,
    0,
    Loading.Idle,
    Loading.Idle,
    Loading.Idle,
    Loading.Idle,
    false
  )

  /** The fixation the selection names: a fixation, or a record of one. */
  def focusOf(m: AppModel): Option[StudioRef.Fixation] =
    m.selection.selected.collectFirst {
      case f @ StudioRef.Fixation(_, _)                     => f
      case StudioRef.SourceRecord(trial, Some(index), _, _) => StudioRef.Fixation(trial, index)
    }

  /** The scale the used-by links of run `run` are read at: the scale of the
    * Compare trail's latest query or pair of that run, else the run's first
    * declared scale; with its σ, which the title names. None when the run's
    * analysis is not in the document.
    */
  def scaleOf(m: AppModel, run: RunId): Option[UsedByScale] =
    val scales = m.document
      .run(run)
      .flatMap(r => m.document.analysis(r.analysis))
      .map(_.recipe.scales.values)
    val visited = m.navigation
      .trail(Perspective.Compare)
      .reverseIterator
      .collectFirst {
        case Place.At(StudioRef.QueryContrast(`run`, scale, _)) => scale
        case Place.At(StudioRef.Pair(`run`, scale, _, _, _))    => scale
      }
    for
      ss    <- scales
      index <- visited
        .filter(_.value < ss.size)
        .orElse(Option.when(ss.nonEmpty)(ScaleIndex.first))
    yield UsedByScale(index, ss(index.value))

  /** Follows the model: a newly selected fixation, or a new revision, is
    * read afresh.
    */
  def sync(s: FixationInspector, m: AppModel): (FixationInspector, Vector[InspectorEffect]) =
    val revision = ExploreTrialView.shownRevision(m)
    val run      = m.document.presentation.shownRun
    val dataset  = revision
      .flatMap(m.document.analysis)
      .flatMap(a => m.document.dataset(a.dataset))
    val focus = focusOf(m)
    val scale = run.flatMap(scaleOf(m, _))
    if revision == s.revision && focus == s.focus && run == s.run && scale == s.scale then
      (s, Vector.empty)
    else
      val ask  = s.ask + 1
      val next = empty.copy(
        revision = revision,
        run = run,
        dataset = dataset,
        focus = focus,
        scale = scale,
        ask = ask,
        raw = s.raw
      )
      (revision, focus) match
        case (Some(r), Some(f)) =>
          val map = for
            r <- run
            x <- scale
          yield StudioRef.TrialMap(r, x.index, f.trial)
          (
            next.copy(
              fixations = Loading.Waiting,
              usedBy = map.fold(Loading.Idle)(_ => Loading.Waiting),
              displays = dataset.fold(Loading.Idle)(_ => Loading.Waiting)
            ),
            Vector(InspectorEffect.ReadFixations(r, f.trial, ask)) ++
              map.map(InspectorEffect.ReadUsedBy(_, ask)) ++
              dataset.map(InspectorEffect.ReadDisplays(_, ask))
          )
        case _ => (next, Vector.empty)

  /** The focused fixation as the backend admitted it. */
  def admitted(s: FixationInspector): Option[AdmittedFixation] =
    for
      f  <- s.focus
      fs <- s.fixations.toOption.collect { case BackendAnswer.Answered(t) => t }
      a  <- fs.fixations.find(_.ref.index == f.index)
    yield a

  def update(
      s: FixationInspector,
      intent: InspectorIntent
  ): (FixationInspector, Vector[InspectorEffect]) =
    def loaded[A](r: Either[String, A]): Loading[A] =
      r.fold(Loading.Failed(_), Loading.Ready(_))
    intent match
      case InspectorIntent.FixationsRead(r, trial, ask, result)
          if ask == s.ask && s.revision.contains(r) && s.focus.exists(_.trial == trial) =>
        val next = s.copy(fixations = loaded(result))
        // The fixation's record is read once its number is known.
        admitted(next) match
          case Some(a) =>
            (
              next.copy(record = Loading.Waiting),
              Vector(InspectorEffect.ReadRecord(r, a.record, ask))
            )
          case None => (next, Vector.empty)
      case InspectorIntent.RecordRead(r, record, ask, result)
          if ask == s.ask && s.revision.contains(r) && admitted(s).exists(_.record == record) =>
        (s.copy(record = loaded(result)), Vector.empty)
      case InspectorIntent.UsedByRead(_, ask, result) if ask == s.ask =>
        (s.copy(usedBy = loaded(result)), Vector.empty)
      case InspectorIntent.DisplaysRead(ask, result) if ask == s.ask =>
        (s.copy(displays = loaded(result)), Vector.empty)
      case InspectorIntent.ShowRaw(on) => (s.copy(raw = on), Vector.empty)
      case _                           => (s, Vector.empty)

  private def row(s: FixationInspector): Option[SourceRecordRow] =
    for
      a <- admitted(s)
      p <- s.record.toOption.collect { case BackendAnswer.Answered(p) => p }
      r <- p.rows.find(_.record.value == a.record)
    yield r

  private def failure[A](l: Loading[BackendAnswer[A]]): Option[String] = l match
    case Loading.Failed(why)                       => Some(why)
    case Loading.Ready(BackendAnswer.Refused(why)) => Some(why)
    case _                                         => None

  def vm(s: FixationInspector): FixationInspectorVM =
    import InspectorTextId.*
    def t(id: InspectorTextId, args: String*) = InspectorText(id, args*)
    val none                                  = t(Missing)
    s.focus match
      case None =>
        FixationInspectorVM(
          Some(t(NoFixation)),
          "",
          Vector.empty,
          None,
          Vector.empty,
          None,
          "",
          Vector.empty,
          None,
          Vector.empty
        )
      case Some(f) =>
        val fs     = s.fixations.toOption.collect { case BackendAnswer.Answered(t) => t }
        val a      = admitted(s)
        val r      = row(s)
        val status = failure(s.fixations).orElse(
          Option.when(s.fixations == Loading.Waiting)(t(Reading))
        )
        val count = fs.fold("")(_.fixations.size.toString)
        val pair  = (p: FramePosition, f: Double => String) => t(Pair, f(p.x), f(p.y))
        val place = a.map(_.placement match
          case MapPlacement.InWindow       => t(Inside)
          case MapPlacement.TrialFailed(w) =>
            t(InsideTrialFails, w.outsideWindow.toString, w.total.toString)
          case MapPlacement.DroppedInitial => t(DroppedInitial)
          case MapPlacement.OutsideScreen  => t(OffScreen)
          case MapPlacement.OutsideWindow(eyes4s.plan.OffWindowPolicy.Exclude) =>
            t(OutsideExcluded)
          case MapPlacement.OutsideWindow(eyes4s.plan.OffWindowPolicy.FailTrial) =>
            t(OutsideFails))
        val fixation = Vector(
          InspectorLine(
            t(OnsetDuration),
            a.fold(none)(x =>
              t(
                Milliseconds,
                Format.count(math.round(x.onsetMs)),
                Format.count(math.round(x.durationMs))
              )
            )
          ),
          InspectorLine(
            t(ImagePx),
            r.flatMap(_.image).fold(none)(pair(_, Format.decimal(_, 0)))
          ),
          InspectorLine(
            t(ScreenRaw),
            r.flatMap(_.screen).fold(none)(pair(_, Format.decimal(_, 1)))
          ),
          InspectorLine(
            t(Degrees),
            r.flatMap(_.degrees).fold(none)(pair(_, v => Format.signed(v, 1) + "°"))
          ),
          InspectorLine(t(Window), place.getOrElse(none))
        )
        // The scale the served degrees are at, as the page states it.
        val frameNote = r.flatMap(_ =>
          s.record.toOption
            .collect { case BackendAnswer.Answered(p) => p.scale }
            .flatten
            .map(x =>
              t(
                FrameNote,
                Format.decimal(x.pixelsPerDegree, 0),
                x.source match
                  case ScaleSource.Recipe  => t(ScaleOfRecipe)
                  case ScaleSource.Dataset => t(ScaleOfDataset)
              )
            )
        )
        val sourceFile = s.dataset.flatMap(_.sources.fixations)
        val source     = Vector(
          InspectorLine(t(File), sourceFile.fold(none)(_.path.value)),
          InspectorLine(
            t(Record),
            a.fold(none)(x =>
              r.flatMap(_ =>
                s.record.toOption.collect { case BackendAnswer.Answered(p) => p.total }
              ).fold(Format.count(x.record.toLong))(total =>
                t(RecordOf, Format.count(x.record.toLong), Format.count(total.toLong))
              )
            )
          ),
          InspectorLine(
            t(Digest),
            sourceFile.fold(none) { src =>
              val hex = src.bytes.hex
              t(DigestShort, hex.take(4), hex.takeRight(3))
            }
          ),
          InspectorLine(t(Ledger), fs.fold(none)(x => t(Admitted, x.dataset.label)))
        )
        val (title, links, note) = usedByOf(s)
        val display              = s.displays.toOption
          .collect { case DisplaySource.Served(reg) => reg }
          .flatMap(_.display(f.trial))
        val outside = fs.map(_.fixations.count(_.placement match
          case MapPlacement.OutsideWindow(_) => true
          case _                             => false))
        val trial = Vector(
          InspectorLine(
            t(Key),
            t(
              KeyValue,
              f.trial.participant,
              f.trial.phase.label,
              f.trial.trial,
              f.trial.occurrence.toString
            )
          ),
          InspectorLine(
            t(DisplayLabel),
            display.fold(none)(d =>
              d.display match
                case Display.Image(link)            => t(DisplayImage, link.fileName.value)
                case Display.Blank                  => t(DisplayBlank)
                case Display.BlankWithFixationCross => t(DisplayCross)
                case Display.Cue(_)                 => t(DisplayCue)
                case Display.Unknown(_)             => t(DisplayUnknown)
            )
          ),
          InspectorLine(t(MatchItem), display.flatMap(_.item).fold(none)(_.value)),
          InspectorLine(
            t(Fixations),
            fs.fold(none)(x =>
              t(FixationsValue, x.fixations.size.toString, outside.getOrElse(0).toString)
            )
          )
        )
        FixationInspectorVM(
          status,
          t(Title, f.index.value.toString, if count.isEmpty then none else count),
          fixation,
          frameNote,
          source,
          if s.raw then r.map(_.raw) else None,
          title,
          links,
          note,
          trial
        )

  // "Used by (run 7, σ 2°)": the pairs that use the trial's map, as links.
  private def usedByOf(
      s: FixationInspector
  ): (String, Vector[InspectorLink], Option[String]) =
    import InspectorTextId.*
    def t(id: InspectorTextId, args: String*) = InspectorText(id, args*)
    val title                                 = (s.run, s.scale) match
      case (Some(r), Some(x)) => t(UsedByTitle, r.number.toString, x.sigma.render)
      case (Some(r), None)    => t(UsedByNoScale, r.number.toString)
      case _                  => t(UsedByNoRun)
    s.usedBy match
      case Loading.Ready(BackendAnswer.Answered(u)) =>
        def focal(ref: StudioRef): Option[String] = ref match
          case StudioRef.Pair(_, _, _, focal, _) => Some(focal.trial)
          case _                                 => None
        val matched = u.asMatched.headOption.map { ref =>
          val label =
            if u.counts.asMatched == 1 then
              focal(ref).fold(t(MatchedMany, "1"))(q => t(MatchedOne, q))
            else t(MatchedMany, u.counts.asMatched.toString)
          InspectorLink(label, Intent.Explain(Place.At(ref)))
        }
        // A control link opens Compare's query view of the first query it
        // controls, where the control list shows it (the board's Main).
        val control = u.asControl.headOption.map { ref =>
          val go = ref match
            case StudioRef.Pair(run, scale, _, focal, _) =>
              Place.At(StudioRef.QueryContrast(run, scale, focal))
            case other => Place.At(other)
          InspectorLink(t(AsControl, u.counts.asControl.toString), Intent.Explain(go))
        }
        val query = u.asQuery.headOption.map { ref =>
          InspectorLink(t(AsQuery, u.counts.asQuery.toString), Intent.Explain(Place.At(ref)))
        }
        val links = Vector(query, matched, control).flatten
        (title, links, Option.when(links.isEmpty)(t(UsedByNone)))
      case Loading.Waiting => (title, Vector.empty, Some(t(Reading)))
      case other           => (title, Vector.empty, failure(other).orElse(Some(t(UsedByNone))))
