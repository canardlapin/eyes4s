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

import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.text.{Format, RecordText, RecordTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{AnalysisRevision, TrialFixations, TrialKey}
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.selection.{
  FixationIndex,
  InputCause,
  RecordNumber,
  SelectionMode,
  SelectionState,
  StudioRef
}

/** Where a source record's position falls, as the backend placed it. */
enum RecordPlace derives CanEqual:
  /** In the analysis window. */
  case Inside

  /** On the screen, outside the window. */
  case Outside

  /** Off the screen. */
  case OffScreen

  /** Not admitted: the record has no admitted fixation to place. */
  case NotAdmitted

/** A position in a named frame, as the backend served it. */
final case class FramePosition(x: Double, y: Double) derives CanEqual

/** One record of a fixation table as the backend serves it (ticket S6.4):
  * its data record (from 1, the header excluded) and ref, its trial and
  * ordinal, onset and duration, the position fields verbatim, the image-frame
  * and angular positions when the study has them (degrees from the window's
  * centre, x right, y up), its sample count, where it falls, and the
  * record's verbatim text. Nothing here is computed by the studio.
  */
final case class SourceRecordRow(
    record: RecordNumber,
    ref: StudioRef,
    fixation: Option[FixationIndex],
    trial: TrialKey,
    ordinal: Int,
    onsetMs: Double,
    durationMs: Double,
    rawX: String,
    rawY: String,
    image: Option[FramePosition],
    degrees: Option[FramePosition],
    samples: Option[Int],
    place: RecordPlace,
    raw: String
) derives CanEqual:

  /** The fixation this record supplied, if it was admitted. */
  def fixationRef: Option[StudioRef] = fixation.map(StudioRef.Fixation(trial, _))

/** A page of a fixation table's records: the first record's row index, the
  * table's record count, and the rows.
  */
final case class SourceRecordPage(from: Int, total: Int, rows: Vector[SourceRecordRow])
    derives CanEqual

/** Where the source records table reads its pages (ticket S6.4): a port, so
  * the table does not depend on how a backend serves records. `done` may be
  * called on any thread.
  */
trait SourceRecordsSource:
  def page(
      revision: AnalysisRevision,
      from: Int,
      size: Int,
      done: Either[String, BackendAnswer[SourceRecordPage]] => Unit
  ): Unit

/** A move of the table's row cursor: a row, a screenful, or an end. */
enum RecordMove derives CanEqual:
  case Up, Down, PageUp, PageDown, First, Last

enum SourceRecordsIntent derives CanEqual:
  /** The rows the view shows: `count` rows from row index `first`. */
  case Viewport(first: Int, count: Int)

  case PageRead(
      revision: AnalysisRevision,
      page: Int,
      ask: Int,
      result: Either[String, BackendAnswer[SourceRecordPage]]
  )

  /** A trial's fixations, for the records of its fixations. */
  case Located(
      revision: AnalysisRevision,
      trial: TrialKey,
      result: Either[String, BackendAnswer[TrialFixations]]
  )

  /** The row cursor's keys; Enter selects the row's fixation. */
  case Move(move: RecordMove)
  case Activate

  /** A click on the row at `index`. */
  case Click(index: Int)

  /** Show or hide the verbatim record under the cursor. */
  case ShowRaw(on: Boolean)

enum SourceRecordsEffect derives CanEqual:
  case RequestPage(revision: AnalysisRevision, page: Int, from: Int, size: Int, ask: Int)
  case Locate(revision: AnalysisRevision, trial: TrialKey)
  case App(intent: Intent)

/** One row as the view draws it: its cells, whether it is under the cursor or
  * selected, and its accessible text; or a row still being read.
  */
enum SourceRowVM derives CanEqual:
  case Shown(cells: Vector[String], cursor: Boolean, selected: Boolean, accessible: String)
  case Reading
  case Failed(reason: String)

/** Explore's source records table (ticket S6.4; Explore.dc.html, bottom):
  * every record of the revision's fixation table, read a page at a time
  * around what the view shows, with a row cursor that follows the selection.
  * At most [[SourceRecords.KeptPages]] pages are held. Pure; a host performs
  * the effects.
  */
final case class SourceRecords(
    revision: Option[AnalysisRevision],
    ask: Int,
    total: Option[Int],
    pages: Map[Int, Loading[BackendAnswer[SourceRecordPage]]],
    viewport: (Int, Int),
    cursor: Option[Int],
    raw: Boolean,
    selection: ViewSelection,
    followed: Option[StudioRef],
    located: Map[TrialKey, Loading[BackendAnswer[TrialFixations]]]
) derives CanEqual

object SourceRecords:

  /** Records per page read. */
  val PageSize: Int = 128

  /** The most pages held: those around the viewport and the cursor's. */
  val KeptPages: Int = 6

  def initial(selection: ViewSelection): SourceRecords =
    SourceRecords(None, 0, None, Map.empty, (0, 0), None, false, selection, None, Map.empty)

  private def pageOf(index: Int): Int = index / PageSize

  private def request(
      s: SourceRecords,
      page: Int
  ): (SourceRecords, Vector[SourceRecordsEffect]) =
    (s.revision, s.pages.get(page)) match
      case (Some(r), None) =>
        (
          s.copy(pages = s.pages.updated(page, Loading.Waiting)),
          Vector(SourceRecordsEffect.RequestPage(r, page, page * PageSize, PageSize, s.ask))
        )
      case _ => (s, Vector.empty)

  // The pages the viewport and the cursor need, asked for; the rest let go
  // when more than KeptPages are held.
  private def fill(s: SourceRecords): (SourceRecords, Vector[SourceRecordsEffect]) =
    val (first, count) = s.viewport
    val last           = s.total.fold(first + count)(t => math.min(first + count, t)) - 1
    val wanted         =
      ((pageOf(first) to pageOf(math.max(first, last))).toVector ++ s.cursor.map(pageOf))
        .filter(p => s.total.forall(p * PageSize < _))
        .distinct
    val (asked, effects) = wanted.foldLeft((s, Vector.empty[SourceRecordsEffect])) {
      case ((st, es), p) =>
        val (n, e) = request(st, p)
        (n, es ++ e)
    }
    val held = asked.pages.keys.toVector
    val drop =
      if held.size <= KeptPages then Vector.empty
      else
        held
          .filterNot(wanted.contains)
          .sortBy(p => -math.abs(p - pageOf(first)))
          .take(held.size - KeptPages)
    (asked.copy(pages = asked.pages -- drop), effects)

  /** Follows the model: the shown run's revision, and the selection, whose
    * record the cursor moves to.
    */
  def sync(s: SourceRecords, m: AppModel): (SourceRecords, Vector[SourceRecordsEffect]) =
    val revision       = ExploreTrialView.shownRevision(m)
    val (projected, _) = s.selection.project(m.selection)
    val base           =
      if revision == s.revision then s.copy(selection = projected)
      else
        initial(projected).copy(
          revision = revision,
          ask = s.ask + 1,
          viewport = s.viewport,
          raw = s.raw
        )
    val (followed, asks) = follow(base, m.selection)
    val (filled, reads)  = fill(followed)
    (filled, asks ++ reads)

  // The first fixation or fixation record the selection holds.
  // A record of another source (the trial inventory) is not this table's.
  private def selectedRecordRef(selection: SelectionState): Option[StudioRef] =
    selection.selected.collectFirst {
      case r @ StudioRef.SourceRecord(_, _, SourceRole.Fixations, _) => r
      case f @ StudioRef.Fixation(_, _)                              => f
    }

  private def follow(
      s: SourceRecords,
      selection: SelectionState
  ): (SourceRecords, Vector[SourceRecordsEffect]) =
    selectedRecordRef(selection) match
      case None                                  => (s.copy(followed = None), Vector.empty)
      case Some(ref) if s.followed.contains(ref) => (s, Vector.empty)
      case Some(ref)                             =>
        recordOf(s, ref) match
          case Some(record) =>
            (s.copy(cursor = Some(record - 1), followed = Some(ref)), Vector.empty)
          case None =>
            ref match
              case StudioRef.Fixation(trial, _) if !s.located.contains(trial) =>
                s.revision.fold((s, Vector.empty)) { r =>
                  (
                    s.copy(located = s.located.updated(trial, Loading.Waiting)),
                    Vector(SourceRecordsEffect.Locate(r, trial))
                  )
                }
              case _ => (s, Vector.empty)

  // The data record of a selected ref: a record ref names it; a fixation's
  // comes from its trial's admitted fixations.
  private def recordOf(s: SourceRecords, ref: StudioRef): Option[Int] = ref match
    case StudioRef.SourceRecord(_, _, SourceRole.Fixations, record) => Some(record.value)
    case StudioRef.Fixation(trial, index)                           =>
      s.located
        .get(trial)
        .flatMap(_.toOption)
        .collect { case BackendAnswer.Answered(t) =>
          t
        }
        .flatMap(_.fixations.find(_.ref.index == index))
        .map(_.record)
    case _ => None

  def update(
      s: SourceRecords,
      intent: SourceRecordsIntent,
      m: AppModel
  ): (SourceRecords, Vector[SourceRecordsEffect]) =
    import SourceRecordsIntent.*
    intent match
      case Viewport(first, count) =>
        fill(s.copy(viewport = (math.max(0, first), math.max(0, count))))
      case PageRead(r, page, ask, result) =>
        if !s.revision.contains(r) || ask != s.ask || !s.pages.contains(page) then
          (s, Vector.empty)
        else
          // A page that does not start where it was asked is refused, by name.
          val loaded = result match
            case Left(why) => Loading.Failed(why)
            case Right(BackendAnswer.Answered(p)) if p.from != page * PageSize =>
              Loading.Ready(
                BackendAnswer.Refused(
                  RecordText(
                    RecordTextId.Misaligned,
                    page.toString,
                    p.from.toString,
                    (page * PageSize).toString
                  )
                )
              )
            case Right(a) => Loading.Ready(a)
          val total = result.toOption.collect { case BackendAnswer.Answered(p) => p.total }
          fill(s.copy(pages = s.pages.updated(page, loaded), total = total.orElse(s.total)))
      case Located(r, trial, result) =>
        if !s.revision.contains(r) || !s.located.get(trial).contains(Loading.Waiting) then
          (s, Vector.empty)
        else
          val loaded = result.fold(Loading.Failed(_), Loading.Ready(_))
          fill(follow(s.copy(located = s.located.updated(trial, loaded)), m.selection)._1)
      case Move(move) =>
        s.total.filter(_ > 0).fold((s, Vector.empty)) { t =>
          val (first, count) = s.viewport
          // With no cursor yet, the first Down lands on the first row shown.
          val at = s.cursor.getOrElse(move match
            case RecordMove.Down => first - 1
            case _               => first)
          val page = math.max(1, count - 1)
          val to   = move match
            case RecordMove.Up       => at - 1
            case RecordMove.Down     => at + 1
            case RecordMove.First    => 0
            case RecordMove.Last     => t - 1
            case RecordMove.PageUp   => at - page
            case RecordMove.PageDown => at + page
          fill(s.copy(cursor = Some(to.max(0).min(t - 1))))
        }
      case Activate => s.cursor.fold((s, Vector.empty))(select(s, _, InputCause.Keyboard))
      case Click(i) =>
        val (moved, _) = fill(s.copy(cursor = Some(i)))
        select(moved, i, InputCause.Pointer)
      case ShowRaw(on) => (s.copy(raw = on), Vector.empty)

  // Selects the row's fixation, or its record when it has none.
  private def select(
      s: SourceRecords,
      index: Int,
      cause: InputCause
  ): (SourceRecords, Vector[SourceRecordsEffect]) =
    rowAt(s, index).fold((s, Vector.empty)) { row =>
      val (next, intent) = s.selection.submit(
        SelectionMode.Replace,
        Vector(row.fixationRef.getOrElse(row.ref)),
        cause
      )
      (
        s.copy(selection = next, followed = Some(row.fixationRef.getOrElse(row.ref))),
        Vector(SourceRecordsEffect.App(intent))
      )
    }

  /** The row at `index`, if its page is read. */
  def rowAt(s: SourceRecords, index: Int): Option[SourceRecordRow] =
    s.pages
      .get(pageOf(index))
      .flatMap(_.toOption)
      .collect { case BackendAnswer.Answered(p) =>
        p
      }
      .flatMap(p => p.rows.lift(index - p.from))

  /** The columns' headers (Explore.dc.html, bottom). */
  val headers: Vector[String] =
    import RecordTextId.*
    Vector(
      Record,
      Trial,
      Ordinal,
      Onset,
      Duration,
      Screen,
      Image,
      Degrees,
      Samples,
      Window
    ).map(RecordText(_))

  /** The row at `index` as the view draws it. */
  def rowVM(s: SourceRecords, index: Int): SourceRowVM =
    s.pages.get(pageOf(index)) match
      case Some(Loading.Failed(why))                       => SourceRowVM.Failed(why)
      case Some(Loading.Ready(BackendAnswer.Refused(why))) => SourceRowVM.Failed(why)
      case _                                               =>
        rowAt(s, index).fold(SourceRowVM.Reading) { r =>
          val selected =
            r.fixationRef.exists(s.selection.isSelected) || s.selection.isSelected(r.ref)
          val cells = cellsOf(r)
          SourceRowVM.Shown(
            cells,
            s.cursor.contains(index),
            selected,
            RecordText(RecordTextId.RowName, cells(0), r.trial.trial, cells(2))
          )
        }

  /** A row's cells, every number as the backend served it. */
  def cellsOf(r: SourceRecordRow): Vector[String] =
    def at(p: Option[FramePosition], f: Double => String) =
      p.fold(RecordText(RecordTextId.None))(q => RecordText(RecordTextId.Pair, f(q.x), f(q.y)))
    Vector(
      Format.count(r.record.value.toLong),
      r.trial.trial,
      r.ordinal.toString,
      Format.count(math.round(r.onsetMs)),
      Format.count(math.round(r.durationMs)),
      RecordText(RecordTextId.Pair, r.rawX, r.rawY),
      at(r.image, v => Format.decimal(v, 0)),
      at(r.degrees, v => Format.signed(v, 1) + "°"),
      r.samples.fold(RecordText(RecordTextId.None))(n => Format.count(n.toLong)),
      RecordText(r.place match
        case RecordPlace.Inside      => RecordTextId.Inside
        case RecordPlace.Outside     => RecordTextId.Outside
        case RecordPlace.OffScreen   => RecordTextId.OffScreen
        case RecordPlace.NotAdmitted => RecordTextId.NotAdmitted)
    )

  /** The verbatim record under the cursor, when 'Show raw record' is on. */
  def rawLine(s: SourceRecords): Option[String] =
    if !s.raw then None else s.cursor.flatMap(rowAt(s, _)).map(_.raw)

  /** Why the table shows no rows, if it shows none. */
  def status(s: SourceRecords): Option[String] =
    (s.revision, s.total) match
      case (None, _)          => Some(RecordText(RecordTextId.NoRun))
      case (Some(_), Some(0)) => Some(RecordText(RecordTextId.Empty))
      case (Some(_), Some(_)) => None
      case (Some(_), None)    =>
        s.pages.get(0) match
          case Some(Loading.Failed(why))                       => Some(why)
          case Some(Loading.Ready(BackendAnswer.Refused(why))) => Some(why)
          case _ => Some(RecordText(RecordTextId.Reading))
