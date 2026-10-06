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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.text.{Format, SummaryText, SummaryTextId}
import eyes4s.studio.core.backend.{QueryContrasts, QueryRow, QueryStatus, RunId, TrialKey}
import eyes4s.studio.core.selection.{QueryCount, ScaleIndex, StudioRef}

/** One query of the navigator: its trial, item, its D at the shown σ or its
  * status when it has none, and its D bar (S8.1; Main.dc.html, left).
  */
final case class QueryEntry(
    ref: StudioRef,
    trial: String,
    item: String,
    said: String,
    scored: Boolean,
    bar: Option[DBar],
    selected: Boolean,
    cursor: Boolean,
    open: Intent
) derives CanEqual:

  /** The row as read aloud: "ret_07, beach-042, D +0.38". */
  def spoken: String = SummaryText(SummaryTextId.QueryRowSpoken, trial, item, said)

/** A zero-centred ink bar, drawn from the zero line to `fromZeroTo`, a
  * fraction of the bar's half-width (−1 to 1): the run's largest |D| at the
  * shown σ fills it, whatever the filter.
  */
final case class DBar(fromZeroTo: Double) derives CanEqual

/** A participant's group of queries, or an item's, whether it is open, and
  * whether the row cursor is on its header.
  */
final case class QueryGroup(
    key: String,
    label: String,
    summary: String,
    summaryRef: Option[StudioRef],
    open: Boolean,
    cursor: Boolean,
    entries: Vector[QueryEntry]
) derives CanEqual

/** The two navigators of Compare's query layout: by participant and by item. */
enum NavigatorKind derives CanEqual:
  case Queries, Items

/** A row of a navigator, where its row cursor can stand: a group's header or
  * a query.
  */
enum NavigatorRow derives CanEqual:
  case Header(key: String)
  case Query(ref: StudioRef)

/** The keys of a navigator's row cursor (DESIGN_SPEC section 10): move,
  * first and last, activate (a header opens or closes, a query opens), and
  * expand or collapse the group under the cursor.
  */
enum NavigatorKey derives CanEqual:
  case Up, Down, First, Last, Activate, Expand, Collapse

/** One line of the count strip: what is counted, how many, and the tally
  * each number shown names, in the order they are shown ("n/a" names none).
  */
final case class StripLine(
    label: String,
    count: String,
    failure: Boolean,
    refs: Vector[StudioRef]
) derives CanEqual

/** What the Queries and Items navigators show (ticket S8.1). */
final case class QueriesNavigatorVM(
    filter: String,
    empty: Option[String],
    dColumn: String,
    groups: Vector[QueryGroup],
    items: Vector[QueryGroup],
    strip: Vector[StripLine]
) derives CanEqual:

  /** The groups and queries of navigator `kind`. */
  def groupsOf(kind: NavigatorKind): Vector[QueryGroup] = kind match
    case NavigatorKind.Queries => groups
    case NavigatorKind.Items   => items

  /** The rows of navigator `kind` as shown: each header, and an open group's
    * queries under it.
    */
  def rows(kind: NavigatorKind): Vector[NavigatorRow] =
    groupsOf(kind).flatMap(g =>
      NavigatorRow.Header(g.key) +:
        (if g.open then g.entries.map(e => NavigatorRow.Query(e.ref)) else Vector.empty)
    )

  /** The row under navigator `kind`'s cursor as assistive technology
    * announces it, if the cursor is on a shown row: a header with its
    * summary and whether it is open, or a query with its item and D.
    */
  def cursorText(kind: NavigatorKind): Option[String] =
    groupsOf(kind).iterator
      .flatMap { g =>
        val header = Option.when(g.cursor)(
          SummaryText(
            SummaryTextId.CursorHeader,
            g.label,
            g.summary,
            SummaryText(if g.open then SummaryTextId.GroupOpen else SummaryTextId.GroupClosed)
          )
        )
        header.iterator ++ g.entries.iterator
          .filter(_.cursor)
          .map(e => SummaryText(SummaryTextId.CursorQuery, g.label, e.spoken))
      }
      .nextOption()

/** The navigators' own state: the filter text, the groups the user opened
  * or closed (a group holding the selection is open unless closed), and each
  * navigator's row cursor.
  */
final case class QueriesNavigator(
    run: Option[RunId],
    filter: String,
    opened: Set[String],
    closed: Set[String],
    cursors: Map[NavigatorKind, NavigatorRow]
) derives CanEqual

object QueriesNavigator:

  val initial: QueriesNavigator = QueriesNavigator(None, "", Set.empty, Set.empty, Map.empty)

  /** Follow the shown run: another run's groups start as the selection
    * opens them, with no cursor; the filter stays.
    */
  def follow(nav: QueriesNavigator, run: Option[RunId]): QueriesNavigator =
    if run == nav.run then nav else initial.copy(run = run, filter = nav.filter)

  /** What the navigators show as selected: the bus's selection and the refs
    * Compare's trail is at (a query is selected when the trail ends at it).
    */
  def selection(m: AppModel): Vector[StudioRef] =
    m.selection.selected ++ m.navigation.trail(Perspective.Compare).collect {
      case Place.At(ref) =>
        ref
    }

  /** The view-model of `nav` over the summary layout's state `s`, with
    * `selected` the bus's selection: participants in the backend's order,
    * their queries under them; items likewise; the count strip from the
    * summary's query contrasts. Nothing is computed but the D bars' scale.
    * A group is open when the user opened it or it holds the selection.
    */
  def vm(
      nav: QueriesNavigator,
      s: CompareSummary,
      selected: Vector[StudioRef]
  ): QueriesNavigatorVM =
    (s.run, s.answered, s.queries, s.shown) match
      case (None, _, _, _) =>
        QueriesNavigatorVM(
          nav.filter,
          Some(SummaryText(SummaryTextId.NoRun)),
          "",
          Vector.empty,
          Vector.empty,
          Vector.empty
        )
      case (Some(run), Some(r), Some(QueriesAnswer.Answered(_)), None) =>
        // Both answers are in, but no σ has served participant means: say why.
        val n   = run.number.toString
        val why = s.reporting match
          case None    => SummaryText(SummaryTextId.NoReportingSpec, n)
          case Some(_) =>
            val reason = s.reports.values
              .collectFirst {
                case ReportAnswer.Refused(error) => error.message
                case ReportAnswer.Failed(reason) => reason
              }
              .getOrElse("The reporting specification has not been evaluated at a scale yet.")
            SummaryText(SummaryTextId.NoMeansScale, n, reason)
        QueriesNavigatorVM(
          nav.filter,
          Some(why),
          "",
          Vector.empty,
          Vector.empty,
          strip(run, r.contrasts)
        )
      case (Some(run), Some(r), Some(QueriesAnswer.Answered(rows)), Some(scale)) =>
        val label   = r.scales.lift(scale.value).getOrElse(scale.value.toString)
        val shown   = rows.filter(matches(nav.filter))
        val kept    = shown.map(_.query).toSet
        val entries =
          entriesOf(run, scale, rows, selected).filter(e => kept(e.key)).map(_.entry)
        val byParticipant = shown.zip(entries).groupBy(_._1.query.participant)
        val participants  = shown.map(_.query.participant).distinct.map { p =>
          val es      = byParticipant(p).map(_._2)
          val summary = r.participants
            .find(_.participant == p)
            .fold("")(ps =>
              SummaryText(
                SummaryTextId.ParticipantHeader,
                ps.contributing.toString,
                ps.requested.toString,
                s.reports
                  .get((scale, true))
                  .collect { case ReportAnswer.Answered(view) => view }
                  .flatMap(
                    _.participant(None, eyes4s.studio.core.backend.ReportRole.Difference, p)
                  )
                  .flatMap(_.value)
                  .fold(SummaryText(SummaryTextId.NotApplicable))(Format.signed(_, 2))
              )
            )
          val ref = s.reports
            .get((scale, true))
            .collect { case ReportAnswer.Answered(view) => view }
            .flatMap(_.participant(None, eyes4s.studio.core.backend.ReportRole.Difference, p))
            .map(_.ref)
          group(nav, NavigatorKind.Queries, s"participant:$p", p, summary, ref, es)
        }
        val byItem = shown.zip(entries).groupBy(_._1.item)
        val items  = shown.map(_.item).distinct.sorted.map { item =>
          val es = byItem(item).map(_._2)
          group(
            nav,
            NavigatorKind.Items,
            s"item:$item",
            item,
            SummaryText(SummaryTextId.ItemHeader, es.size.toString),
            None,
            es
          )
        }
        QueriesNavigatorVM(
          nav.filter,
          None,
          SummaryText(SummaryTextId.DColumn, label),
          participants,
          items,
          strip(run, r.contrasts)
        )
      case (Some(run), _, _, _) =>
        val n      = run.number.toString
        val status = (s.summary, s.queries) match
          case (Some(SummaryAnswer.Refused(e)), _) =>
            SummaryText(SummaryTextId.Unreadable, n, e.message)
          case (Some(SummaryAnswer.Failed(why)), _) =>
            SummaryText(SummaryTextId.Unreadable, n, why)
          case (_, Some(QueriesAnswer.Failed(why))) =>
            SummaryText(SummaryTextId.Unreadable, n, why)
          case _ => SummaryText(SummaryTextId.Reading, n)
        QueriesNavigatorVM(
          nav.filter,
          Some(status),
          "",
          Vector.empty,
          Vector.empty,
          s.answered.fold(Vector.empty)(r => strip(run, r.contrasts))
        )

  /** The count strip (Main.dc.html): requested, contributing, failed, no
    * match and query not admitted, and by design, which this summary does
    * not count (n/a).
    */
  def strip(run: RunId, c: QueryContrasts): Vector[StripLine] =
    import SummaryTextId.*
    def tally(count: QueryCount) = Vector(StudioRef.QueryTally(run, count))
    Vector(
      StripLine(
        SummaryText(StripRequested),
        Format.count(c.requested.toLong),
        false,
        tally(QueryCount.Requested)
      ),
      StripLine(
        SummaryText(StripContributing),
        Format.count(c.contributing.toLong),
        false,
        tally(QueryCount.Contributing)
      ),
      StripLine(
        SummaryText(StripFailed),
        Format.count(c.failed.toLong),
        c.failed > 0,
        tally(QueryCount.Failed)
      ),
      StripLine(
        SummaryText(StripNoMatchNotAdmitted),
        SummaryText(
          StripPair,
          Format.count(c.noMatch.toLong),
          Format.count(c.queryNotAdmitted.toLong)
        ),
        false,
        tally(QueryCount.NoMatch) ++ tally(QueryCount.QueryNotAdmitted)
      ),
      StripLine(SummaryText(StripByDesign), SummaryText(NotApplicable), false, Vector.empty)
    )

  /** The controls inside a navigator's own focus stop: its filter, unless
    * it says why it is empty.
    */
  def focusStops(vm: QueriesNavigatorVM): Vector[FocusStop] =
    if vm.empty.isDefined then Vector.empty
    else Vector(FocusStop(A11yRole.TextField, SummaryText(SummaryTextId.Filter)))

  /** Closes the group `key` if it is `open`, opens it otherwise. */
  def toggle(nav: QueriesNavigator, key: String, open: Boolean): QueriesNavigator =
    if open then nav.copy(opened = nav.opened - key, closed = nav.closed + key)
    else nav.copy(opened = nav.opened + key, closed = nav.closed - key)

  /** A key on navigator `kind`'s row cursor over `vm`, what `nav` shows: the
    * state after it, and the intent a query's activation sends. A cursor
    * not on a shown row starts from the first; moves stop at the ends.
    * Collapse on a query moves to its group's header.
    */
  def key(
      nav: QueriesNavigator,
      vm: QueriesNavigatorVM,
      kind: NavigatorKind,
      key: NavigatorKey
  ): (QueriesNavigator, Option[Intent]) =
    val rows = vm.rows(kind)
    val at   = nav.cursors.get(kind).map(rows.indexOf).filter(_ >= 0)
    def to(i: Int): (QueriesNavigator, Option[Intent]) =
      if rows.isEmpty then (nav, None)
      else
        (nav.copy(cursors = nav.cursors.updated(kind, rows(i.max(0).min(rows.size - 1)))), None)
    val groups                  = vm.groupsOf(kind)
    def header(k: String)       = groups.find(_.key == k)
    def holding(ref: StudioRef) = groups.find(_.entries.exists(_.ref == ref))
    (key, at.map(i => (i, rows(i)))) match
      case (_, None) if key != NavigatorKey.Last                      => to(0)
      case (NavigatorKey.Up, Some((i, _)))                            => to(i - 1)
      case (NavigatorKey.Down, Some((i, _)))                          => to(i + 1)
      case (NavigatorKey.First, _)                                    => to(0)
      case (NavigatorKey.Last, _)                                     => to(rows.size - 1)
      case (NavigatorKey.Activate, Some((_, NavigatorRow.Header(k)))) =>
        (header(k).fold(nav)(g => toggle(nav, k, g.open)), None)
      case (NavigatorKey.Activate, Some((_, NavigatorRow.Query(ref)))) =>
        (nav, holding(ref).flatMap(_.entries.find(_.ref == ref)).map(_.open))
      case (NavigatorKey.Expand, Some((_, NavigatorRow.Header(k)))) =>
        (header(k).filterNot(_.open).fold(nav)(g => toggle(nav, k, g.open)), None)
      case (NavigatorKey.Collapse, Some((_, NavigatorRow.Header(k)))) =>
        (header(k).filter(_.open).fold(nav)(g => toggle(nav, k, g.open)), None)
      case (NavigatorKey.Collapse, Some((_, NavigatorRow.Query(ref)))) =>
        (
          holding(ref).fold(nav)(g =>
            nav.copy(cursors = nav.cursors.updated(kind, NavigatorRow.Header(g.key)))
          ),
          None
        )
      case _ => (nav, None)

  private def group(
      nav: QueriesNavigator,
      kind: NavigatorKind,
      key: String,
      label: String,
      summary: String,
      summaryRef: Option[StudioRef],
      entries: Vector[QueryEntry]
  ): QueryGroup =
    val cursor = nav.cursors.get(kind)
    QueryGroup(
      key,
      label,
      summary,
      summaryRef,
      !nav.closed(key) && (nav.opened(key) || entries.exists(_.selected)),
      cursor.contains(NavigatorRow.Header(key)),
      entries.map(e => e.copy(cursor = cursor.contains(NavigatorRow.Query(e.ref))))
    )

  /** Filters by participant or item, ignoring case. */
  def filter(nav: QueriesNavigator, text: String): QueriesNavigator = nav.copy(filter = text)

  private def matches(filter: String)(q: QueryRow): Boolean =
    val f = filter.trim.toLowerCase
    f.isEmpty || q.query.participant.toLowerCase.contains(f) || q.item.toLowerCase.contains(f)

  private def entriesOf(
      run: RunId,
      scale: ScaleIndex,
      rows: Vector[QueryRow],
      selected: Vector[StudioRef]
  ): Vector[Keyed] =
    def dOf(q: QueryRow): Option[Double] = q.status match
      case QueryStatus.Contributing(_, _, d) => d.lift(scale.value)
      case _                                 => None
    val extent = rows.flatMap(dOf).map(math.abs).maxOption.filter(_ > 0.0).getOrElse(1.0)
    rows.map { q =>
      val ref = StudioRef.QueryContrast(run, scale, q.query)
      val d   = dOf(q)
      Keyed(
        q.query,
        QueryEntry(
          ref,
          q.query.trial,
          q.item,
          d.fold(statusWord(q.status))(Format.signed(_, 2)),
          d.isDefined,
          d.map(v => DBar(v / extent)),
          selected.exists(sameQuery(run, q.query)),
          false,
          Intent.Explain(Place.At(ref))
        )
      )
    }

  private final case class Keyed(key: TrialKey, entry: QueryEntry)

  /** Whether `ref` is the query contrast of `query` in `run`, at any σ: the
    * navigator shows one σ, and a query picked at another (a ladder rung)
    * is still this query.
    */
  private def sameQuery(run: RunId, query: TrialKey)(ref: StudioRef): Boolean = ref match
    case StudioRef.QueryContrast(r, _, q) => r == run && q == query
    case _                                => false

  private def statusWord(status: QueryStatus): String = status match
    case QueryStatus.Contributing(_, _, _) => SummaryText(SummaryTextId.NotApplicable)
    case QueryStatus.Failed(_) | QueryStatus.FailedAtScales(_) =>
      SummaryText(SummaryTextId.StatusFailed)
    case QueryStatus.NoMatch(_)     => SummaryText(SummaryTextId.StatusNoMatch)
    case QueryStatus.NotAdmitted(_) => SummaryText(SummaryTextId.StatusNotAdmitted)
