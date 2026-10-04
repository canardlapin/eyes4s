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
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.text.{Format, SummaryText, SummaryTextId}
import eyes4s.studio.core.backend.{QueryContrasts, QueryRow, QueryStatus, RunId, TrialKey}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

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
) derives CanEqual

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

/** One line of the count strip: what is counted and how many. */
final case class StripLine(label: String, count: String, failure: Boolean) derives CanEqual

/** What the Queries and Items navigators show (ticket S8.1). */
final case class QueriesNavigatorVM(
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

/** The navigators' own state: the filter text, the groups the user opened
  * or closed (a group holding the selection is open unless closed), and each
  * navigator's row cursor.
  */
final case class QueriesNavigator(
    filter: String,
    opened: Set[String],
    closed: Set[String],
    cursors: Map[NavigatorKind, NavigatorRow]
) derives CanEqual

object QueriesNavigator:

  val initial: QueriesNavigator = QueriesNavigator("", Set.empty, Set.empty, Map.empty)

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
          Some(SummaryText(SummaryTextId.NoRun)),
          "",
          Vector.empty,
          Vector.empty,
          Vector.empty
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
                Format.signed(ps.all.d, 2)
              )
            )
          group(nav, NavigatorKind.Queries, p, p, summary, es)
        }
        val byItem = shown.zip(entries).groupBy(_._1.item)
        val items  = shown.map(_.item).distinct.sorted.map { item =>
          val es = byItem(item).map(_._2)
          group(
            nav,
            NavigatorKind.Items,
            s"item:$item",
            item,
            SummaryText(SummaryTextId.ItemHeader, item, es.size.toString),
            es
          )
        }
        QueriesNavigatorVM(
          None,
          SummaryText(SummaryTextId.DColumn, label),
          participants,
          items,
          strip(r.contrasts)
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
          Some(status),
          "",
          Vector.empty,
          Vector.empty,
          s.answered.fold(Vector.empty)(r => strip(r.contrasts))
        )

  /** The count strip (Main.dc.html): requested, contributing, failed, no
    * match and query not admitted, and by design, which this summary does
    * not count (n/a).
    */
  def strip(c: QueryContrasts): Vector[StripLine] =
    import SummaryTextId.*
    Vector(
      StripLine(SummaryText(StripRequested), Format.count(c.requested.toLong), false),
      StripLine(SummaryText(StripContributing), Format.count(c.contributing.toLong), false),
      StripLine(SummaryText(StripFailed), Format.count(c.failed.toLong), c.failed > 0),
      StripLine(
        SummaryText(StripNoMatchNotAdmitted),
        SummaryText(
          StripPair,
          Format.count(c.noMatch.toLong),
          Format.count(c.queryNotAdmitted.toLong)
        ),
        false
      ),
      StripLine(SummaryText(StripByDesign), SummaryText(NotApplicable), false)
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
      entries: Vector[QueryEntry]
  ): QueryGroup =
    val cursor = nav.cursors.get(kind)
    QueryGroup(
      key,
      label,
      summary,
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
          selected.contains(ref),
          false,
          Intent.Explain(Place.At(ref))
        )
      )
    }

  private final case class Keyed(key: TrialKey, entry: QueryEntry)

  private def statusWord(status: QueryStatus): String = status match
    case QueryStatus.Contributing(_, _, _) => SummaryText(SummaryTextId.NotApplicable)
    case QueryStatus.Failed(_)             => SummaryText(SummaryTextId.StatusFailed)
    case QueryStatus.NoMatch(_)            => SummaryText(SummaryTextId.StatusNoMatch)
    case QueryStatus.NotAdmitted(_)        => SummaryText(SummaryTextId.StatusNotAdmitted)
