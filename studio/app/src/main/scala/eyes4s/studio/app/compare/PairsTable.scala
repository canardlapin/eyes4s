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

import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotSourceError,
  PlotValue
}
import eyes4s.studio.app.text.Messages
import eyes4s.studio.core.backend.{PairDesign, PairRowEntry, PairScoreState, QueryRow, RunId}
import eyes4s.studio.core.figures.MethodsReadError
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** The backend's answer for a run's pair rows at one scale. */
enum PairsAnswer derives CanEqual:
  case Answered(rows: Vector[PairRowEntry])

  /** The paged read was refused; the typed paging error says where. */
  case Failed(error: MethodsReadError)

  /** The read itself failed (the effect raised). */
  case Broken(reason: String)

enum PairsEffect derives CanEqual:
  /** Read every page of `run`'s pair rows at `scale` (protocol 1.9). */
  case ReadPairs(run: RunId, scale: ScaleIndex)

/** The pairs table's view-model: its source (every pair row of the run at
  * the focused scale), the row of the focused pair, or why there is none.
  */
final case class PairsVM(
    status: Option[String],
    source: Option[PlotSource],
    focused: Option[StudioRef],
    retry: Option[String]
) derives CanEqual

/** The strings of the pairs table. */
enum PairsTextId derives CanEqual:
  case Caption, NoQuery, Reading, Unreadable, Broken, Retry
  case Query, QueryItem, Role, Reference, ReferenceItem, Sigma, Cosine, Score
  case Matched, Control, Scored, Failed, NotServed

object PairsText:
  import PairsTextId.*

  def english(id: PairsTextId): String = id match
    case Caption       => "Pairs of run {0} at σ {1}: {2} rows"
    case NoQuery       => "Choose a query in the Queries navigator"
    case Reading       => "Reading the pair rows of run {0} at σ {1}…"
    case Unreadable    => "The pair rows could not be read: {0}"
    case Broken        => "The pair rows could not be read: {0}"
    case Retry         => "Retry"
    case Query         => "Query"
    case QueryItem     => "Query item"
    case Role          => "Role"
    case Reference     => "Reference"
    case ReferenceItem => "Reference item"
    case Sigma         => "σ"
    case Cosine        => "Cosine"
    case Score         => "Score"
    case Matched       => "matched"
    case Control       => "control"
    case Scored        => "scored"
    case Failed        => "failed: {0}"
    case NotServed     => "not served"

  def apply(id: PairsTextId, args: String*): String = Messages.fill(english(id), args.toVector)

/** Compare's pairs table (ticket S8.5; Main.dc.html, the contrast group's
  * "Pairs table" tab): every pair row of the shown run at the trail's scale,
  * read through protocol 1.9 `pairRows` a page at a time (refused as
  * [[eyes4s.studio.core.figures.MethodsReads.pairRowsAt]] refuses), each
  * row traced to its pair. A failed pair says so with its diagnostic and a
  * pair the backend does not serve says that; neither shows a number. The
  * focused query's pair is the table's cursor row. Pure; a host performs
  * the effects.
  */
final case class PairsTable(
    key: Option[(RunId, ScaleIndex)],
    answer: Option[PairsAnswer],
    cache: Option[PairsCache] = None
) derives CanEqual

/** The source last built from an answer, and what it was built from: the
  * answer's key and label and the very answer and query rows (compared by
  * reference), so a render that changes none of them reuses it.
  */
final case class PairsCache(
    key: (RunId, ScaleIndex),
    label: String,
    entries: Vector[PairRowEntry],
    queries: Vector[QueryRow],
    source: Either[PlotSourceError, PlotSource]
) derives CanEqual:
  def fits(
      k: (RunId, ScaleIndex),
      l: String,
      es: Vector[PairRowEntry],
      qs: Vector[QueryRow]
  ): Boolean = key == k && label == l && (entries eq es) && (queries eq qs)

object PairsTable:
  import PairsTextId.*

  val empty: PairsTable = PairsTable(None, None)

  /** Follows the panels' focus: a new run or scale is read once. */
  def sync(s: PairsTable, focus: Option[PanelFocus]): (PairsTable, Vector[PairsEffect]) =
    val key = focus.map(f => (f.run, f.scale))
    if key == s.key then (s, Vector.empty)
    else
      (PairsTable(key, None, None), key.toVector.map((r, sc) => PairsEffect.ReadPairs(r, sc)))

  /** An answer for `run` at `scale`; kept only while that is still asked. */
  def read(s: PairsTable, run: RunId, scale: ScaleIndex, answer: PairsAnswer): PairsTable =
    if s.key.contains((run, scale)) && s.answer.isEmpty then s.copy(answer = Some(answer))
    else s

  /** Read again after a failed read. */
  def retry(s: PairsTable): (PairsTable, Vector[PairsEffect]) =
    (s.key, s.answer) match
      case (Some((r, sc)), Some(PairsAnswer.Failed(_) | PairsAnswer.Broken(_))) =>
        (s.copy(answer = None, cache = None), Vector(PairsEffect.ReadPairs(r, sc)))
      case _ => (s, Vector.empty)

  /** The column identities of the table's source. */
  final case class Columns(
      query: ColumnId,
      queryItem: ColumnId,
      role: ColumnId,
      reference: ColumnId,
      referenceItem: ColumnId,
      sigma: ColumnId,
      cosine: ColumnId,
      score: ColumnId
  ) derives CanEqual

  val columns: Either[PlotSourceError, Columns] =
    for
      q  <- ColumnId.of("query")
      qi <- ColumnId.of("query-item")
      r  <- ColumnId.of("role")
      rf <- ColumnId.of("reference")
      ri <- ColumnId.of("reference-item")
      s  <- ColumnId.of("sigma")
      c  <- ColumnId.of("cosine")
      sc <- ColumnId.of("score")
    yield Columns(q, qi, r, rf, ri, s, c, sc)

  /** The rows as a value source, one row per pair, each traced to its
    * [[StudioRef.Pair]]. The cosine is the served score, or missing when the
    * pair failed or is not served, and the Score column says which.
    */
  def source(
      run: RunId,
      scale: ScaleIndex,
      label: String,
      entries: Vector[PairRowEntry],
      queries: Vector[QueryRow]
  ): Either[PlotSourceError, PlotSource] =
    val items = queries.map(q => q.query -> q.item).toMap
    for
      c   <- columns
      src <- PlotSource(
        PairsText(Caption, run.number.toString, label, entries.size.toString),
        Vector(
          PlotColumn(c.query, PairsText(Query), ColumnFormat.Label),
          PlotColumn(c.queryItem, PairsText(QueryItem), ColumnFormat.Label),
          PlotColumn(c.role, PairsText(Role), ColumnFormat.Label),
          PlotColumn(c.reference, PairsText(Reference), ColumnFormat.Label),
          PlotColumn(c.referenceItem, PairsText(ReferenceItem), ColumnFormat.Label),
          PlotColumn(c.sigma, PairsText(Sigma), ColumnFormat.Label),
          PlotColumn(c.cosine, PairsText(Cosine), ColumnFormat.Decimal(2)),
          PlotColumn(c.score, PairsText(Score), ColumnFormat.Label)
        ),
        entries.map { e =>
          val (cosine, score) = e.score match
            case PairScoreState.Scored(v) => (PlotValue.Number(v), PairsText(Scored))
            case PairScoreState.Failed(d) => (PlotValue.Missing, PairsText(Failed, d.message))
            case PairScoreState.NotServed => (PlotValue.Missing, PairsText(NotServed))
          PlotRow(
            StudioRef.Pair(run, scale, e.design, e.query, e.reference),
            Vector(
              PlotValue.Text(e.query.label),
              items.get(e.query).fold(PlotValue.Missing)(PlotValue.Text(_)),
              PlotValue.Text(PairsText(e.design match
                case PairDesign.Matched => Matched
                case PairDesign.Control => Control)),
              PlotValue.Text(e.reference.label),
              PlotValue.Text(e.referenceItem),
              PlotValue.Text(label),
              cosine,
              PlotValue.Text(score)
            )
          )
        }
      )
    yield src

  /** The table's view-model under the panels' `focus` (its reference pair,
    * if the trail names one, else the query's matched pair), with the run's
    * query rows and scale labels. Rows are shown only under the focus they
    * were read for: an answer for another run or scale reads as Reading, so
    * every row's label and ref come from the answer's own key.
    */
  def vm(
      s: PairsTable,
      focus: Option[PanelFocus],
      reference: Option[(PairDesign, eyes4s.studio.core.backend.TrialKey)],
      queries: Vector[QueryRow],
      scales: Vector[String]
  ): PairsVM = view(s, focus, reference, queries, scales)._2

  /** [[vm]], and the table with the source it built kept: a render that
    * changes neither the answer, its key, its label nor the query rows
    * reuses the source (by reference) instead of building every row again.
    */
  def view(
      s: PairsTable,
      focus: Option[PanelFocus],
      reference: Option[(PairDesign, eyes4s.studio.core.backend.TrialKey)],
      queries: Vector[QueryRow],
      scales: Vector[String]
  ): (PairsTable, PairsVM) =
    def labelOf(scale: ScaleIndex) = scales.lift(scale.value).getOrElse(scale.value.toString)
    def reading(f: PanelFocus)     =
      PairsVM(
        Some(PairsText(Reading, f.run.number.toString, labelOf(f.scale))),
        None,
        None,
        None
      )
    focus match
      case None => (s, PairsVM(Some(PairsText(NoQuery)), None, None, None))
      case Some(f) if !s.key.contains((f.run, f.scale)) => (s, reading(f))
      case Some(f)                                      =>
        s.answer match
          case None                        => (s, reading(f))
          case Some(PairsAnswer.Failed(e)) =>
            (
              s,
              PairsVM(
                Some(PairsText(Unreadable, e.message)),
                None,
                None,
                Some(PairsText(Retry))
              )
            )
          case Some(PairsAnswer.Broken(why)) =>
            (s, PairsVM(Some(PairsText(Broken, why)), None, None, Some(PairsText(Retry))))
          case Some(PairsAnswer.Answered(rows)) =>
            val key   = (f.run, f.scale)
            val label = labelOf(f.scale)
            val cache = s.cache
              .filter(_.fits(key, label, rows, queries))
              .getOrElse(
                PairsCache(
                  key,
                  label,
                  rows,
                  queries,
                  source(f.run, f.scale, label, rows, queries)
                )
              )
            val next = s.copy(cache = Some(cache))
            cache.source match
              case Left(e)    => (next, PairsVM(Some(e.message), None, None, None))
              case Right(src) =>
                val focused =
                  reference.map((d, r) => f.pair(d, r)).filter(src.rowOf(_).isDefined)
                (next, PairsVM(None, Some(src), focused, None))
