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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.plot.{
  ColumnId,
  PlotSource,
  PlotText,
  PlotTextId,
  PlotValue,
  RovingMove
}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.StudioRef
import intaglio.interaction.NamedPickingPlan
import intaglio.{DevicePoint, GraphicsError, GraphicsName, IntaglioError, value}

/** Why a plot could not be built from its source. Every case names the plot
  * kind and the column, row or mark it refused.
  */
enum PlotBuildError derives CanEqual:

  /** The builder encodes a column the source does not have. */
  case MissingColumn(plot: String, column: ColumnId)

  /** The builder places marks by a column whose values are not numbers. */
  case NotNumeric(plot: String, column: ColumnId)

  /** The scene, its panel or its identity was refused. */
  case Scene(plot: String, error: PlotSceneError)

  /** Intaglio refused a value while `during`. */
  case Graphics(plot: String, during: String, error: GraphicsError)

  /** A mark or an unplotted row claims row `row` for `ref`, which is not its row. */
  case MarkRow(plot: String, ref: StudioRef, row: Int)

  /** A row is drawn, represented or set aside `times` times; once is required. */
  case RowAccounting(plot: String, ref: StudioRef, times: Int)

  /** Two marks share a grob name, so a pick could not tell them apart. */
  case DuplicateMarkName(plot: String, name: String)

  /** Marks are listed out of order: mark `index` says it is `order`. */
  case MarkOrder(plot: String, index: Int, order: Int)

  /** Mark `name` accounts for no row. */
  case EmptyMark(plot: String, name: String)

  /** Row `ref` has no position because of column `column`, which the source lacks. */
  case ReasonColumn(plot: String, ref: StudioRef, column: ColumnId)

  /** Row `ref`'s reason for having no position misstates its cell in `column`:
    * a missing value that is present, or an off-scale value that is not the
    * cell's.
    */
  case ReasonValue(plot: String, ref: StudioRef, column: ColumnId)

  /** Row `ref` is placed at `placed` on the axis of `column`, which is not
    * where the axis puts its cell.
    */
  case Position(plot: String, ref: StudioRef, column: ColumnId, placed: Double)

  /** Mark `name` is nudged by a non-finite offset. */
  case Nudge(plot: String, name: String, dxPx: Double, dyPx: Double)

  /** A category axis of `column` lists `level` more than once. */
  case DuplicateLevel(plot: String, column: ColumnId, level: String)

  /** One displayed level names distinct nominal result groups. */
  case AmbiguousLevel(
      plot: String,
      column: ColumnId,
      level: String,
      identities: Vector[StudioRef]
  )

  /** A singleton mark placed at its row's value cannot replace its row's words.
    */
  case SummaryOfOne(plot: String, name: String)

  /** Mark `name` is given a summary with no words. */
  case BlankSummary(plot: String, name: String)

  /** The source has row `ref`, which is not a kind of row the plot draws. */
  case UnexpectedRow(plot: String, ref: StudioRef)

  def message: String = this match
    case MissingColumn(p, c)  => s"plot $p: the source has no column '${c.value}'"
    case NotNumeric(p, c)     => s"plot $p: column '${c.value}' is not numeric"
    case Scene(p, e)          => s"plot $p: ${e.message}"
    case Graphics(p, d, e)    => s"plot $p: Intaglio refused $d: ${e.message}"
    case MarkRow(p, ref, row) =>
      s"plot $p: a mark of $ref claims row $row, which is not its row"
    case RowAccounting(p, ref, n) =>
      s"plot $p: row $ref is drawn, represented or set aside $n times; every row must be " +
        "accounted for once"
    case DuplicateMarkName(p, n) => s"plot $p: two marks are named '$n'"
    case MarkOrder(p, i, o)      => s"plot $p: mark $i is listed with order $o"
    case EmptyMark(p, n)         => s"plot $p: mark '$n' stands for no row"
    case ReasonColumn(p, ref, c) =>
      s"plot $p: row $ref has no position for column '${c.value}', which the source lacks"
    case ReasonValue(p, ref, c) =>
      s"plot $p: row $ref's reason for having no position misstates its '${c.value}' cell"
    case Position(p, ref, c, v) =>
      s"plot $p: row $ref is placed at $v on the '${c.value}' axis, which is not where its cell is"
    case DuplicateLevel(p, c, l)      => s"plot $p: the '${c.value}' axis lists '$l' twice"
    case AmbiguousLevel(p, c, l, ids) =>
      s"plot $p: '${c.value}' level '$l' names distinct result groups: ${ids.mkString(", ")}"
    case Nudge(p, n, dx, dy) => s"plot $p: mark '$n' is nudged by ($dx, $dy) px"
    case SummaryOfOne(p, n)  =>
      s"plot $p: mark '$n' places one row, so its readout is that row, not a summary"
    case BlankSummary(p, n)    => s"plot $p: mark '$n' is given a blank summary"
    case UnexpectedRow(p, ref) => s"plot $p: row $ref is not a kind of row this plot draws"

/** Why a row of the source has no position in a plot. The row is then drawn
  * as a positionless mark ([[RowMarking.Positionless]]) or set aside
  * ([[Unplotted]]); either way the table still lists it.
  */
enum NoPosition derives CanEqual:

  /** The row has no value in a column the plot places marks by. Missing is
    * not zero: a positionless mark for it is drawn differently from a value.
    */
  case MissingValue(column: ColumnId)

  /** The row's `value` in `column` has no place on the plot's scale, as a
    * σ at or below zero on a log axis (S4.5d) or a time outside the
    * timeline's span (S4.5e).
    */
  case OffScale(column: ColumnId, value: Double)

object NoPosition:

  /** The column whose value leaves the row without a position. */
  def columnOf(reason: NoPosition): ColumnId = reason match
    case MissingValue(c) => c
    case OffScale(c, _)  => c

/** How an axis turns a number into a data coordinate. */
enum AxisScale derives CanEqual:

  /** The number itself. */
  case Linear

  /** The base-10 logarithm of a positive number, as the scale profile's σ
    * axis (S4.5d); a number at or below zero is off the scale.
    */
  case Log10

/** How one axis of a plot places a row: the column it reads and how its
  * cell becomes a data coordinate.
  */
enum Axis derives CanEqual:

  /** The cell's number, on `scale`. */
  case Numeric(column: ColumnId, scale: AxisScale)

  /** The index in `levels` of the cell as the table writes it, as
    * Remembered and Forgotten on the participant plot (S4.5c).
    */
  case Category(column: ColumnId, levels: Vector[String])

object Axis:

  /** The column `axis` reads. */
  def columnOf(axis: Axis): ColumnId = axis match
    case Numeric(c, _)  => c
    case Category(c, _) => c

  /** The data coordinate `axis` gives the row at `row`, if its cell has one. */
  def position(axis: Axis, source: PlotSource, row: Int): Option[Double] =
    axis match
      case Numeric(c, AxisScale.Linear) => source.number(row, c)
      case Numeric(c, AxisScale.Log10)  =>
        source.number(row, c).filter(_ > 0.0).map(math.log10)
      case Category(c, levels) =>
        source
          .indexOf(c)
          .flatMap(source.text(row, _))
          .map(levels.indexOf(_))
          .filter(_ >= 0)
          .map(_.toDouble)

/** Where a plot places a row: its x and y axes. A [[RowMarking.Placed]] row
  * sits exactly where both put its cells, and [[BuiltPlot.apply]] refuses
  * one that does not.
  */
final case class PositionEncoding(x: Axis, y: Axis) derives CanEqual:

  /** Where the encoding puts the row at `row`, if both its cells place it. */
  def place(source: PlotSource, row: Int): Option[DataPoint] =
    for
      px <- Axis.position(x, source, row)
      py <- Axis.position(y, source, row)
    yield DataPoint(px, py)

/** How a mark shows one row of its source. */
enum RowMarking derives CanEqual:

  /** The mark shows the row at `at`, the data point the plot's
    * [[PositionEncoding]] gives the row's cells. Only these rows are held to
    * position parity, and [[BuiltPlot.apply]] checks every one.
    */
  case Placed(at: DataPoint)

  /** The mark is drawn for the row, but the row's values give it no
    * position, as a dashed empty marker for a missing value (S4.5c).
    */
  case Positionless(reason: NoPosition)

  /** The mark stands for the row among others at a position that is not the
    * row's own, as a histogram bar for the controls in its bin (S4.5b).
    */
  case Represented

/** One row a mark accounts for: its ref, its index in the source and how
  * the mark shows it.
  */
final case class MarkedRow(ref: StudioRef, row: Int, marking: RowMarking) derives CanEqual

/** A finite offset in logical pixels on the device axes (y down). */
final case class PixelOffset private (dxPx: Double, dyPx: Double) derives CanEqual

object PixelOffset:
  val Zero: PixelOffset = PixelOffset(0.0, 0.0)

  /** The offset, if both parts are finite. */
  def of(dxPx: Double, dyPx: Double): Option[PixelOffset] =
    Option.when(dxPx.isFinite && dyPx.isFinite)(PixelOffset(dxPx, dyPx))

/** One drawn mark of a plot: the rows it accounts for (at least one), its
  * anchor `at` in the panel's data coordinates, the pixel nudge `nudgePx`
  * its grob is drawn with away from the anchor (a beeswarm's spread, S4.5b;
  * zero unless [[nudged]]), how far its painted outline reaches from its
  * drawn centre in logical pixels, its position in the plot's roving order
  * (from 0), the name of its grob, and an optional `summary`
  * ([[summarised]]). The roving cursor and the feedback rings centre on the
  * drawn centre, the anchor moved by the nudge.
  *
  * A mark is keyed by the ref of its first row ([[ref]]): the roving
  * cursor's focus and a feedback ring name that ref. Selecting the mark
  * selects all its rows ([[refs]]). A mark of one placed row is anchored at
  * that row's position.
  *
  * Every mark is its own named grob, so a named picking plan resolves a hit
  * to one mark, as a trial's fixation marks do (S4.2).
  */
final case class PlotMark private (
    first: MarkedRow,
    rest: Vector[MarkedRow],
    at: DataPoint,
    reachPx: Double,
    order: Int,
    name: GraphicsName,
    nudgePx: PixelOffset,
    summary: Option[String] = None
) derives CanEqual:

  /** Every row the mark accounts for, first row first. */
  def rows: Vector[MarkedRow] = first +: rest

  /** The mark's key: the ref of its first row. */
  def ref: StudioRef = first.ref

  /** The refs of every row: what selecting the mark selects. */
  def refs: Vector[StudioRef] = rows.map(_.ref)

  /** The same mark at roving position `order`. */
  def withOrder(order: Int): PlotMark = copy(order = order)

  /** The same mark drawn as grob `name`. */
  def withName(name: GraphicsName): PlotMark = copy(name = name)

  /** The same mark drawn `dxPx` right and `dyPx` down of its anchor, refusing
    * a non-finite nudge.
    */
  def nudged(kind: String, dxPx: Double, dyPx: Double): Either[PlotBuildError, PlotMark] =
    PixelOffset
      .of(dxPx, dyPx)
      .toRight(PlotBuildError.Nudge(kind, name.value, dxPx, dyPx))
      .map(n => copy(nudgePx = n))

  /** The same mark, whose readout names how many rows it stands for and
    * then says `text` rather than every row's words, as a histogram bar says
    * which bin it is instead of listing sixty controls (S4.5b). The table
    * still lists every row. For a represented singleton (a bin with one
    * member), appends the summary to its row's words. Refuses other singleton
    * summaries and a blank summary.
    */
  def summarised(kind: String, text: String): Either[PlotBuildError, PlotMark] =
    if rest.isEmpty && first.marking != RowMarking.Represented then
      Left(PlotBuildError.SummaryOfOne(kind, name.value))
    else if text.trim.isEmpty then Left(PlotBuildError.BlankSummary(kind, name.value))
    else Right(copy(summary = Some(text)))

object PlotMark:

  /** A mark of one row, placed at the row's values `at`. */
  def placed(
      ref: StudioRef,
      row: Int,
      at: DataPoint,
      reachPx: Double,
      order: Int,
      name: GraphicsName
  ): PlotMark =
    PlotMark(
      MarkedRow(ref, row, RowMarking.Placed(at)),
      Vector.empty,
      at,
      reachPx,
      order,
      name,
      PixelOffset.Zero
    )

  /** A mark of one row whose values give it no position, drawn at `at`. */
  def positionless(
      ref: StudioRef,
      row: Int,
      reason: NoPosition,
      at: DataPoint,
      reachPx: Double,
      order: Int,
      name: GraphicsName
  ): PlotMark =
    PlotMark(
      MarkedRow(ref, row, RowMarking.Positionless(reason)),
      Vector.empty,
      at,
      reachPx,
      order,
      name,
      PixelOffset.Zero
    )

  /** A mark for several rows (an aggregate), refusing one for none. */
  def of(
      kind: String,
      rows: Vector[MarkedRow],
      at: DataPoint,
      reachPx: Double,
      order: Int,
      name: GraphicsName
  ): Either[PlotBuildError, PlotMark] =
    rows match
      case first +: rest =>
        Right(PlotMark(first, rest, at, reachPx, order, name, PixelOffset.Zero))
      case _ => Left(PlotBuildError.EmptyMark(kind, name.value))

/** A row the plot neither draws nor represents, returned as data rather
  * than dropped: the table still lists it.
  */
final case class Unplotted(ref: StudioRef, row: Int, reason: NoPosition) derives CanEqual

/** A plot built from its value source (tickets S4.5a and S4.5x): the scene,
  * its marks in roving order, and the rows it set aside.
  *
  * The accounting is exact: every row of `source` appears exactly once,
  * either in one mark's [[PlotMark.rows]] (placed at its values, drawn
  * without a position, or represented by an aggregate) or as [[Unplotted]]
  * with its reason. A value a plot shows but no row holds (a group mean, a
  * grand-mean tick) is a row of the source with its own ref, such as a
  * [[StudioRef.GroupCell]] beside its [[StudioRef.ParticipantSummary]] rows,
  * never a number the builder computes.
  *
  * Selection goes through rows: selecting a mark selects every row it
  * accounts for, and a selected row rings the one mark that accounts for
  * it ([[markOf]]).
  *
  * `title` names the plot's tab; `description` is its accessible summary.
  */
final case class BuiltPlot private (
    source: PlotSource,
    plot: PlotScene,
    title: String,
    description: String,
    encoding: PositionEncoding,
    marks: Vector[PlotMark],
    unplotted: Vector[Unplotted]
):
  private lazy val byRef: Map[StudioRef, PlotMark] =
    marks.flatMap(m => m.refs.map(_ -> m)).toMap
  private lazy val byName: Map[GraphicsName, PlotMark] = marks.map(m => m.name -> m).toMap

  /** The mark that accounts for the row of `ref`, if the plot draws or
    * represents it.
    */
  def markOf(ref: StudioRef): Option[PlotMark] = byRef.get(ref)

  /** The mark drawn as grob `name`. */
  def markNamed(name: GraphicsName): Option[PlotMark] = byName.get(name)

  /** What a focused mark says, if its rows are this plot's (every mark
    * [[BuiltPlot.apply]] accepts): a mark of one row says exactly the words
    * of its table row, followed by any represented singleton's summary;
    * a mark of several says how many and then its
    * [[PlotMark.summary]], or each row's words when it has none.
    */
  def readout(mark: PlotMark): Option[String] =
    mark.rows match
      case Vector(one) =>
        source.rowText(one.row).map { text =>
          mark.summary.fold(text)(summary => text + PlotText(PlotTextId.RowSeparator) + summary)
        }
      case rows =>
        rows
          .foldLeft[Option[Vector[String]]](Some(Vector.empty)) { (acc, r) =>
            acc.flatMap(ts => source.rowText(r.row).map(ts :+ _))
          }
          .map(ts =>
            PlotText(
              PlotTextId.MarkRows,
              ts.size.toString,
              mark.summary.getOrElse(ts.mkString(PlotText(PlotTextId.RowSeparator)))
            )
          )

  /** The words for a row the plot does not draw, if it is one of this
    * plot's rows.
    */
  def unplottedText(u: Unplotted): Option[String] =
    source.rowText(u.row).map(PlotText(PlotTextId.Unplotted, _, reasonText(u.reason)))

  /** Why a row has no position, in the source's headers and formats. */
  def reasonText(reason: NoPosition): String =
    val id     = NoPosition.columnOf(reason)
    val column = source.indexOf(id).flatMap(source.columns.lift)
    val header = column.fold(id.value)(_.header)
    reason match
      case NoPosition.MissingValue(_) => PlotText(PlotTextId.MissingValue, header)
      case NoPosition.OffScale(_, v)  =>
        val written =
          column.fold(v.toString)(c => PlotSource.write(PlotValue.Number(v), c.format))
        PlotText(PlotTextId.OffScale, header, written)

object BuiltPlot:

  /** A built plot, refusing marks that misstate their rows, an axis column
    * the source lacks or a category listed twice, a row accounted for other
    * than once, a reason naming a column the source lacks or misstating its
    * cell, a placed row away from where `encoding` puts it, shared grob names
    * and marks out of order.
    */
  def apply(
      kind: String,
      source: PlotSource,
      plot: PlotScene,
      title: String,
      description: String,
      encoding: PositionEncoding,
      marks: Vector[PlotMark],
      unplotted: Vector[Unplotted]
  ): Either[PlotBuildError, BuiltPlot] =
    def rowOk(ref: StudioRef, row: Int) = source.rows.lift(row).exists(_.ref == ref)
    val axes                            = Vector(encoding.x, encoding.y)
    val marked                          = marks.flatMap(_.rows)
    val claims  = marked.map(r => (r.ref, r.row)) ++ unplotted.map(u => (u.ref, u.row))
    val counts  = claims.groupMapReduce(_._1)(_ => 1)(_ + _)
    val reasons = marked.collect { case MarkedRow(ref, row, RowMarking.Positionless(why)) =>
      (ref, row, why)
    } ++ unplotted.map(u => (u.ref, u.row, u.reason))
    def reasonHolds(row: Int, why: NoPosition) = why match
      case NoPosition.MissingValue(c) => source.value(row, c).contains(PlotValue.Missing)
      case NoPosition.OffScale(c, v)  => source.number(row, c).contains(v)
    // The first axis whose coordinate for a placed row is not the row's.
    def misplaced(r: MarkedRow, at: DataPoint) =
      Vector((encoding.x, at.x), (encoding.y, at.y))
        .find((axis, v) => !Axis.position(axis, source, r.row).contains(v))
        .map((axis, v) => PlotBuildError.Position(kind, r.ref, Axis.columnOf(axis), v))
    for
      _ <- claims.find(!rowOk.tupled(_)).map(PlotBuildError.MarkRow(kind, _, _)).toLeft(())
      _ <- axes
        .map(Axis.columnOf)
        .find(source.indexOf(_).isEmpty)
        .map(PlotBuildError.MissingColumn(kind, _))
        .toLeft(())
      _ <- axes
        .collectFirst {
          case Axis.Category(c, levels) if levels.distinct.size != levels.size =>
            PlotBuildError.DuplicateLevel(
              kind,
              c,
              levels.diff(levels.distinct).take(1).mkString
            )
        }
        .toLeft(())
      _ <- source.rows
        .map(r => r.ref -> counts.getOrElse(r.ref, 0))
        .find(_._2 != 1)
        .map(PlotBuildError.RowAccounting(kind, _, _))
        .toLeft(())
      _ <- reasons
        .map((ref, _, why) => (ref, NoPosition.columnOf(why)))
        .find((_, c) => source.indexOf(c).isEmpty)
        .map(PlotBuildError.ReasonColumn(kind, _, _))
        .toLeft(())
      _ <- reasons
        .find((_, row, why) => !reasonHolds(row, why))
        .map((ref, _, why) => PlotBuildError.ReasonValue(kind, ref, NoPosition.columnOf(why)))
        .toLeft(())
      _ <- marked.iterator
        .flatMap(r =>
          r.marking match
            case RowMarking.Placed(at) => misplaced(r, at)
            case _                     => None
        )
        .nextOption()
        .toLeft(())
      _ <- marks
        .map(_.name)
        .groupBy(identity)
        .collectFirst {
          case (n, ns) if ns.size > 1 => PlotBuildError.DuplicateMarkName(kind, n.value)
        }
        .toLeft(())
      _ <- marks.zipWithIndex
        .find((m, i) => m.order != i)
        .map((m, i) => PlotBuildError.MarkOrder(kind, i, m.order))
        .toLeft(())
    yield new BuiltPlot(source, plot, title, description, encoding, marks, unplotted)

/** A kind of plot: builds its scene from a value source (tickets S4.5a and
  * S4.5x).
  *
  * The scale ladder, participant, scale profile and timeline plots (S4.5b–e)
  * are builders; the plot host shows any of them with its TableTwin, the
  * roving cursor and the shared selection. A builder computes no science: it
  * places the source's values and draws them in token colours.
  *
  * A builder declares where it places rows, as a [[PositionEncoding]] of
  * two axes ([[Axis.Numeric]] on a linear or log scale, or
  * [[Axis.Category]]), and passes it to [[BuiltPlot.apply]], which keeps it
  * as [[BuiltPlot.encoding]]. It accounts for every row of the source exactly
  * once, and the built plot checks all of it:
  *
  *  - a row drawn at its own values is a [[RowMarking.Placed]] row of a
  *    mark, at exactly the data point the encoding gives its cells
  *    ([[PositionEncoding.place]]): a dot is a mark of one placed row
  *    ([[PlotMark.placed]]); a line
  *    through one participant's scales is one mark of several placed rows
  *    ([[PlotMark.of]]);
  *  - a row drawn without a position, as a dashed empty marker for a
  *    missing value, is a [[RowMarking.Positionless]] mark
  *    ([[PlotMark.positionless]]): it is focusable and selectable and its
  *    table row is listed, but it is held to no position;
  *  - rows one mark stands for, as a histogram bar for the controls in its
  *    bin, are [[RowMarking.Represented]] rows of one aggregate mark;
  *  - a row with no mark at all is [[Unplotted]], with its reason.
  *
  * A reason must hold of its row: [[NoPosition.MissingValue]] names a
  * missing cell and [[NoPosition.OffScale]] the cell's own number.
  *
  * Values the plot shows that are not per-row, such as group means or
  * grand-mean ticks, come as rows of the source with their own refs and are
  * drawn by their own marks. Decorations that stand for no row (a zero rule,
  * the line joining a linked pair) are plain grobs, not marks.
  */
trait PlotBuilder:

  /** What the builder draws, for scene ids and errors ("dot-plot"). */
  def kind: String

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot]

/** Why a plot's interaction targets could not be resolved or queried. */
enum PlotTargetError derives CanEqual:

  /** The frame was compiled from another scene than the plot's. */
  case SceneMismatch(plot: SceneId, frame: SceneId)

  /** A mark's position could not be mapped through the plot's transform. */
  case Frame(scene: SceneId, error: PlotSceneError)

  /** Intaglio's named picking plan refused a query. */
  case Picking(scene: SceneId, point: DevicePoint, error: IntaglioError)

  /** A pointer tolerance must be finite and not negative. */
  case InvalidTolerance(scene: SceneId, toleranceDevicePx: Double)

  def message: String = this match
    case SceneMismatch(p, f) =>
      s"plot scene ${p.value}: the frame on the canvas was compiled from ${f.value}"
    case Frame(id, e)      => s"scene ${id.value}: ${e.message}"
    case Picking(id, p, e) =>
      s"scene ${id.value}: picking at device (${p.x}, ${p.y}) failed: ${e.message}"
    case InvalidTolerance(id, t) =>
      s"scene ${id.value}: pointer tolerance $t device px is not finite and non-negative"

/** A plot mark as laid out on one surface. */
final case class PlotTarget(mark: PlotMark, anchor: DevicePoint) extends RovingTarget[StudioRef]
    derives CanEqual:
  def ref: StudioRef                   = mark.ref
  override def refs: Vector[StudioRef] = mark.refs
  def reachPx: Double                  = mark.reachPx
  def order: Int                       = mark.order

/** The interaction targets of a built plot on one surface (ticket S4.5a).
  *
  * Marks are placed through the plot's [[PlotTransform]], the one mapping the
  * scene was drawn with, then moved by their [[PlotMark.nudgePx]] at the
  * surface's device scale, so nudged marks at one data point are apart for
  * the roving cursor as on screen. Pointer hits come from the named picking
  * plan of the same scene and render context: since every mark is its own
  * named grob, a hit resolves to one mark.
  */
final class PlotTargets private (
    val plot: BuiltPlot,
    val transform: PlotTransform,
    picking: NamedPickingPlan,
    val targets: Vector[PlotTarget]
) extends RovingTargets[StudioRef, PlotTargetError]:
  private val byName: Map[GraphicsName, PlotTarget] = targets.map(t => t.mark.name -> t).toMap
  private val byRef: Map[StudioRef, PlotTarget]     =
    targets.flatMap(t => t.refs.map(_ -> t)).toMap

  def sceneId: SceneId    = plot.plot.id
  def deviceScale: Double = transform.surface.deviceScale

  /** The target of the mark that accounts for the row of `ref`. */
  def target(ref: StudioRef): Option[PlotTarget] = byRef.get(ref)

  /** The mark under `point` (device pixels): the nearest mark whose painted
    * geometry lies within `toleranceDevicePx`, of equally near ones the one
    * drawn last; failing that, the last-drawn mark whose painted reach covers
    * `point` ([[RovingCursor.inside]]).
    */
  def pick(
      point: DevicePoint,
      toleranceDevicePx: Double
  ): Either[PlotTargetError, Option[PlotTarget]] =
    if !toleranceDevicePx.isFinite || toleranceDevicePx < 0.0 then
      Left(PlotTargetError.InvalidTolerance(sceneId, toleranceDevicePx))
    else
      picking
        .hits(point, toleranceDevicePx)
        .left
        .map(PlotTargetError.Picking(sceneId, point, _))
        .map(
          _.iterator
            .flatMap(h => byName.get(h.name))
            .nextOption()
            .orElse(RovingCursor.inside(targets, point, deviceScale))
        )

  def step(from: Option[StudioRef], move: RovingMove): Option[PlotTarget] =
    RovingCursor.step(targets, from.flatMap(byRef.get), move)

  /** The plot's accessible text under `state`: the focused mark says exactly
    * what its table rows say ([[BuiltPlot.readout]]), marked as selected when
    * all its rows are and with how many when some are; with no mark focused,
    * the plot's description.
    */
  def accessibleText(state: MarkInputState[StudioRef]): String =
    state.spoken(
      this,
      (ref, share) =>
        target(ref)
          .flatMap(t => plot.readout(t.mark))
          .fold(plot.description) { said =>
            share match
              case SelectionShare.Unselected   => said
              case SelectionShare.All          => PlotText.selected(said, true)
              case SelectionShare.Partly(k, n) =>
                PlotText(PlotTextId.PartlySelected, said, k.toString, n.toString)
          },
      plot.description
    )

object PlotTargets:

  /** The targets of `plot` drawn with `transform`, picked through `picking`,
    * the plan compiled from the same scene and context.
    */
  def resolve(
      plot: BuiltPlot,
      transform: PlotTransform,
      picking: NamedPickingPlan
  ): Either[PlotTargetError, PlotTargets] =
    val id = plot.plot.id
    for
      _ <- Either.cond(
        transform.sceneId == id,
        (),
        PlotTargetError.SceneMismatch(id, transform.sceneId)
      )
      targets <- plot.marks.foldLeft[Either[PlotTargetError, Vector[PlotTarget]]](
        Right(Vector.empty)
      ) { (acc, m) =>
        for
          ts <- acc
          at <- transform.dataToDevice(m.at).left.map(PlotTargetError.Frame(id, _))
          k = transform.surface.deviceScale
        yield ts :+ PlotTarget(
          m,
          DevicePoint(at.x + m.nudgePx.dxPx * k, at.y + m.nudgePx.dyPx * k)
        )
      }
    yield new PlotTargets(plot, transform, picking, targets)
