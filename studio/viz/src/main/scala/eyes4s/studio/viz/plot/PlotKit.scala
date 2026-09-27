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

import eyes4s.studio.app.plot.{ColumnId, PlotSource, PlotText, PlotTextId, RovingMove}
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

  /** A mark claims a row that holds another ref, or no row. */
  case MarkRow(plot: String, ref: StudioRef, row: Int)

  /** A row is drawn or set aside more than once, or neither. */
  case RowAccounting(plot: String, ref: StudioRef, times: Int)

  /** Two marks share a grob name, so a pick could not tell them apart. */
  case DuplicateMarkName(plot: String, name: String)

  /** Marks are listed out of order: mark `index` says it is `order`. */
  case MarkOrder(plot: String, index: Int, order: Int)

  def message: String = this match
    case MissingColumn(p, c)  => s"plot $p: the source has no column '${c.value}'"
    case NotNumeric(p, c)     => s"plot $p: column '${c.value}' is not numeric"
    case Scene(p, e)          => s"plot $p: ${e.message}"
    case Graphics(p, d, e)    => s"plot $p: Intaglio refused $d: ${e.message}"
    case MarkRow(p, ref, row) =>
      s"plot $p: a mark of $ref claims row $row, which is not its row"
    case RowAccounting(p, ref, n) =>
      s"plot $p: row $ref is drawn or set aside $n times; every row must be accounted for once"
    case DuplicateMarkName(p, n) => s"plot $p: two marks are named '$n'"
    case MarkOrder(p, i, o)      => s"plot $p: mark $i is listed with order $o"

/** One drawn mark of a plot: the row of the source it shows, where (in the
  * panel's data coordinates), how far its painted outline reaches from its
  * centre in logical pixels, its position in the plot's roving order (from
  * 0) and the name of its grob.
  *
  * Every mark is its own named grob, so a named picking plan resolves a hit
  * to one mark, as a trial's fixation marks do (S4.2).
  */
final case class PlotMark(
    ref: StudioRef,
    row: Int,
    at: DataPoint,
    reachPx: Double,
    order: Int,
    name: GraphicsName
) derives CanEqual

/** Why a row of the source has no mark. */
enum UnplottedReason derives CanEqual:
  /** The row has no value in a column the plot places marks by. */
  case MissingValue(column: ColumnId)

/** A row the plot does not draw, returned as data rather than dropped: the
  * table still lists it.
  */
final case class Unplotted(ref: StudioRef, row: Int, reason: UnplottedReason) derives CanEqual

/** A plot built from its value source (ticket S4.5a): the scene, one mark per
  * drawn row, and the rows it could not draw. Every row of `source` is
  * accounted for exactly once, as a mark or as [[Unplotted]], and marks are
  * listed in their roving order.
  *
  * `title` names the plot's tab; `description` is its accessible summary.
  */
final case class BuiltPlot private (
    source: PlotSource,
    plot: PlotScene,
    title: String,
    description: String,
    marks: Vector[PlotMark],
    unplotted: Vector[Unplotted]
):
  private lazy val byRef: Map[StudioRef, PlotMark]     = marks.map(m => m.ref -> m).toMap
  private lazy val byName: Map[GraphicsName, PlotMark] = marks.map(m => m.name -> m).toMap

  /** The mark of `ref`, if the plot draws its row. */
  def markOf(ref: StudioRef): Option[PlotMark] = byRef.get(ref)

  /** The mark drawn as grob `name`. */
  def markNamed(name: GraphicsName): Option[PlotMark] = byName.get(name)

  /** What a focused mark says: exactly the words of its table row, if the
    * mark's row is one of this plot's (every mark [[BuiltPlot.apply]] accepts).
    */
  def readout(mark: PlotMark): Option[String] = source.rowText(mark.row)

  /** The words for a row the plot does not draw, if it is one of this
    * plot's rows.
    */
  def unplottedText(u: Unplotted): Option[String] =
    val reason = u.reason match
      case UnplottedReason.MissingValue(c) =>
        val header = source.indexOf(c).flatMap(source.columns.lift).fold(c.value)(_.header)
        PlotText(PlotTextId.MissingValue, header)
    source.rowText(u.row).map(PlotText(PlotTextId.Unplotted, _, reason))

object BuiltPlot:

  /** A built plot, refusing marks that misstate their rows or order. */
  def apply(
      kind: String,
      source: PlotSource,
      plot: PlotScene,
      title: String,
      description: String,
      marks: Vector[PlotMark],
      unplotted: Vector[Unplotted]
  ): Either[PlotBuildError, BuiltPlot] =
    def rowOk(ref: StudioRef, row: Int) = source.rows.lift(row).exists(_.ref == ref)
    val claims = marks.map(m => (m.ref, m.row)) ++ unplotted.map(u => (u.ref, u.row))
    val counts = claims.groupMapReduce(_._1)(_ => 1)(_ + _)
    for
      _ <- claims.find(!rowOk.tupled(_)).map(PlotBuildError.MarkRow(kind, _, _)).toLeft(())
      _ <- source.rows
        .map(r => r.ref -> counts.getOrElse(r.ref, 0))
        .find(_._2 != 1)
        .map(PlotBuildError.RowAccounting(kind, _, _))
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
    yield new BuiltPlot(source, plot, title, description, marks, unplotted)

/** A kind of plot: builds its scene from a value source (ticket S4.5a).
  *
  * The scale ladder, participant, scale profile and timeline plots (S4.5b–e)
  * are builders; the plot host shows any of them with its TableTwin, the
  * roving cursor and the shared selection. A builder computes no science: it
  * places the source's values and draws them in token colours.
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
  def ref: StudioRef  = mark.ref
  def reachPx: Double = mark.reachPx
  def order: Int      = mark.order

/** The interaction targets of a built plot on one surface (ticket S4.5a).
  *
  * Marks are placed through the plot's [[PlotTransform]], the one mapping the
  * scene was drawn with, and pointer hits come from the named picking plan of
  * the same scene and render context: since every mark is its own named
  * grob, a hit resolves to one row.
  */
final class PlotTargets private (
    val plot: BuiltPlot,
    val transform: PlotTransform,
    picking: NamedPickingPlan,
    val targets: Vector[PlotTarget]
) extends RovingTargets[StudioRef, PlotTargetError]:
  private val byName: Map[GraphicsName, PlotTarget] = targets.map(t => t.mark.name -> t).toMap
  private val byRef: Map[StudioRef, PlotTarget]     = targets.map(t => t.ref -> t).toMap

  def sceneId: SceneId    = plot.plot.id
  def deviceScale: Double = transform.surface.deviceScale

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
    * what its table row says ([[BuiltPlot.readout]]), marked when selected;
    * with no mark focused, the plot's description.
    */
  def accessibleText(state: MarkInputState[StudioRef]): String =
    state.spoken(
      this,
      (ref, selected) =>
        target(ref)
          .flatMap(t => plot.readout(t.mark))
          .fold(plot.description)(PlotText.selected(_, selected)),
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
          ts     <- acc
          anchor <- transform.dataToDevice(m.at).left.map(PlotTargetError.Frame(id, _))
        yield ts :+ PlotTarget(m, anchor)
      }
    yield new PlotTargets(plot, transform, picking, targets)
