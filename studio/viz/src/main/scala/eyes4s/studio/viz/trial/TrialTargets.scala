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

package eyes4s.studio.viz.trial

import eyes4s.studio.app.plot.RovingMove
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.viz.plot.{DataPoint, RovingCursor, RovingTarget, RovingTargets, SceneId}
import intaglio.interaction.NamedPickingPlan
import intaglio.{DevicePoint, DeviceScene, GraphicsName, IntaglioError, ResolvedViewportFrame}

/** Why a trial's interaction targets could not be resolved or queried. Every
  * case names the scene and the value it refused.
  */
enum TrialTargetError derives CanEqual:

  /** The scene's data panel has no resolved viewport frame, or a mark's
    * position could not be mapped through it.
    */
  case Frame(scene: SceneId, what: String, error: IntaglioError)

  /** Intaglio's named picking plan refused a query. */
  case Picking(scene: SceneId, point: DevicePoint, error: IntaglioError)

  /** A device scale must be positive and finite. */
  case InvalidScale(scene: SceneId, deviceScale: Double)

  /** A pointer tolerance must be finite and not negative. */
  case InvalidTolerance(scene: SceneId, toleranceDevicePx: Double)

  def message: String = this match
    case Frame(id, what, e) => s"scene ${id.value}: $what: ${e.message}"
    case Picking(id, p, e)  =>
      s"scene ${id.value}: picking at device (${p.x}, ${p.y}) failed: ${e.message}"
    case InvalidScale(id, s) =>
      s"scene ${id.value}: device scale $s is not positive and finite"
    case InvalidTolerance(id, t) =>
      s"scene ${id.value}: pointer tolerance $t device px is not finite and non-negative"

/** A fixation mark as laid out on one surface: where Intaglio drew its centre
  * (device pixels) and its radius in device pixels.
  */
final case class MarkTarget(mark: TrialMark, anchor: DevicePoint, radiusDevicePx: Double)
    extends RovingTarget[StudioRef.Fixation] derives CanEqual:
  def ref: StudioRef.Fixation = mark.ref
  def reachPx: Double         = mark.reachPx
  def order: Int              = mark.order

/** The interaction targets of one trial scene on one surface (ticket S4.2).
  *
  * The marks are positioned through the resolved viewport frame of the
  * scene's data panel ([[TrialScene.PanelName]]), the frame Intaglio drew
  * them in, and a device position maps back to screen pixels through the
  * same frame's inverse. Pointer hits come from Intaglio's named picking plan
  * of the drawn scene; since every mark is its own named grob, a hit resolves
  * to exactly one fixation, and a hit on anything that is not a mark (the
  * image, the order lines, a casing's primary ring aside) is no fixation.
  */
final class TrialTargets private (
    val sceneId: SceneId,
    val frame: ResolvedViewportFrame,
    picking: NamedPickingPlan,
    val targets: Vector[MarkTarget],
    val deviceScale: Double
) extends RovingTargets[StudioRef.Fixation, TrialTargetError]:
  private val byName: Map[GraphicsName, MarkTarget] = targets.map(t => t.mark.name -> t).toMap
  private val byRef: Map[StudioRef, MarkTarget]     = targets.map(t => t.ref -> t).toMap

  /** The target of fixation `ref`, if this trial draws it. */
  def target(ref: StudioRef): Option[MarkTarget] = byRef.get(ref)

  /** The mark under `point` (device pixels): the nearest mark whose painted
    * geometry lies within `toleranceDevicePx`, of equally near ones the one
    * drawn last; failing that, the last-drawn mark whose centre lies within its
    * painted reach of `point`.
    *
    * The fallback makes the inside of a hollow mark (a control ring, a dashed
    * outside-window mark) pickable: Intaglio picks an unfilled outline on its
    * annulus only, which leaves a dead centre in marks larger than the
    * tolerance. It can go once Intaglio has an interior pick policy (intaglio
    * bd-01M3FQQHVHHCEC349JV46VM4FB).
    */
  def pick(
      point: DevicePoint,
      toleranceDevicePx: Double
  ): Either[TrialTargetError, Option[MarkTarget]] =
    if !toleranceDevicePx.isFinite || toleranceDevicePx < 0.0 then
      Left(TrialTargetError.InvalidTolerance(sceneId, toleranceDevicePx))
    else
      picking
        .hits(point, toleranceDevicePx)
        .left
        .map(TrialTargetError.Picking(sceneId, point, _))
        .map(_.iterator.flatMap(h => byName.get(h.name)).nextOption().orElse(inside(point)))

  // The last-drawn mark whose painted reach covers `point` (see `pick`).
  private def inside(point: DevicePoint): Option[MarkTarget] =
    RovingCursor.inside(targets, point, deviceScale)

  /** The screen position (data coordinates) drawn at `point`: the inverse of
    * the panel frame the marks were drawn through.
    */
  def toData(point: DevicePoint): Either[TrialTargetError, DataPoint] =
    frame
      .deviceToNative(point)
      .map(p => DataPoint(p.x, p.y))
      .left
      .map(TrialTargetError.Frame(sceneId, s"device point (${point.x}, ${point.y})", _))

  /** Where the roving cursor goes from `from` on `move`, or `None` if it stays.
    *
    * With no focus, every move starts at the first mark (`Last` at the last).
    * A directional move goes to the nearest mark strictly on that side, ties
    * broken by fixation order. Marks drawn at one position are a stack: Right
    * and Down step forward through it, Left and Up back, before leaving it, so
    * no stacked mark traps or hides from the cursor. `Next` and `Previous`
    * visit every mark in fixation order.
    */
  def step(from: Option[StudioRef], move: RovingMove): Option[MarkTarget] =
    RovingCursor.step(targets, from.flatMap(byRef.get), move)

object TrialTargets:

  /** Device positions this close (in device pixels) are one position. */
  val Epsilon: Double = RovingCursor.Epsilon

  /** The targets of `scene`, drawn as `device` at `deviceScale`, with the
    * named picking plan compiled from the same scene and render context.
    */
  def resolve(
      scene: TrialScene,
      device: DeviceScene,
      picking: NamedPickingPlan,
      deviceScale: Double
  ): Either[TrialTargetError, TrialTargets] =
    val id = scene.plot.id
    for
      _ <- Either.cond(
        deviceScale.isFinite && deviceScale > 0.0,
        (),
        TrialTargetError.InvalidScale(id, deviceScale)
      )
      panel <- GraphicsName(TrialScene.PanelName, "trial panel").left.map(e =>
        TrialTargetError.Frame(id, "the panel name", e)
      )
      resolved <- device
        .frame(panel)
        .left
        .map(TrialTargetError.Frame(id, s"the ${TrialScene.PanelName} frame", _))
      targets <- scene.marks.foldLeft[Either[TrialTargetError, Vector[MarkTarget]]](
        Right(Vector.empty)
      ) { (acc, m) =>
        for
          ts     <- acc
          anchor <- resolved
            .nativeToDevice(DevicePoint(m.at.x, m.at.y))
            .left
            .map(TrialTargetError.Frame(id, s"fixation ${m.ref.index.value}", _))
        yield ts :+ MarkTarget(m, anchor, m.radiusPx * deviceScale)
      }
    yield new TrialTargets(id, resolved, picking, targets, deviceScale)
