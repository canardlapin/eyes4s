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

package eyes4s.plan

import eyes4s.core.*
import eyes4s.kernel.*

/** Refusals of an initial-fixation policy, each naming its operands. */
enum InitialFixationError derives CanEqual:
  /** The disc around the fixation cross needs a positive, finite radius. */
  case NonPositiveRadius(radiusDegrees: Double)

  /** The fixation cross needs finite coordinates. */
  case NonFiniteCross(x: Double, y: Double)

  /** The fixation cross must lie on the plan's admission frame. */
  case CrossOffFrame(x: Double, y: Double, frame: FrameId)

  /** A radius in degrees needs the plan's declared units per degree. */
  case MissingAngularScale(radiusDegrees: Double)

  /** A stored tally must partition its trial: `dropped` of `total`. */
  case InvalidTally(dropped: Int, total: Int, droppedMicros: Long, totalMicros: Long)

  /** The policy dropped every fixation of a trial, so its map would be empty. */
  case NoFixationKept(dropped: Int, droppedMicros: Long)

  def message: String = this match
    case NonPositiveRadius(r) =>
      s"The disc around the fixation cross needs a positive, finite radius in degrees, got $r."
    case NonFiniteCross(x, y) =>
      s"The fixation cross needs finite coordinates, got ($x, $y)."
    case CrossOffFrame(x, y, frame) =>
      s"The fixation cross at ($x, $y) lies outside the admission frame ${frame.name}."
    case MissingAngularScale(r) =>
      s"Dropping fixations within $r degrees of the cross needs the plan's declared units " +
        "per degree, and the plan declares none."
    case InvalidTally(dropped, total, droppedMicros, totalMicros) =>
      s"An initial-fixation tally must partition its trial: dropped=$dropped of total=$total, " +
        s"droppedMicros=$droppedMicros of totalMicros=$totalMicros."
    case NoFixationKept(dropped, micros) =>
      s"The initial-fixation policy dropped all $dropped fixations ($micros microseconds); " +
        "the trial's map would be empty."

object InitialFixationError:
  given diagnose: Diagnose[InitialFixationError, Nothing] =
    new Diagnose[InitialFixationError, Nothing]:
      val family: DiagnosticFamily = DiagnosticCatalog.initialFixation
      def apply(error: InitialFixationError): Diagnostic[Nothing] =
        RevisionDiagnostics.initialFixation(error)

/** Which fixations at the start of every trial a study leaves out, applied
  * identically to focal (query) and reference trials, before the analysis
  * window and the screen are considered.
  *
  *  - `KeepAll`: none; the default, and what every version-1 and version-2
  *    plan means.
  *  - `DropFirst`: the first fixation of the trial, whatever its position.
  *  - `DropLeadingInClosedDisc`: the leading run of fixations whose centres lie
  *    in the closed disc of `radiusDegrees` around `cross` (admission-frame
  *    units), measured through the plan's one [[LinearAngularScale]]: the
  *    fixations before the first one whose centre lies farther than the
  *    radius from the cross. A later return to the cross is kept. The
  *    distance is `centre.distanceTo(cross) / unitsPerDegree`, the linear
  *    approximation the scale declares, and a centre exactly at the radius is
  *    within it. Built by [[InitialFixationPolicy.dropLeadingInClosedDisc]].
  */
sealed trait InitialFixationPolicy[U <: Unit2D] derives CanEqual:
  import InitialFixationPolicy.*

  /** The short label a review panel shows. */
  def render: String = this match
    case KeepAll()                     => "keep all"
    case DropFirst()                   => "drop first fixation"
    case DropLeadingInClosedDisc(_, r) =>
      s"drop leading fixations within ${Provenance.Param.Num(r).render}° of the cross"

  /** One sentence a methods section can cite. */
  def methods: String = this match
    case KeepAll()   => "All fixations of every trial were retained."
    case DropFirst() =>
      "The first fixation of every trial, query and reference alike, was excluded."
    case DropLeadingInClosedDisc(cross, r) =>
      s"Fixations at the start of every trial, query and reference alike, whose centres lay " +
        s"within ${Provenance.Param.Num(r).render}° of the fixation cross at " +
        s"(${Provenance.Param.Num(cross.x).render}, ${Provenance.Param.Num(cross.y).render}) " +
        "were excluded, up to the first fixation farther from it."

  /** Description parameters; a keep-all policy describes nothing. */
  def parameters: Vector[Provenance.Param] =
    import Provenance.Param.*
    this match
      case KeepAll()                         => Vector.empty
      case DropFirst()                       => Vector(Text("dropFirst"))
      case DropLeadingInClosedDisc(cross, r) =>
        Vector(Text("dropLeadingInClosedDisc"), Num(cross.x), Num(cross.y), Num(r))

  def isKeepAll: Boolean = this match
    case KeepAll() => true
    case _         => false

object InitialFixationPolicy:
  final case class KeepAll[U <: Unit2D]()   extends InitialFixationPolicy[U]
  final case class DropFirst[U <: Unit2D]() extends InitialFixationPolicy[U]
  final case class DropLeadingInClosedDisc[U <: Unit2D] private[plan] (
      cross: Pt[U],
      radiusDegrees: Double
  ) extends InitialFixationPolicy[U]

  def keepAll[U <: Unit2D]: InitialFixationPolicy[U]   = KeepAll()
  def dropFirst[U <: Unit2D]: InitialFixationPolicy[U] = DropFirst()

  /** Drop the leading run of fixations whose centres lie in the closed disc
    * of `radiusDegrees` around `cross`. The cross is checked against the
    * admission frame and the radius resolved through the plan's angular
    * scale when the plan is configured.
    */
  def dropLeadingInClosedDisc[U <: Unit2D](
      cross: Pt[U],
      radiusDegrees: Double
  ): Either[InitialFixationError, InitialFixationPolicy[U]] =
    if !radiusDegrees.isFinite || radiusDegrees <= 0.0 then
      Left(InitialFixationError.NonPositiveRadius(radiusDegrees))
    else if !cross.x.isFinite || !cross.y.isFinite then
      Left(InitialFixationError.NonFiniteCross(cross.x, cross.y))
    else Right(DropLeadingInClosedDisc(cross, radiusDegrees))

/** How the policy fell on one trial, by count and by duration: the leading
  * `dropped` of its `total` fixations were left out and the rest kept.
  */
final case class InitialFixationTally private (
    dropped: Int,
    total: Int,
    droppedDuration: Span,
    totalDuration: Span
) derives CanEqual:
  def kept: Int           = total - dropped
  def keptDuration: Span  = totalDuration - droppedDuration
  def anyDropped: Boolean = dropped > 0
  def render: String      =
    s"dropped $dropped of $total initial fixations (${droppedDuration.render} of " +
      s"${totalDuration.render})"

object InitialFixationTally:
  /** Rebuild a stored tally. It must partition its trial: the dropped
    * fixations lie within it, none dropped means no dropped duration, and
    * all dropped means all of the duration.
    */
  def of(
      dropped: Int,
      total: Int,
      droppedDuration: Span,
      totalDuration: Span
  ): Either[InitialFixationError, InitialFixationTally] =
    val counts    = dropped >= 0 && dropped <= total
    val durations = !droppedDuration.isNegative &&
      droppedDuration.toMicros <= totalDuration.toMicros &&
      (dropped != 0 || droppedDuration.toMicros == 0L) &&
      (dropped != total || droppedDuration.toMicros == totalDuration.toMicros)
    if counts && durations then
      Right(new InitialFixationTally(dropped, total, droppedDuration, totalDuration))
    else
      Left(
        InitialFixationError.InvalidTally(
          dropped,
          total,
          droppedDuration.toMicros,
          totalDuration.toMicros
        )
      )

  private[plan] def count[U <: Unit2D](path: Scanpath[U], dropped: Int): InitialFixationTally =
    var droppedDuration = Span.zero
    var totalDuration   = Span.zero
    var i               = 0
    while i < path.n do
      val d = path.fixations(i).duration
      totalDuration = totalDuration + d
      if i < dropped then droppedDuration = droppedDuration + d
      i += 1
    new InitialFixationTally(dropped, path.n, droppedDuration, totalDuration)

/** One trial under the policy: its tally and the scanpath of the fixations it
  * keeps, absent when it keeps none. Under `KeepAll`, and whenever nothing is
  * dropped, `kept` is the trial's own scanpath.
  */
final class InitialFixationOutcome[U <: Unit2D] private[plan] (
    val tally: InitialFixationTally,
    val kept: Option[Scanpath[U]]
)

/** A policy checked against a plan's admission frame and angular scale, so
  * applying it to a trial is total.
  */
final class InitialFixationRule[U <: Unit2D] private (
    val policy: InitialFixationPolicy[U],
    val unitsPerDegree: Option[Double],
    leading: Scanpath[U] => Int
):
  /** How many leading fixations of `path` the policy drops. */
  def dropCount(path: Scanpath[U]): Int = leading(path)

  /** The trial's tally and kept fixations. */
  def select(path: Scanpath[U]): InitialFixationOutcome[U] =
    val dropped = dropCount(path)
    new InitialFixationOutcome(
      InitialFixationTally.count(path, dropped),
      path.dropLeading(dropped)
    )

  /** One trial of a prepared study: the fixations it keeps, or the failure
    * that leaves its map empty.
    */
  private[plan] def kept[K](key: K, path: Scanpath[U]): Either[StudyFailure[K], Scanpath[U]] =
    val outcome = select(path)
    outcome.kept.toRight(
      StudyFailure.InitialFixations(
        key,
        InitialFixationError.NoFixationKept(
          outcome.tally.dropped,
          outcome.tally.droppedDuration.toMicros
        )
      )
    )

object InitialFixationRule:
  /** Check a policy against the admission frame and the plan's one angular
    * scale: the cross must lie on the frame, and a radius in degrees needs
    * the scale.
    */
  def of[U <: Unit2D](
      policy: InitialFixationPolicy[U],
      admission: Frame[U],
      scale: Option[LinearAngularScale[U]]
  ): Either[InitialFixationError, InitialFixationRule[U]] = policy match
    case InitialFixationPolicy.KeepAll() => Right(new InitialFixationRule(policy, None, _ => 0))
    case InitialFixationPolicy.DropFirst() =>
      Right(new InitialFixationRule(policy, None, path => math.min(1, path.n)))
    case InitialFixationPolicy.DropLeadingInClosedDisc(cross, radius) =>
      if !admission.contains(cross) then
        Left(InitialFixationError.CrossOffFrame(cross.x, cross.y, admission.id))
      else
        scale
          .toRight(InitialFixationError.MissingAngularScale(radius))
          .map { s =>
            val perDegree = s.unitsPerDegree
            new InitialFixationRule(
              policy,
              Some(perDegree),
              path =>
                var i = 0
                while i < path.n &&
                  path.fixations(i).centre.distanceTo(cross) / perDegree <= radius
                do i += 1
                i
            )
          }

/** Totals over a study's per-trial initial-fixation tallies. */
final case class InitialFixationSummary(
    dropped: Int,
    total: Int,
    trialsWithDrops: Int,
    trialsEmptied: Int,
    trials: Int,
    untallied: Int
) derives CanEqual

object InitialFixationSummary:
  def of[K](
      tallies: Vector[(K, Either[GeometryError, InitialFixationTally])]
  ): InitialFixationSummary =
    val counted = tallies.collect { case (_, Right(t)) => t }
    InitialFixationSummary(
      counted.map(_.dropped).sum,
      counted.map(_.total).sum,
      counted.count(_.anyDropped),
      counted.count(t => t.kept == 0),
      counted.size,
      tallies.size - counted.size
    )
