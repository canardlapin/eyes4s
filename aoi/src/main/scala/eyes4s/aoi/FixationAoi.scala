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

package eyes4s.aoi

import eyes4s.core.*
import eyes4s.kernel.*

import scala.collection.mutable

/** Membership of one fixation centre. Off-surface fixations are retained explicitly;
  * a scanpath alone cannot classify the gaps between fixations as blinks or saccades.
  */
sealed trait FixationMembership derives CanEqual

object FixationMembership:
  /** Non-empty area identities in set order, constructed only by checked assignment. */
  final class Areas private (val ids: Vector[AoiId]) extends FixationMembership:
    override def equals(other: Any): Boolean = other match
      case that: Areas => ids == that.ids
      case _           => false
    override def hashCode: Int    = ids.hashCode
    override def toString: String = s"Areas($ids)"

  object Areas:
    private[aoi] def containing(ids: Vector[AoiId]): Areas = new Areas(ids)
    def unapply(value: Areas): Some[Vector[AoiId]]         = Some(value.ids)

  case object Background extends FixationMembership
  case object OffSurface extends FixationMembership

/** Exact microsecond accounting over the scanpath's extent. Fixation dwell and the
  * time between fixations remain distinct. Multiple membership increases duplicated
  * time without increasing the union or the analysable denominator.
  */
final class FixationAoiReport private[aoi] (
    val aoiUnionTime: Span,
    val backgroundTime: Span,
    val offSurfaceTime: Span,
    val interFixationTime: Span,
    val duplicatedAoiTime: Span
):
  def analysableTime: Span = aoiUnionTime + backgroundTime
  def fixationTime: Span   = analysableTime + offSurfaceTime
  def accountedTime: Span  = fixationTime + interFixationTime

/** One area's fixation-level measures. Dwell sums only contained fixation durations;
  * its proportion uses all on-surface fixation duration, including background, and is
  * absent if that denominator is zero. First entry is relative to the first fixation
  * onset. A visit is a maximal consecutive run in fixation order, irrespective of
  * intervening non-fixation time (bead bd-01M2R18YTC8BF4T1890QX991RW).
  *
  * Each visit starts with an entry, including a contained first fixation, so
  * `entryCount == visitCount`. `fixationCount` counts every contained fixation.
  */
final class FixationAoiMetric private[aoi] (
    val id: AoiId,
    val label: String,
    val dwell: Span,
    val dwellProportion: Option[Double],
    val firstEntryLatency: Option[Span],
    val fixationCount: Int,
    val visitCount: Int
):
  def entryCount: Int = visitCount

/** A transition between consecutive fixations assigned to different areas. Gaps do
  * not break this chain; a background or off-surface fixation does. Under Multiple,
  * a changed membership set contributes each distinct ordered pair of area identities.
  */
final class FixationAoiTransition private[aoi] (
    val from: AoiId,
    val to: AoiId,
    val count: Int
)

/** Per-area metrics and non-zero transitions in set order, retaining their policy
  * and exact time accounting. Values are derived only from checked assignments.
  */
final class FixationAoiMeasurements private[aoi] (
    val areas: Vector[FixationAoiMetric],
    val transitions: Vector[FixationAoiTransition],
    val report: FixationAoiReport,
    val policy: MembershipPolicy
)

/** Checked AOI membership preserving one result per fixation, including background
  * and off-surface centres. Assignment checks complete frame identity through Agreement.
  */
final class FixationAoiAssignment[U <: Unit2D] private[aoi] (
    val aoiSet: AoiSet[U],
    val scanpath: Scanpath[U],
    val policy: MembershipPolicy,
    private val memberships: Vector[FixationMembership],
    val report: FixationAoiReport
):
  def size: Int                                   = memberships.size
  def get(index: Int): Option[FixationMembership] = memberships.lift(index)
  def toVector: Vector[FixationMembership]        = memberships
  def accountingHolds: Boolean = report.accountedTime == scanpath.extent.duration

  def measure: FixationAoiMeasurements =
    val denominator = report.analysableTime.toMicros
    val metrics     = aoiSet.areas.map { area =>
      var dwell                    = Span.zero
      var fixations                = 0
      var visits                   = 0
      var inside                   = false
      var firstEntry: Option[Span] = None
      var index                    = 0
      while index < size do
        val present = memberships(index) match
          case FixationMembership.Areas(ids) => ids.contains(area.id)
          case _                             => false
        if present then
          val fixation = scanpath.fixations(index)
          dwell = dwell + fixation.duration
          fixations += 1
          if !inside then
            visits += 1
            if firstEntry.isEmpty then
              firstEntry = Some(scanpath.first.span.onset.until(fixation.span.onset))
        inside = present
        index += 1
      new FixationAoiMetric(
        area.id,
        area.label,
        dwell,
        Option.when(denominator > 0L)(dwell.toMicros.toDouble / denominator.toDouble),
        firstEntry,
        fixations,
        visits
      )
    }
    val counts = mutable.Map.empty[(AoiId, AoiId), Int].withDefaultValue(0)
    var index  = 1
    while index < size do
      (memberships(index - 1), memberships(index)) match
        case (FixationMembership.Areas(from), FixationMembership.Areas(to)) if from != to =>
          from.foreach { left =>
            to.foreach { right =>
              if left != right then counts.update((left, right), counts((left, right)) + 1)
            }
          }
        case _ => ()
      index += 1
    val transitions = aoiSet.ids.flatMap { from =>
      aoiSet.ids.collect {
        case to if from != to && counts((from, to)) > 0 =>
          new FixationAoiTransition(from, to, counts((from, to)))
      }
    }
    new FixationAoiMeasurements(metrics, transitions, report, policy)

object FixationAoiAssignment:
  private[aoi] def build[U <: Unit2D](
      aoiSet: AoiSet[U],
      scanpath: Scanpath[U],
      policy: MembershipPolicy,
      select: Pt[U] => Vector[AoiId]
  ): Either[AoiError, FixationAoiAssignment[U]] =
    val memberships               = Vector.newBuilder[FixationMembership]
    var failure: Option[AoiError] = None
    var union                     = Span.zero
    var background                = Span.zero
    var offSurface                = Span.zero
    var duplicated                = Span.zero
    var index                     = 0
    while index < scanpath.n && failure.isEmpty do
      val fixation = scanpath.fixations(index)
      if !aoiSet.frame.contains(fixation.centre) then
        memberships += FixationMembership.OffSurface
        offSurface = offSurface + fixation.duration
      else
        val ids = select(fixation.centre)
        if ids.isEmpty then
          memberships += FixationMembership.Background
          background = background + fixation.duration
        else if policy == MembershipPolicy.RejectOverlap && ids.size > 1 then
          failure = Some(
            AoiError.ObservedFixationOverlap(
              aoiSet.frame.id,
              index,
              fixation.centre.x,
              fixation.centre.y,
              ids
            )
          )
        else
          memberships += FixationMembership.Areas.containing(ids)
          union = union + fixation.duration
          duplicated = duplicated + Span.micros(fixation.duration.toMicros * (ids.size - 1L))
      index += 1
    failure match
      case Some(error) => Left(error)
      case None        =>
        val interFixation = scanpath.transitions.foldLeft(Span.zero)(_ + _.duration)
        Right(
          new FixationAoiAssignment(
            aoiSet,
            scanpath,
            policy,
            memberships.result(),
            new FixationAoiReport(union, background, offSurface, interFixation, duplicated)
          )
        )
