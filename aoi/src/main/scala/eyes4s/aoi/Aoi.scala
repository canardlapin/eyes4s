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

/** Validated nominal identity of one area of interest. */
opaque type AoiId = String

object AoiId:
  def of(value: String): Either[AoiError, AoiId] =
    if value.trim.isEmpty then Left(AoiError.BlankId(value)) else Right(value.trim)

  extension (id: AoiId) def value: String = id

/** A static, frame-bound area of interest. */
final class Aoi[U <: Unit2D] private (
    val id: AoiId,
    val label: String,
    val frame: Frame[U],
    val region: Region[U],
    val attributes: Map[String, String]
):
  override def equals(other: Any): Boolean = other match
    case aoi: Aoi[?] =>
      id == aoi.id && label == aoi.label && frame == aoi.frame && region == aoi.region &&
      attributes == aoi.attributes
    case _ => false

  override def hashCode: Int =
    (((id.hashCode * 31 + label.hashCode) * 31 + frame.hashCode) * 31 + region.hashCode) * 31 +
      attributes.hashCode

object Aoi:
  def of[U <: Unit2D](
      id: String,
      label: String,
      frame: Frame[U],
      region: Region[U],
      attributes: Map[String, String] = Map.empty
  ): Either[AoiError, Aoi[U]] =
    for
      parsedId <- AoiId.of(id)
      _        <- Either.cond(
        label.trim.nonEmpty,
        (),
        AoiError.BlankLabel(parsedId, label)
      )
      blankAttribute = attributes.keys.toVector.sorted.find(_.trim.isEmpty)
      _ <- blankAttribute.fold[Either[AoiError, Unit]](Right(()))(key =>
        Left(AoiError.BlankAttributeKey(parsedId, key))
      )
    yield new Aoi(parsedId, label.trim, frame, region, attributes)

/** Validated raster resolution used only to compare region areas. */
opaque type AoiResolution = (Int, Int)

object AoiResolution:
  def of(nx: Int, ny: Int): Either[AoiError, AoiResolution] =
    if nx <= 0 || ny <= 0 then Left(AoiError.NonPositiveResolution(nx, ny))
    else Right(nx -> ny)

  extension (resolution: AoiResolution)
    def nx: Int = resolution._1
    def ny: Int = resolution._2

/** How simultaneous membership in overlapping areas is interpreted. */
enum MembershipPolicy derives CanEqual:
  case Multiple
  case ExclusiveByPriority
  case SmallestContaining(resolution: AoiResolution)
  case RejectOverlap

private sealed trait PreparedMembershipPolicy[U <: Unit2D]

private object PreparedMembershipPolicy:
  final case class Multiple[U <: Unit2D](frame: Frame[U])  extends PreparedMembershipPolicy[U]
  final case class Exclusive[U <: Unit2D](frame: Frame[U]) extends PreparedMembershipPolicy[U]
  final case class RejectOverlap[U <: Unit2D](frame: Frame[U])
      extends PreparedMembershipPolicy[U]
  final case class Smallest[U <: Unit2D](grid: Grid[U]) extends PreparedMembershipPolicy[U]

/** Why represented sample time was excluded from AOI membership. */
enum ExclusionReason derives CanEqual:
  case Blink
  case SignalLoss
  case OffSurface

sealed trait SampleMembership derives CanEqual

object SampleMembership:
  final case class Areas private[aoi] (ids: Vector[AoiId]) extends SampleMembership
  case object Background                                   extends SampleMembership
  final case class Excluded(reason: ExclusionReason)       extends SampleMembership

/** A non-empty collection of static AOIs sharing one complete frame identity. */
final class AoiSet[U <: Unit2D] private (
    val frame: Frame[U],
    val areas: Vector[Aoi[U]]
):
  def size: Int          = areas.length
  def ids: Vector[AoiId] = areas.map(_.id)

  def assign(
      recording: Recording[U],
      policy: MembershipPolicy
  ): Either[AoiError, AoiAssignment[U]] =
    assign(recording, policy, recording.representedSupport)

  def assign(
      recording: Recording[U],
      policy: MembershipPolicy,
      temporalSupport: TemporalSupport
  ): Either[AoiError, AoiAssignment[U]] =
    assign(recording, policy, recording.representedSupport(temporalSupport))

  private def assign(
      recording: Recording[U],
      policy: MembershipPolicy,
      support: SampleSupportLedger
  ): Either[AoiError, AoiAssignment[U]] =
    for
      _ <- Agreement.frames(frame, recording.frame).left.map(AoiError.FrameConflict.apply)
      prepared <- prepare(policy)
      result   <- assignPrepared(recording, policy, prepared, support)
    yield result

  private def assignPrepared(
      recording: Recording[U],
      policy: MembershipPolicy,
      prepared: PreparedMembershipPolicy[U],
      support: SampleSupportLedger
  ): Either[AoiError, AoiAssignment[U]] =
    val assignments               = Array.ofDim[SampleMembership](recording.size)
    var failure: Option[AoiError] = None
    var index                     = 0
    while index < recording.size && failure.isEmpty do
      val sample = recording.samples(index)
      sample.gaze match
        case Gaze.Blink() =>
          assignments(index) = SampleMembership.Excluded(ExclusionReason.Blink)
        case Gaze.Lost() =>
          assignments(index) = SampleMembership.Excluded(ExclusionReason.SignalLoss)
        case Gaze.OffScreen(_) =>
          assignments(index) = SampleMembership.Excluded(ExclusionReason.OffSurface)
        case Gaze.Tracked(point, _) if !frame.contains(point) =>
          assignments(index) = SampleMembership.Excluded(ExclusionReason.OffSurface)
        case Gaze.Tracked(point, _) =>
          val containing = areas.filter(_.region.contains(point))
          containing.headOption match
            case None => assignments(index) = SampleMembership.Background
            case Some(first) if containing.length == 1 =>
              assignments(index) = SampleMembership.Areas(Vector(first.id))
            case Some(first) =>
              prepared match
                case PreparedMembershipPolicy.Multiple(_) =>
                  assignments(index) = SampleMembership.Areas(containing.map(_.id))
                case PreparedMembershipPolicy.Exclusive(_) =>
                  assignments(index) = SampleMembership.Areas(Vector(first.id))
                case PreparedMembershipPolicy.Smallest(grid) =>
                  val chosen = containing.tail.foldLeft(first) { (smallest, candidate) =>
                    if candidate.region.area(grid) < smallest.region.area(grid) then candidate
                    else smallest
                  }
                  assignments(index) = SampleMembership.Areas(Vector(chosen.id))
                case PreparedMembershipPolicy.RejectOverlap(_) =>
                  failure = Some(
                    AoiError.ObservedOverlap(
                      frame.id,
                      index,
                      point.x,
                      point.y,
                      containing.map(_.id)
                    )
                  )
      index += 1

    failure match
      case Some(error) => Left(error)
      case None        =>
        val memberships = IArray.from(assignments)
        Right(
          new AoiAssignment(
            this,
            recording,
            policy,
            support,
            memberships,
            AoiAssignment.report(memberships, support)
          )
        )

  private def prepare(
      policy: MembershipPolicy
  ): Either[AoiError, PreparedMembershipPolicy[U]] = policy match
    case MembershipPolicy.Multiple            => Right(PreparedMembershipPolicy.Multiple(frame))
    case MembershipPolicy.ExclusiveByPriority =>
      Right(PreparedMembershipPolicy.Exclusive(frame))
    case MembershipPolicy.RejectOverlap =>
      Right(PreparedMembershipPolicy.RejectOverlap(frame))
    case MembershipPolicy.SmallestContaining(resolution) =>
      Grid
        .over(frame, resolution.nx, resolution.ny)
        .left
        .map(
          AoiError.ResolutionGridFailure(frame.id, resolution.nx, resolution.ny, _)
        )
        .map(PreparedMembershipPolicy.Smallest.apply)

end AoiSet

object AoiSet:
  def of[U <: Unit2D](areas: Vector[Aoi[U]]): Either[AoiError, AoiSet[U]] =
    areas.headOption match
      case None        => Left(AoiError.EmptySet)
      case Some(first) =>
        val duplicate = areas.indices.drop(1).collectFirst {
          case second if areas.take(second).exists(_.id == areas(second).id) =>
            val id    = areas(second).id
            val first = areas.indexWhere(_.id == id)
            AoiError.DuplicateId(id, first, second)
        }
        duplicate.fold {
          Agreement
            .allFrames(areas.map(_.frame))
            .left
            .map(AoiError.FrameConflict.apply)
            .map(_ => new AoiSet(first.frame, areas))
        }(Left(_))

/** Exact time ledger for one assignment. */
final case class AoiAssignmentReport(
    aoiUnionTime: Span,
    backgroundTime: Span,
    excludedTime: Span,
    policyCensoredTime: Span,
    duplicatedAoiTime: Span
) derives CanEqual:
  def analysableTime: Span = aoiUnionTime + backgroundTime
  def accountedTime: Span  = analysableTime + excludedTime + policyCensoredTime

/** Sample-level AOI assignment retaining background and exclusion support. */
final class AoiAssignment[U <: Unit2D] private[aoi] (
    val aoiSet: AoiSet[U],
    val recording: Recording[U],
    val policy: MembershipPolicy,
    val support: SampleSupportLedger,
    private val memberships: IArray[SampleMembership],
    val report: AoiAssignmentReport
):
  def size: Int                                 = memberships.length
  def get(index: Int): Option[SampleMembership] = memberships.lift(index)
  def toVector: Vector[SampleMembership]        = memberships.toVector

  def accountingHolds: Boolean = report.accountedTime == support.representedTime

  def measure: AoiMeasurements =
    val analysableMicros = report.analysableTime.toMicros
    val metrics          = aoiSet.areas.map { area =>
      var dwellMicros              = 0L
      var runs                     = 0
      var inside                   = false
      var firstEntry: Option[Span] = None
      var index                    = 0
      while index < size do
        val present = memberships(index) match
          case SampleMembership.Areas(ids) => ids.contains(area.id)
          case _                           => false
        if present then
          dwellMicros += support.durationAtKnownIndex(index).toMicros
          if !inside then
            runs += 1
            if firstEntry.isEmpty then
              firstEntry = Some(recording.first.t.until(recording.samples(index).t))
        inside = present
        index += 1
      AoiMetric(
        area.id,
        area.label,
        Span.micros(dwellMicros),
        Option.when(analysableMicros > 0L)(dwellMicros.toDouble / analysableMicros),
        firstEntry,
        runs
      )
    }

    // Transitions are between adjacent represented samples. Background or
    // excluded support breaks the chain; Multiple membership contributes the
    // explicit cross-product only when the membership set changes.
    val transitionMap = mutable.Map.empty[(AoiId, AoiId), Int].withDefaultValue(0)
    var index         = 1
    while index < size do
      (memberships(index - 1), memberships(index)) match
        case (SampleMembership.Areas(from), SampleMembership.Areas(to)) if from != to =>
          from.foreach { left =>
            to.foreach { right =>
              if left != right then
                transitionMap.update((left, right), transitionMap((left, right)) + 1)
            }
          }
        case _ => ()
      index += 1
    val transitions = aoiSet.ids.flatMap { from =>
      aoiSet.ids.collect {
        case to if from != to && transitionMap((from, to)) > 0 =>
          AoiTransition(from, to, transitionMap((from, to)))
      }
    }
    AoiMeasurements(metrics, transitions, report, policy)

end AoiAssignment

object AoiAssignment:
  private[aoi] def report(
      memberships: IArray[SampleMembership],
      support: SampleSupportLedger
  ): AoiAssignmentReport =
    var unionMicros      = 0L
    var backgroundMicros = 0L
    var excludedMicros   = 0L
    var duplicatedMicros = 0L
    var index            = 0
    while index < memberships.length do
      val duration = support.durationAtKnownIndex(index).toMicros
      memberships(index) match
        case SampleMembership.Areas(ids) =>
          unionMicros += duration
          duplicatedMicros += duration * (ids.length - 1L)
        case SampleMembership.Background  => backgroundMicros += duration
        case SampleMembership.Excluded(_) => excludedMicros += duration
      index += 1
    AoiAssignmentReport(
      Span.micros(unionMicros),
      Span.micros(backgroundMicros),
      Span.micros(excludedMicros),
      support.censoredTime,
      Span.micros(duplicatedMicros)
    )

final case class AoiMetric(
    id: AoiId,
    label: String,
    dwell: Span,
    dwellProportion: Option[Double],
    firstEntryLatency: Option[Span],
    runCount: Int
) derives CanEqual

final case class AoiTransition(from: AoiId, to: AoiId, count: Int) derives CanEqual

final case class AoiMeasurements(
    areas: Vector[AoiMetric],
    transitions: Vector[AoiTransition],
    report: AoiAssignmentReport,
    policy: MembershipPolicy
) derives CanEqual

enum AoiError derives CanEqual:
  case BlankId(value: String)
  case BlankLabel(id: AoiId, value: String)
  case BlankAttributeKey(id: AoiId, key: String)
  case NonPositiveResolution(nx: Int, ny: Int)
  case EmptySet
  case DuplicateId(id: AoiId, firstIndex: Int, secondIndex: Int)
  case FrameConflict(underlying: GeometryError)
  case ResolutionGridFailure(
      frame: FrameId,
      nx: Int,
      ny: Int,
      underlying: GeometryError
  )
  case ObservedOverlap(
      frame: FrameId,
      sampleIndex: Int,
      x: Double,
      y: Double,
      areas: Vector[AoiId]
  )

  def message: String = this match
    case BlankId(value)        => s"An AOI requires a non-empty id, got value='$value'."
    case BlankLabel(id, value) => s"AOI '$id' requires a non-empty label, got value='$value'."
    case BlankAttributeKey(id, key)    => s"AOI '$id' has a blank attribute key='$key'."
    case NonPositiveResolution(nx, ny) =>
      s"Smallest-containing AOI membership requires positive resolution, got nx=$nx, ny=$ny."
    case EmptySet                       => "An AoiSet requires at least one area."
    case DuplicateId(id, first, second) =>
      s"AoiSet contains duplicate id='$id' at indices $first and $second."
    case FrameConflict(underlying) => s"AoiSet frame conflict: ${underlying.message}"
    case ResolutionGridFailure(frame, nx, ny, underlying) =>
      s"Smallest-containing AOI membership in frame='$frame' could not construct " +
        s"resolution=${nx}x$ny: ${underlying.message}"
    case ObservedOverlap(frame, sample, x, y, areas) =>
      s"Reject-overlap assignment in frame='$frame' found sample[$sample]=($x,$y) in AOIs=${areas.mkString("[", ",", "]")}."

end AoiError
