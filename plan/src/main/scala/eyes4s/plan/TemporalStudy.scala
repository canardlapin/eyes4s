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
import eyes4s.design.*
import eyes4s.kernel.*

/** Measured trial origin and observed support; never inferred from its first fixation. */
final case class TrialEpoch(anchor: Instant, coverage: ObservedCoverage)

final class StudyWindow private (val name: String, val window: Window):
  def resolve(epoch: TrialEpoch): Either[TemporalStudyError, Interval] =
    val start = BigInt(epoch.anchor.toMicros) + window.from.toMicros
    val end   = BigInt(epoch.anchor.toMicros) + window.until.toMicros
    if !start.isValidLong || !end.isValidLong then
      Left(
        TemporalStudyError.AnchorOverflow(
          name,
          epoch.anchor.toMicros,
          window.from.toMicros,
          window.until.toMicros
        )
      )
    else
      Interval
        .of(epoch.coverage.clock, Instant.micros(start.toLong), Instant.micros(end.toLong))
        .left
        .map(TemporalStudyError.Time.apply)

object StudyWindow:
  def of(name: String, window: Window): Either[TemporalStudyError, StudyWindow] =
    val width = BigInt(window.until.toMicros) - window.from.toMicros
    Either.cond(
      name.trim.nonEmpty && width > 0 && width.isValidLong,
      new StudyWindow(name, window),
      TemporalStudyError.InvalidWindow(name, window.from.toMicros, window.until.toMicros)
    )

/** A declared comparison of two occasions, always scoped within participant. */
final class RepetitionContrast private (
    val name: String,
    val focalPhase: String,
    val referencePhase: String
)
object RepetitionContrast:
  def withinParticipant(
      name: String,
      focalPhase: String,
      referencePhase: String
  ): Either[TemporalStudyError, RepetitionContrast] =
    Either.cond(
      name.trim.nonEmpty && focalPhase.trim.nonEmpty && referencePhase.trim.nonEmpty && focalPhase != referencePhase,
      new RepetitionContrast(name, focalPhase, referencePhase),
      TemporalStudyError.InvalidRepetition(name, focalPhase, referencePhase)
    )

final class TemporalStudyInput[K, U <: Unit2D] private (
    val study: StudyInput[K, U],
    val epochs: Map[K, TrialEpoch],
    val hash: ContentHash
):
  def reference: ArtifactRef[TemporalStudyInput[K, U]] = ArtifactRef.of(hash)

object TemporalStudyInput:
  def of[K: KeyDigest: Ordering, U <: Unit2D](
      study: StudyInput[K, U],
      epochs: Vector[(K, TrialEpoch)]
  ): Either[TemporalStudyError, TemporalStudyInput[K, U]] =
    val duplicates = epochs
      .groupBy(_._1)
      .collect { case (k, rows) if rows.size > 1 => KeyDigest[K].digest(k).render }
      .toVector
      .sorted
    val repeatedTrials = study.trials.rows
      .groupBy(_.key)
      .collect { case (k, rows) if rows.size > 1 => KeyDigest[K].digest(k).render }
      .toVector
      .sorted
    val unknown = epochs
      .map(_._1)
      .filterNot(k => study.trials.rows.exists(_.key == k))
      .map(k => KeyDigest[K].digest(k).render)
    if repeatedTrials.nonEmpty then Left(TemporalStudyError.DuplicateTrials(repeatedTrials))
    else if unknown.nonEmpty then Left(TemporalStudyError.UnknownEpochs(unknown))
    else if duplicates.nonEmpty then Left(TemporalStudyError.DuplicateEpochs(duplicates))
    else
      val timing = epochs.sortBy(_._1).map { case (key, epoch) =>
        ContentHash.combineAll(
          Vector(
            KeyDigest[K].digest(key),
            ContentHash.ofString(epoch.coverage.clock.name),
            ContentHash.ofString(epoch.anchor.toMicros.toString)
          ) ++ epoch.coverage.intervals.flatMap(i =>
            Vector(
              ContentHash.ofString(i.onset.toMicros.toString),
              ContentHash.ofString(i.offset.toMicros.toString)
            )
          )
        )
      }
      Right(
        new TemporalStudyInput(
          study,
          epochs.toMap,
          ContentHash.combineAll(study.hash +: timing)
        )
      )

final class TemporalCell[K, U <: Unit2D, P, S, D] private[plan] (
    val repetition: RepetitionContrast,
    val window: StudyWindow,
    val study: StudyPlan[K, U, P, S, D],
    val occupancy: Vector[(K, Either[TemporalStudyError, WindowOccupancy[U]])],
    val result: StudyResult[K, U, S, D]
)
final class TemporalStudyResult[K, U <: Unit2D, P, S, D] private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val cells: Vector[TemporalCell[K, U, P, S, D]]
)

/** Window/repetition composition over the ordinary study interpreter; scales stay separate. */
final class TemporalStudyPlan[K, U <: Unit2D, P, S, D] private (
    val base: StudyPlan[K, U, P, S, D],
    val input: ArtifactRef[TemporalStudyInput[K, U]],
    val windows: Vector[StudyWindow],
    val repetitions: Vector[RepetitionContrast],
    val boundary: FixationBoundary
)(using UnitLabel[U]):
  def inspect: Either[DescriptorError, RecipeInspection] = RecipeDescriptors.temporal(this)

  /** Typed availability report; see [[Preflight.temporal]]. */
  def preflight(
      available: Option[TemporalStudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): TemporalReport[K, U] = Preflight.temporal(this, available, budget)

  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    base.description ++ Vector(
      "temporal.input"    -> Vector(Text(input.digest)),
      "temporal.boundary" -> Vector(Text(boundary.toString)),
      "temporal.scope"    -> Vector(Text("withinParticipant"))
    ) ++
      windows.zipWithIndex.map { case (w, i) =>
        s"window.$i" -> Vector(
          Text(w.name),
          Text(w.window.from.toMicros.toString),
          Text(w.window.until.toMicros.toString)
        )
      } ++
      repetitions.zipWithIndex.map { case (r, i) =>
        s"repetition.$i" -> Vector(Text(r.name), Text(r.focalPhase), Text(r.referencePhase))
      }
  def diff(other: TemporalStudyPlan[K, U, P, S, D]): Vector[PlanChange] =
    PlanChange.between(description, other.description)
  def prerequisites(available: Option[TemporalStudyInput[K, U]]): Vector[TemporalStudyError] =
    available match
      case None => Vector(TemporalStudyError.Input(PlanError.MissingArtifact(input.digest)))
      case Some(a) if a.reference != input =>
        Vector(
          TemporalStudyError.Input(PlanError.ArtifactMismatch(input.digest, a.reference.digest))
        )
      case Some(a) => base.prerequisites(Some(a.study)).map(TemporalStudyError.Input.apply)

  /** Execute every repetition and window: the prepared study driven to completion. */
  def run(
      available: TemporalStudyInput[K, U]
  ): Either[TemporalStudyError, TemporalStudyResult[K, U, P, S, D]] =
    prepare(available).flatMap(_.run)

  /** Bind the input and prepare every repetition's study once, in plan order,
    * without estimating anything; see [[PreparedTemporalStudy]].
    */
  def prepare(
      available: TemporalStudyInput[K, U],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[TemporalStudyError, PreparedTemporalStudy[K, U, P, S, D]] =
    TemporalWork.prepare(this, available, budget)

object TemporalStudyPlan:
  def of[K, U <: Unit2D: UnitLabel, P, S, D](
      base: StudyPlan[K, U, P, S, D],
      input: ArtifactRef[TemporalStudyInput[K, U]],
      windows: Vector[StudyWindow],
      repetitions: Vector[RepetitionContrast],
      boundary: FixationBoundary
  ): Either[TemporalStudyError, TemporalStudyPlan[K, U, P, S, D]] =
    if base.weight != Weight.Duration then Left(TemporalStudyError.Weighting(base.weight))
    else if windows.isEmpty || windows.map(_.name).distinct.size != windows.size then
      Left(TemporalStudyError.WindowNames(windows.map(_.name)))
    else if repetitions.isEmpty || repetitions.map(_.name).distinct.size != repetitions.size
    then Left(TemporalStudyError.RepetitionNames(repetitions.map(_.name)))
    else Right(new TemporalStudyPlan(base, input, windows, repetitions, boundary))

enum TemporalStudyError derives CanEqual:
  case Input(underlying: PlanError)
  case Time(underlying: TimeError)
  case Occupancy(underlying: WindowOccupancyError)
  case InvalidWindow(name: String, fromMicros: Long, untilMicros: Long)
  case AnchorOverflow(window: String, anchor: Long, from: Long, until: Long)
  case InvalidRepetition(name: String, focal: String, reference: String)
  case WindowNames(names: Vector[String])
  case RepetitionNames(names: Vector[String])
  case DuplicateEpochs(keyDigests: Vector[String])
  case DuplicateTrials(keyDigests: Vector[String])
  case UnknownEpochs(keyDigests: Vector[String])
  case MissingEpoch(keyDigest: String)
  case Weighting(weight: Weight)
  def message: String = this match
    case Input(e)               => e.message
    case Time(e)                => e.message
    case Occupancy(e)           => e.message
    case InvalidWindow(n, f, u) =>
      s"Study window '$n' needs a non-empty name and positive representable width, got [$f, $u) microseconds."
    case AnchorOverflow(w, a, f, u) =>
      s"Window '$w' [$f, $u) cannot be anchored at $a microseconds without overflow."
    case InvalidRepetition(n, f, r) =>
      s"Repetition '$n' needs distinct non-empty focal/reference phases, got '$f' and '$r'."
    case WindowNames(ns)     => s"Windows need non-empty unique names, got $ns."
    case RepetitionNames(ns) => s"Repetitions need non-empty unique names, got $ns."
    case DuplicateTrials(ks) =>
      s"Temporal trial keys must be unique; duplicate key digests $ks."
    case UnknownEpochs(ks) => s"Epoch metadata has no corresponding trial for key digests $ks."
    case DuplicateEpochs(ks) => s"Duplicate epoch metadata for key digests $ks."
    case MissingEpoch(k) => s"No measured anchor and observed coverage for trial key digest $k."
    case Weighting(w)    => s"Temporal duration maps require Duration weighting, got $w."
