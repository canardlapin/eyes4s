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

import cats.syntax.all.*
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

/** A completed temporal study: the plan it ran and one cell per repetition
  * and window, repetitions outer. Only a completed run or
  * [[TemporalStudyResult.reconstruct]] builds one.
  */
final class TemporalStudyResult[K, U <: Unit2D, P, S, D] private[plan] (
    val plan: TemporalStudyPlan[K, U, P, S, D],
    val cells: Vector[TemporalCell[K, U, P, S, D]]
):
  def input: ArtifactRef[TemporalStudyInput[K, U]]            = plan.input
  def description: Vector[(String, Vector[Provenance.Param])] = plan.description

/** One archived cell of a temporal result as a reader holds it before
  * reconstruction: the names of its repetition and window, every trial's
  * occupancy outcome in input order, and the cell's completed study result.
  * It becomes a [[TemporalCell]] only through [[TemporalStudyResult.reconstruct]].
  */
final case class TemporalCellRecord[K, U <: Unit2D, S, D](
    repetition: String,
    window: String,
    occupancy: Vector[(K, Either[TemporalStudyError, WindowOccupancy[U]])],
    result: StudyResult[K, U, S, D]
)

object TemporalStudyResult:
  /** Checked reconstruction of a completed temporal result from its plan and
    * its archived cells, without re-running any estimation or comparison.
    *
    * Re-derived from the plan: the cell layout (repetitions outer, windows
    * inner, each cell named by its repetition and window), each cell's study
    * plan (the base plan with the repetition's phases), each cell's
    * provenance context, the input identity and the description. Checked
    * against them: every cell's study result describes its repetition's plan
    * and passes `StudyResult.reconstruct` in the cell's context; every scale
    * estimates exactly the trials the occupancy ledger lists, in the same
    * order; every density was estimated from its trial's occupancy (its
    * provenance names the digest of the occupancy measure); every temporal
    * failure is the occupancy's own failure, and a failed occupancy leaves
    * only that failure or an earlier frame failure; every cell lists the same
    * trials in the same order; every occupancy uses the plan's boundary and
    * spans its window; each trial is anchored at one instant on one clock
    * across all cells; and a missing epoch names its trial's digest and is
    * missing in every cell. Not re-derived: densities,
    * scores, means and differences, and the fixation positions of each
    * measure, which only the input can confirm.
    */
  def reconstruct[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      cells: Vector[TemporalCellRecord[K, U, S, D]]
  ): Either[TemporalResultError[K], TemporalStudyResult[K, U, P, S, D]] =
    import TemporalResultError.*
    val positions = for
      repetition <- plan.repetitions
      window     <- plan.windows
    yield (repetition, window)
    for
      _ <- Either.cond(
        cells.size == positions.size,
        (),
        CellCount(positions.size, cells.size)
      )
      _ <- positions
        .zip(cells)
        .zipWithIndex
        .collectFirst {
          case (((repetition, window), cell), index)
              if cell.repetition != repetition.name || cell.window != window.name =>
            CellLayout[K](index, repetition.name, window.name, cell.repetition, cell.window)
        }
        .toLeft(())
      studies <- plan.repetitions.traverse(repetition =>
        plan
          .repetitionPlan(repetition)
          .left
          .map(Repetition(repetition.name, _))
          .map(repetition.name -> _)
      )
      byName = studies.toMap
      // Every cell runs over the input's trials in input order.
      _ <- positions
        .zip(cells)
        .collectFirst {
          case ((repetition, window), cell)
              if cell.occupancy.map(_._1) != cells.head.occupancy.map(_._1) =>
            Cell(
              repetition.name,
              window.name,
              OccupancyKeys(cells.head.occupancy.map(_._1), cell.occupancy.map(_._1))
            )
        }
        .toLeft(())
      rebuilt <- positions.zip(cells).traverse { case ((repetition, window), cell) =>
        checkCell(plan, repetition, window, byName(repetition.name), cell).left
          .map(Cell(repetition.name, window.name, _))
      }
      _ <- anchors(positions.zip(cells))
    yield new TemporalStudyResult(plan, rebuilt)

  private def checkCell[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      repetition: RepetitionContrast,
      window: StudyWindow,
      study: StudyPlan[K, U, P, S, D],
      cell: TemporalCellRecord[K, U, S, D]
  ): Either[TemporalResultError[K], TemporalCell[K, U, P, S, D]] =
    import TemporalResultError.*
    val stored   = cell.result
    val trials   = cell.occupancy.map(_._1)
    val outcomes = cell.occupancy.toMap
    val width    = BigInt(window.window.until.toMicros) - window.window.from.toMicros
    def occupancy(key: K, value: WindowOccupancy[U]): Option[TemporalResultError[K]] =
      if value.boundary != plan.boundary then
        Some(Occupancy(key, "boundary", plan.boundary.toString, value.boundary.toString))
      else
        val span = BigInt(value.interval.offset.toMicros) - value.interval.onset.toMicros
        Option.when(span != width)(
          Occupancy(key, "widthMicros", width.toString, span.toString)
        )
    def outcome(
        key: K,
        estimated: Either[StudyFailure[K], Mass[U]]
    ): Option[TemporalResultError[K]] =
      (outcomes.get(key), estimated) match
        case (Some(Right(value)), Right(mass)) =>
          val expected = value.measure.provenance.inputs.render
          val found    = mass.provenance.inputs.render
          Option.when(expected != found)(Density(key, Some(expected), found))
        case (_, Right(mass)) => Some(Density(key, None, mass.provenance.inputs.render))
        case (Some(Left(error)), Left(failure @ StudyFailure.Temporal(_, recorded))) =>
          Option.when(recorded != error)(Failure(key, failure))
        case (Some(Left(_)), Left(StudyFailure.Frame(_, _)))  => None
        case (Some(Left(_)), Left(failure))                   => Some(Failure(key, failure))
        case (_, Left(failure @ StudyFailure.Temporal(_, _))) => Some(Failure(key, failure))
        case (_, Left(_))                                     => None
    def epoch(key: K, digest: String): Option[TemporalResultError[K]] =
      val expected = plan.base.layout.digest.digest(key).render
      Option.when(digest != expected)(Occupancy(key, "epoch", expected, digest))
    for
      _ <- Either.cond(
        stored.description == study.description,
        (),
        Plan(PlanChange.between(study.description, stored.description))
      )
      result <- StudyResult
        .reconstruct(
          stored.input,
          plan.base.layout,
          stored.description,
          stored.scales,
          plan.provenanceContext(repetition, window)
        )
        .left
        .map(Result(_))
      _ <- result.scales
        .collectFirst {
          case scale if scale.estimation.map(_._1) != trials =>
            OccupancyKeys(scale.estimation.map(_._1), trials)
        }
        .toLeft(())
      _ <- cell.occupancy
        .collectFirst(Function.unlift {
          case (key, Right(value))                                  => occupancy(key, value)
          case (key, Left(TemporalStudyError.MissingEpoch(digest))) => epoch(key, digest)
          case _                                                    => None
        })
        .toLeft(())
      _ <- result.scales
        .flatMap(_.estimation)
        .collectFirst(Function.unlift((key, estimated) => outcome(key, estimated)))
        .toLeft(())
    yield new TemporalCell(repetition, window, study, cell.occupancy, result)

  /** Every trial is anchored at one instant on one clock in every cell where
    * its occupancy resolved, and a trial whose epoch is missing in one cell
    * is missing, with the same digest, in every cell: any other outcome
    * (an occupancy, or a failure such as an anchor overflow) implies an epoch.
    */
  private def anchors[K, U <: Unit2D, S, D](
      cells: Vector[((RepetitionContrast, StudyWindow), TemporalCellRecord[K, U, S, D])]
  ): Either[TemporalResultError[K], Unit] =
    import TemporalResultError.*
    def render(
        window: StudyWindow,
        value: Either[TemporalStudyError, WindowOccupancy[U]]
    ): String = value match
      case Right(occupancy) =>
        val micros = BigInt(occupancy.interval.onset.toMicros) - window.window.from.toMicros
        s"${occupancy.interval.clock.name}@$micros"
      case Left(TemporalStudyError.MissingEpoch(digest)) => s"missing:$digest"
      case Left(error)                                   => s"epoch:${error.productPrefix}"
    val observed = cells.flatMap { case ((repetition, window), cell) =>
      cell.occupancy.map((key, value) => (repetition, window, key, render(window, value)))
    }
    val byKey    = observed.groupBy(_._3)
    val missing  = byKey.view.mapValues(_.map(_._4).find(_.startsWith("missing:"))).toMap
    val anchored = byKey.view.mapValues(_.map(_._4).find(_.contains("@"))).toMap
    observed
      .collectFirst(Function.unlift { (repetition, window, key, found) =>
        val expected =
          missing(key).orElse(Option.when(found.contains("@"))(anchored(key)).flatten)
        expected
          .filter(_ != found)
          .map(e => Cell(repetition.name, window.name, Occupancy(key, "anchor", e, found)))
      })
      .toLeft(())

/** Refusals while rebuilding a completed temporal result from its plan and
  * archived cells. A cell's own refusal is wrapped in [[Cell]] with the names
  * of its repetition and window.
  */
enum TemporalResultError[K] derives CanEqual:
  case CellCount(expected: Int, found: Int)
  case CellLayout(
      index: Int,
      repetition: String,
      window: String,
      foundRepetition: String,
      foundWindow: String
  )
  case Repetition(repetition: String, underlying: TemporalStudyError)
  case Plan(changes: Vector[PlanChange])
  case Result(underlying: StudyResultError[K])
  case OccupancyKeys(expected: Vector[K], found: Vector[K])
  case Occupancy(key: K, field: String, expected: String, found: String)
  case Density(key: K, expected: Option[String], found: String)
  case Failure(key: K, failure: StudyFailure[K])
  case Cell(repetition: String, window: String, underlying: TemporalResultError[K])

  def message: String = this match
    case CellCount(expected, found) =>
      s"The plan has $expected repetition and window cells; the result stores $found. " +
        "A completed result stores every cell; partial work is not a result."
    case CellLayout(index, repetition, window, foundRepetition, foundWindow) =>
      s"Cell $index is repetition '$foundRepetition' window '$foundWindow'; the plan places " +
        s"repetition '$repetition' window '$window' there."
    case Repetition(repetition, underlying) =>
      s"Repetition '$repetition' has no study plan: ${underlying.message}"
    case Plan(changes) =>
      "The cell's study result describes another plan than its repetition's, differing in " +
        s"${changes.map(_.field).mkString(", ")}."
    case Result(underlying)             => underlying.message
    case OccupancyKeys(expected, found) =>
      s"The cell estimated trials $expected but its occupancy ledger lists $found."
    case Occupancy(key, field, expected, found) =>
      s"Trial $key has occupancy $field $found where the plan and the other cells give $expected."
    case Density(key, expected, found) =>
      expected match
        case Some(digest) =>
          s"Trial $key has a density estimated from measure $found, not from its occupancy measure $digest."
        case None =>
          s"Trial $key has a density estimated from measure $found although its occupancy failed."
    case Failure(key, failure) =>
      s"Trial $key records a failure its occupancy does not produce: ${failure.message}"
    case Cell(repetition, window, underlying) =>
      s"Repetition '$repetition' window '$window': ${underlying.message}"

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

  /** The study one repetition runs in every window: the base plan with the
    * repetition's focal and reference phases, under duration weighting.
    */
  def repetitionPlan(
      repetition: RepetitionContrast
  ): Either[TemporalStudyError, StudyPlan[K, U, P, S, D]] =
    StudyPlan
      .of(
        base.input,
        base.layout,
        base.grid,
        repetition.focalPhase,
        repetition.referencePhase,
        Weight.Duration,
        base.estimates,
        base.policy,
        base.method,
        base.parameters
      )
      .left
      .map(TemporalStudyError.Input.apply)

  /** The provenance context every evaluation of one cell records beside the
    * study's own parameters: the temporal input, the window and its bounds,
    * the fixation boundary and the repetition.
    */
  def provenanceContext(
      repetition: RepetitionContrast,
      window: StudyWindow
  ): Vector[(String, Provenance.Param)] =
    Vector(
      "temporal.input"       -> Provenance.Param.Text(input.digest),
      "temporal.window"      -> Provenance.Param.Text(window.name),
      "temporal.fromMicros"  -> Provenance.Param.Text(window.window.from.toMicros.toString),
      "temporal.untilMicros" -> Provenance.Param.Text(window.window.until.toMicros.toString),
      "temporal.boundary"    -> Provenance.Param.Text(boundary.toString),
      "temporal.repetition"  -> Provenance.Param.Text(repetition.name)
    )

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
