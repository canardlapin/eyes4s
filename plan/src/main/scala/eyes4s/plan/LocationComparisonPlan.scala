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

import eyes4s.compare.*
import eyes4s.compare.Similarity.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** An explicit focal/matched/control triple. No matching rule is inferred. */
final class LocationComparisonQuery private (
    val id: String,
    val focal: LocationTrialId,
    val matched: LocationTrialId,
    val control: LocationTrialId
)

object LocationComparisonQuery:
  def of(
      id: String,
      focal: LocationTrialId,
      matched: LocationTrialId,
      control: LocationTrialId
  ): Either[LocationQueryError, LocationComparisonQuery] =
    if id.trim.isEmpty then Left(LocationQueryError.EmptyQueryId(id))
    else if focal == matched then
      Left(LocationQueryError.SameTrial(id, "focal", focal, "matched", matched))
    else if focal == control then
      Left(LocationQueryError.SameTrial(id, "focal", focal, "control", control))
    else if matched == control then
      Left(LocationQueryError.SameTrial(id, "matched", matched, "control", control))
    else Right(new LocationComparisonQuery(id, focal, matched, control))

enum LocationQueryError derives CanEqual:
  case EmptyQueryId(value: String)
  case SameTrial(
      id: String,
      leftRole: String,
      left: LocationTrialId,
      rightRole: String,
      right: LocationTrialId
  )

  def message: String = this match
    case EmptyQueryId(value) =>
      s"A location comparison query identifier must not be empty, got '$value'."
    case SameTrial(id, leftRole, left, rightRole, right) =>
      s"Location comparison query '$id' uses trial '${left.value}' as both $leftRole and $rightRole ('${right.value}')."

object LocationQueryError:
  given diagnose: Diagnose[LocationQueryError, Nothing] =
    Diagnose.derived(LocationDiagnostics.query)(_.message)

enum LocationPlanError derives CanEqual:
  case Grid(underlying: GeometryError)
  case EmptyInput
  case EmptyQueries
  case DuplicateQuery(id: String)
  case MissingTrial(query: String, role: String, trial: LocationTrialId)

  def message: String = this match
    case Grid(error)        => error.message
    case EmptyInput         => "A location comparison plan needs one or more location trials."
    case EmptyQueries       => "A location comparison plan needs one or more explicit queries."
    case DuplicateQuery(id) => s"Location comparison plan contains query '$id' more than once."
    case MissingTrial(query, role, trial) =>
      s"Location comparison query '$query' names missing $role trial '${trial.value}'."

object LocationPlanError:
  given diagnose: Diagnose[LocationPlanError, Nothing] =
    Diagnose.derived(LocationDiagnostics.plan)(_.message)

/** A trial's binned counts and its corresponding normalised surface. */
final class LocationMap private[plan] (
    val bins: Vector[Double],
    val mass: Mass[Px]
)

enum LocationMapError derives CanEqual:
  case Measure(trial: LocationTrialId, underlying: SurfaceError)
  case Binning(trial: LocationTrialId, underlying: GeometryError)
  case Surface(trial: LocationTrialId, underlying: SurfaceError)

  def message: String = this match
    case Measure(trial, error) =>
      s"Location trial '${trial.value}' could not form a uniform point measure: ${error.message}"
    case Binning(trial, error) =>
      s"Location trial '${trial.value}' could not be binned: ${error.message}"
    case Surface(trial, error) =>
      s"Location trial '${trial.value}' could not form a normalised surface: ${error.message}"

object LocationMapError:
  given diagnose: Diagnose[LocationMapError, Nothing] =
    Diagnose.derived(LocationDiagnostics.map)(_.message)

/** The one retained map outcome for a trial. */
final class LocationTrialMap private[plan] (
    val trial: LocationTrialId,
    val result: Either[LocationMapError, LocationMap]
)

enum LocationRunError derives CanEqual:
  case Map(query: String, role: String, trial: LocationTrialId, underlying: LocationMapError)
  case Comparison(
      query: String,
      role: String,
      focal: LocationTrialId,
      reference: LocationTrialId,
      underlying: CompareError
  )

  def message: String = this match
    case Map(query, role, trial, error) =>
      s"Location comparison query '$query' $role trial '${trial.value}' has no map: ${error.message}"
    case Comparison(query, role, focal, reference, error) =>
      s"Location comparison query '$query' $role comparison '${focal.value}' to '${reference.value}' failed: ${error.message}"

object LocationRunError:
  given diagnose: Diagnose[LocationRunError, Nothing] =
    Diagnose.derived(LocationDiagnostics.run)(_.message)

/** The retained result of one explicit comparison triple. */
final class LocationComparisonOutcome private[plan] (
    val query: LocationComparisonQuery,
    val matched: Either[LocationRunError, Similarity],
    val control: Either[LocationRunError, Similarity],
    val difference: Either[LocationRunError, Double]
)

/** One complete location-only run. Trial maps and query outcomes retain their
  * caller order, with exactly one entry for each input trial and explicit query.
  */
final class LocationComparisonResult private[plan] (
    val grid: Grid[Px],
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val maps: Vector[LocationTrialMap],
    val outcomes: Vector[LocationComparisonOutcome]
)

/** Archive representation of one query's two stored cosine scores. Scores are
  * raw doubles so [[LocationComparisonResult.restore]] can check their bounds.
  */
final case class LocationStoredOutcome(
    query: String,
    matched: Either[LocationRunError, Double],
    control: Either[LocationRunError, Double]
)

/** Refusals while reconstructing an archived location-only result. */
enum LocationRestoreError derives CanEqual:
  case MapTrialSet(expected: Vector[LocationTrialId], found: Vector[LocationTrialId])
  case InputHash(expected: ContentHash, found: ContentHash)
  case PlanHash(expected: ContentHash, found: ContentHash)
  case MapFailureIdentity(entry: LocationTrialId, errorTrial: LocationTrialId)
  case BinLength(trial: LocationTrialId, expected: Int, found: Int)
  case BinValue(trial: LocationTrialId, index: Int, value: Double)
  case BinTotal(trial: LocationTrialId, expected: Int, found: Double)
  case RebuildMap(trial: LocationTrialId, underlying: SurfaceError)
  case OutcomeQuerySet(expected: Vector[String], found: Vector[String])
  case ScoreWithoutMap(query: String, role: String, trial: LocationTrialId)
  case InvalidScore(query: String, role: String, value: Double)
  case NonFiniteDifference(query: String, matched: Double, control: Double)
  case FailureOperands(query: String, role: String, underlying: LocationRunError)

  def message: String = this match
    case MapTrialSet(expected, found) =>
      s"Stored location maps name trials ${found.map(_.value)}, expected exactly ${expected.map(_.value)}."
    case InputHash(expected, found) =>
      s"Stored location input hash ${found.render} does not match expected ${expected.render}."
    case PlanHash(expected, found) =>
      s"Stored location plan hash ${found.render} does not match expected ${expected.render}."
    case MapFailureIdentity(entry, errorTrial) =>
      s"Stored map entry '${entry.value}' carries a failure for trial '${errorTrial.value}'."
    case BinLength(trial, expected, found) =>
      s"Stored map for trial '${trial.value}' has $found bins, expected $expected."
    case BinValue(trial, index, value) =>
      s"Stored map for trial '${trial.value}' bin=$index must be finite, non-negative, and integral, got $value."
    case BinTotal(trial, expected, found) =>
      s"Stored map for trial '${trial.value}' totals $found observations, expected $expected."
    case RebuildMap(trial, error) =>
      s"Stored map for trial '${trial.value}' could not be rebuilt: ${error.message}"
    case OutcomeQuerySet(expected, found) =>
      s"Stored location outcomes name queries $found, expected exactly $expected."
    case ScoreWithoutMap(query, role, trial) =>
      s"Stored location query '$query' has a successful $role score although trial '${trial.value}' has no map."
    case InvalidScore(query, role, value) =>
      s"Stored location query '$query' has invalid $role cosine score $value."
    case NonFiniteDifference(query, matched, control) =>
      s"Stored location query '$query' has non-finite difference from matched=$matched and control=$control."
    case FailureOperands(query, role, error) =>
      s"Stored location query '$query' $role failure has incompatible operands: ${error.message}"

object LocationRestoreError:
  given diagnose: Diagnose[LocationRestoreError, Nothing] =
    Diagnose.derived(LocationDiagnostics.restore)(_.message)

object LocationComparisonResult:
  /** The permitted roundoff beyond cosine's mathematical [0, 1] range. Values
    * inside this tolerance retain their original stored representation; they
    * are never clamped.
    */
  val CosineBoundTolerance: Double = 1e-12

  /** Reconstruct a saved result without rebinning coordinates or rerunning
    * cosine comparison. Stored entries may be shuffled; the result follows the
    * plan's canonical trial and query order.
    */
  def restore(
      plan: LocationComparisonPlan,
      storedInputHash: ContentHash,
      storedPlanHash: ContentHash,
      maps: Vector[(LocationTrialId, Either[LocationMapError, Vector[Double]])],
      outcomes: Vector[LocationStoredOutcome]
  ): Either[LocationRestoreError, LocationComparisonResult] =
    if storedInputHash != plan.inputHash then
      return Left(LocationRestoreError.InputHash(plan.inputHash, storedInputHash))
    if storedPlanHash != plan.planHash then
      return Left(LocationRestoreError.PlanHash(plan.planHash, storedPlanHash))
    val expectedTrials = plan.input.trials.map(_.id)
    val foundTrials    = maps.map(_._1)
    if foundTrials.distinct.size != foundTrials.size || foundTrials.toSet != expectedTrials.toSet
    then return Left(LocationRestoreError.MapTrialSet(expectedTrials, foundTrials))
    val expectedQueries = plan.queries.map(_.id)
    val foundQueries    = outcomes.map(_.query)
    if foundQueries.distinct.size != foundQueries.size || foundQueries.toSet != expectedQueries.toSet
    then return Left(LocationRestoreError.OutcomeQuerySet(expectedQueries, foundQueries))

    val storedMaps     = maps.toMap
    val storedOutcomes = outcomes.iterator.map(value => value.query -> value).toMap
    val rebuilt        = plan.input.trials.map { trial =>
      restoreMap(plan, trial, storedMaps(trial.id)).map(result => trial.id -> result)
    }
    val mapsByTrial = sequence(rebuilt).map(_.toMap)
    for
      byTrial          <- mapsByTrial
      restoredOutcomes <- sequence(
        plan.queries.map(query => restoreOutcome(query, storedOutcomes(query.id), byTrial))
      )
    yield new LocationComparisonResult(
      plan.grid,
      plan.inputHash,
      plan.planHash,
      plan.input.trials.map(trial => new LocationTrialMap(trial.id, byTrial(trial.id))),
      restoredOutcomes
    )

  private def restoreMap(
      plan: LocationComparisonPlan,
      trial: LocationTrial,
      stored: Either[LocationMapError, Vector[Double]]
  ): Either[LocationRestoreError, Either[LocationMapError, LocationMap]] = stored match
    case Left(error) =>
      if mapTrial(error) == trial.id then Right(Left(error))
      else Left(LocationRestoreError.MapFailureIdentity(trial.id, mapTrial(error)))
    case Right(bins) =>
      if bins.size != plan.grid.size then
        Left(LocationRestoreError.BinLength(trial.id, plan.grid.size, bins.size))
      else
        bins.zipWithIndex.find { case (value, _) =>
          !value.isFinite || value < 0.0 || value != math.rint(value)
        } match
          case Some((value, index)) =>
            Left(LocationRestoreError.BinValue(trial.id, index, value))
          case None =>
            val total = bins.sum
            if total != trial.observations.size.toDouble then
              Left(LocationRestoreError.BinTotal(trial.id, trial.observations.size, total))
            else
              PointMeasure
                .uniform(plan.input.frame, IArray.from(trial.observations.map(_.position)))
                .left
                .map(LocationRestoreError.RebuildMap(trial.id, _))
                .flatMap(measure =>
                  Surface
                    .intensity(plan.grid, IArray.from(bins), measure.provenance)
                    .left
                    .map(LocationRestoreError.RebuildMap(trial.id, _))
                    .flatMap(
                      _.normalised.left.map(LocationRestoreError.RebuildMap(trial.id, _))
                    )
                )
                .map(mass => Right(new LocationMap(bins, mass)))

  private def restoreOutcome(
      query: LocationComparisonQuery,
      stored: LocationStoredOutcome,
      maps: Map[LocationTrialId, Either[LocationMapError, LocationMap]]
  ): Either[LocationRestoreError, LocationComparisonOutcome] =
    for
      matched    <- restoreScore(query, "matched", query.matched, stored.matched, maps)
      control    <- restoreScore(query, "control", query.control, stored.control, maps)
      difference <- restoreDifference(query, matched, control)
    yield new LocationComparisonOutcome(query, matched, control, difference)

  private def restoreDifference(
      query: LocationComparisonQuery,
      matched: Either[LocationRunError, Similarity],
      control: Either[LocationRunError, Similarity]
  ): Either[LocationRestoreError, Either[LocationRunError, Double]] = (matched, control) match
    case (Right(left), Right(right)) =>
      val difference = left.value - right.value
      Either.cond(
        difference.isFinite,
        Right(difference),
        LocationRestoreError.NonFiniteDifference(query.id, left.value, right.value)
      )
    case (Left(error), _) => Right(Left(error))
    case (_, Left(error)) => Right(Left(error))

  private def restoreScore(
      query: LocationComparisonQuery,
      role: String,
      reference: LocationTrialId,
      stored: Either[LocationRunError, Double],
      maps: Map[LocationTrialId, Either[LocationMapError, LocationMap]]
  ): Either[LocationRestoreError, Either[LocationRunError, Similarity]] = stored match
    case Right(value) =>
      if maps(query.focal).isLeft then
        Left(LocationRestoreError.ScoreWithoutMap(query.id, "focal", query.focal))
      else if maps(reference).isLeft then
        Left(LocationRestoreError.ScoreWithoutMap(query.id, role, reference))
      else if !value.isFinite || value < -CosineBoundTolerance || value > 1.0 + CosineBoundTolerance
      then Left(LocationRestoreError.InvalidScore(query.id, role, value))
      else
        Similarity
          .of(value)
          .left
          .map(_ => LocationRestoreError.InvalidScore(query.id, role, value))
          .map(Right.apply)
    case Left(error) =>
      if compatibleFailure(query, role, reference, error, maps) then Right(Left(error))
      else Left(LocationRestoreError.FailureOperands(query.id, role, error))

  private def compatibleFailure(
      query: LocationComparisonQuery,
      role: String,
      reference: LocationTrialId,
      error: LocationRunError,
      maps: Map[LocationTrialId, Either[LocationMapError, LocationMap]]
  ): Boolean = error match
    case LocationRunError.Map(errorQuery, errorRole, trial, underlying) =>
      errorQuery == query.id &&
      ((errorRole == "focal" && trial == query.focal) || (errorRole == role && trial == reference)) &&
      maps(trial) == Left(underlying)
    case LocationRunError.Comparison(errorQuery, errorRole, focal, other, _) =>
      errorQuery == query.id && errorRole == role && focal == query.focal && other == reference &&
      maps(query.focal).isRight && maps(reference).isRight

  private def mapTrial(error: LocationMapError): LocationTrialId = error match
    case LocationMapError.Measure(trial, _) => trial
    case LocationMapError.Binning(trial, _) => trial
    case LocationMapError.Surface(trial, _) => trial

  private def sequence[A](
      values: Vector[Either[LocationRestoreError, A]]
  ): Either[LocationRestoreError, Vector[A]] =
    values.foldLeft[Either[LocationRestoreError, Vector[A]]](Right(Vector.empty)) {
      (acc, next) =>
        for prior <- acc; value <- next yield prior :+ value
    }

/** A location-only comparison over one uniform pixel grid. */
final class LocationComparisonPlan private (
    val input: LocationInput,
    val grid: Grid[Px],
    val queries: Vector[LocationComparisonQuery],
    val inputHash: ContentHash,
    val planHash: ContentHash
):
  def execution: LocationExecution = LocationExecution.start(this)

  def run: LocationComparisonResult = LocationExecution.drain(execution).result.get

/** One immutable, resumable evaluation of a [[LocationComparisonPlan]]. Each
  * [[step]] forms at most one trial map or one complete query triple.
  */
final class LocationExecution private (
    private val plan: LocationComparisonPlan,
    private val maps: Vector[LocationTrialMap],
    private val mapsByTrial: Map[LocationTrialId, Either[LocationMapError, LocationMap]],
    private val outcomes: Vector[LocationComparisonOutcome]
):
  def completedMaps: Int    = maps.size
  def totalMaps: Int        = plan.input.trials.size
  def completedQueries: Int = outcomes.size
  def totalQueries: Int     = plan.queries.size
  def complete: Boolean     = completedMaps == totalMaps && completedQueries == totalQueries
  def isComplete: Boolean   = complete
  def result: Option[LocationComparisonResult] =
    Option.when(complete)(
      new LocationComparisonResult(plan.grid, plan.inputHash, plan.planHash, maps, outcomes)
    )

  def step: LocationExecution =
    if completedMaps < totalMaps then
      val trial  = plan.input.trials(completedMaps)
      val mapped = LocationExecution.map(plan, trial)
      new LocationExecution(
        plan,
        maps :+ mapped,
        mapsByTrial.updated(trial.id, mapped.result),
        outcomes
      )
    else if completedQueries < totalQueries then
      val query   = plan.queries(completedQueries)
      val outcome = LocationExecution.outcome(query, mapsByTrial)
      new LocationExecution(plan, maps, mapsByTrial, outcomes :+ outcome)
    else this

object LocationExecution:
  private[plan] def start(plan: LocationComparisonPlan): LocationExecution =
    new LocationExecution(plan, Vector.empty, Map.empty, Vector.empty)

  @annotation.tailrec
  private[plan] def drain(execution: LocationExecution): LocationExecution =
    if execution.complete then execution else drain(execution.step)

  private def map(plan: LocationComparisonPlan, trial: LocationTrial): LocationTrialMap =
    val result = for
      measure <- PointMeasure
        .uniform(plan.input.frame, IArray.from(trial.observations.map(_.position)))
        .left
        .map(LocationMapError.Measure(trial.id, _))
      bins      <- measure.binned(plan.grid).left.map(LocationMapError.Binning(trial.id, _))
      intensity <- Surface
        .intensity(plan.grid, bins, measure.provenance)
        .left
        .map(LocationMapError.Surface(trial.id, _))
      mass <- intensity.normalised.left.map(LocationMapError.Surface(trial.id, _))
    yield new LocationMap(bins.toVector, mass)
    new LocationTrialMap(trial.id, result)

  private def outcome(
      query: LocationComparisonQuery,
      maps: Map[LocationTrialId, Either[LocationMapError, LocationMap]]
  ): LocationComparisonOutcome =
    def score(role: String, reference: LocationTrialId): Either[LocationRunError, Similarity] =
      for
        focal <- maps(query.focal).left
          .map(LocationRunError.Map(query.id, "focal", query.focal, _))
        other <- maps(reference).left.map(LocationRunError.Map(query.id, role, reference, _))
        value <- Distribution
          .cosine[Px]
          .compare(focal.mass, other.mass)
          .left
          .map(LocationRunError.Comparison(query.id, role, query.focal, reference, _))
      yield value
    val matched    = score("matched", query.matched)
    val control    = score("control", query.control)
    val difference = for left <- matched; right <- control yield left.value - right.value
    new LocationComparisonOutcome(query, matched, control, difference)

object LocationComparisonPlan:
  def binned(
      input: LocationInput,
      columns: Int,
      rows: Int,
      queries: Vector[LocationComparisonQuery]
  ): Either[LocationPlanError, LocationComparisonPlan] =
    for
      _    <- Either.cond(input.trials.nonEmpty, (), LocationPlanError.EmptyInput)
      _    <- Either.cond(queries.nonEmpty, (), LocationPlanError.EmptyQueries)
      grid <- Grid.over(input.frame, columns, rows).left.map(LocationPlanError.Grid.apply)
      _    <- checkedQueries(input, queries)
    yield
      val inputHash = hashes.input(input)
      val planHash  = hashes.plan(inputHash, grid, queries)
      new LocationComparisonPlan(input, grid, queries, inputHash, planHash)

  /** Canonical, length-delimited content identities bind the complete input
    * rows in retained order and the exact explicit triples.
    */
  private object hashes:
    private def tagged(tag: String, values: ContentHash*): ContentHash =
      ContentHash.combineAll(ContentHash.ofString(tag) +: values)

    private def sequence(tag: String, values: Vector[ContentHash]): ContentHash =
      tagged(tag, ContentHash.ofString(values.size.toString), ContentHash.combineAll(values))

    private def frame(frame: Frame[Px]): ContentHash =
      tagged(
        "frame-v1",
        ContentHash.ofString(frame.id.name),
        ContentHash.of(
          IArray(frame.bounds.xMin, frame.bounds.yMin, frame.bounds.xMax, frame.bounds.yMax)
        ),
        ContentHash.ofString(frame.yAxis.toString)
      )

    private def observation(observation: LocationObservation): ContentHash =
      tagged(
        "observation-v1",
        ContentHash.ofString(observation.source.value),
        ContentHash.ofString(observation.sourceOrdinal.toString),
        ContentHash.of(IArray(observation.position.x, observation.position.y))
      )

    private def trial(trial: LocationTrial): ContentHash =
      tagged(
        "trial-v1",
        ContentHash.ofString(trial.id.value),
        sequence("observations-v1", trial.observations.map(observation))
      )

    def input(input: LocationInput): ContentHash =
      tagged(
        "location-input-v1",
        frame(input.frame),
        sequence("trials-v1", input.trials.map(trial))
      )

    private def query(query: LocationComparisonQuery): ContentHash =
      tagged(
        "query-v1",
        ContentHash.ofString(query.id),
        ContentHash.ofString(query.focal.value),
        ContentHash.ofString(query.matched.value),
        ContentHash.ofString(query.control.value)
      )

    def plan(
        inputHash: ContentHash,
        grid: Grid[Px],
        queries: Vector[LocationComparisonQuery]
    ): ContentHash =
      tagged(
        "location-comparison-plan-v1",
        inputHash,
        ContentHash.ofString(grid.nx.toString),
        ContentHash.ofString(grid.ny.toString),
        sequence("queries-v1", queries.map(query)),
        ContentHash.ofString("uniform-binned-cosine-v1")
      )

  private def checkedQueries(
      input: LocationInput,
      queries: Vector[LocationComparisonQuery]
  ): Either[LocationPlanError, Unit] =
    val ids        = scala.collection.mutable.HashSet.empty[String]
    var queryIndex = 0
    while queryIndex < queries.size do
      val query = queries(queryIndex)
      if !ids.add(query.id) then return Left(LocationPlanError.DuplicateQuery(query.id))
      val referenced = Vector(
        "focal"   -> query.focal,
        "matched" -> query.matched,
        "control" -> query.control
      )
      var referenceIndex = 0
      while referenceIndex < referenced.size do
        val (role, trial) = referenced(referenceIndex)
        if !input.byId.contains(trial) then
          return Left(LocationPlanError.MissingTrial(query.id, role, trial))
        referenceIndex += 1
      queryIndex += 1
    Right(())
