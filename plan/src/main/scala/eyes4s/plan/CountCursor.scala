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

import eyes4s.design.*
import eyes4s.kernel.{Provenance, Unit2D}

/** Cancellable exact counting followed by bounded refusal assembly before controls.
  * Pair pages and diagnostic assembly each spend at most the pair quantum.
  * `visited` counts schedule work only; diagnostic steps spend work units without
  * adding candidate visits. Preparation includes canonical sorting and digesting.
  */
final class CountCursor[K] private[plan] (
    private val cursor: PairCursor[K, K],
    private val controls: DirectedPairSchedule[K, K],
    private val phase: CountPhase[K],
    private val groups: CountMatchedGroups[K],
    private val focal: Set[K],
    private val cardinality: CountCardinalityIndex[K],
    private val maps: Int,
    private val scales: Int,
    val visited: Long,
    private val input: ArtifactRef[?],
    private val description: Vector[(String, Vector[Provenance.Param])],
    private val owner: StudyCountIdentity,
    private val keysPerDesign: Long
):
  def stage: StudyDesign = phase match
    case CountPhase.Control(_, _, _) => StudyDesign.Control
    case _                           => StudyDesign.Matched

  private def next(
      cursor: PairCursor[K, K] = cursor,
      phase: CountPhase[K] = phase,
      groups: CountMatchedGroups[K] = groups,
      focal: Set[K] = focal,
      visited: Long = visited
  ): CountCursor[K] = new CountCursor(
    cursor,
    controls,
    phase,
    groups,
    focal,
    cardinality,
    maps,
    scales,
    visited,
    input,
    description,
    owner,
    keysPerDesign
  )

  def advance(
      quanta: WorkQuanta
  ): Either[PlanError, WorkStep[StudyDesign, CountCursor[K], StudyCounts[K]]] =
    phase match
      case CountPhase.Diagnostics(matched, result, diagnostic) =>
        val (units, advanced) = diagnostic.advance(quanta.pairs)
        val following         = advanced match
          case Right(more)   => next(phase = CountPhase.Diagnostics(matched, result, more))
          case Left(refusal) =>
            next(
              cursor = controls.start,
              phase = CountPhase.Control(matched, result, refusal),
              groups = CountMatchedGroups.empty,
              focal = Set.empty
            )
        Right(WorkStep.More(stage, units, following))
      case _ =>
        cursor.advance(quanta.pairs).left.map(PlanError.Schedule.apply).map {
          case PairPage.More(page, units, following) =>
            WorkStep.More(
              stage,
              units,
              next(
                cursor = following,
                groups = appendMatched(page),
                focal = focal ++ page.map(_.left),
                visited = visited + units
              )
            )
          case PairPage.Done(page, units, report) =>
            val allFocal = focal ++ page.map(_.left)
            val counts   = new DesignCounts(
              report.eligiblePairCount,
              allFocal.size.toLong,
              report.unmatchedLeft.size.toLong,
              report.unmatchedRight.size.toLong,
              report.ambiguous.size.toLong
            )
            phase match
              case CountPhase.Control(matched, result, refusal) =>
                WorkStep.Done(
                  units,
                  new StudyCounts(
                    matched,
                    counts,
                    result,
                    matched.focalWithPairs + matched.unmatchedFocal,
                    maps.toLong,
                    scales,
                    input,
                    description,
                    owner,
                    keysPerDesign,
                    refusal
                  )
                )
              case _ =>
                val result = cardinality(appendMatched(page).finish, report)
                WorkStep.More(
                  stage,
                  units,
                  next(
                    phase = CountPhase.Diagnostics(counts, result, cardinality.refusal(result)),
                    visited = visited + units
                  )
                )
        }

  private[plan] def pairingRefusal: Option[PlanError] = phase match
    case CountPhase.Control(_, _, refusal) => refusal
    case _                                 => None

  private def appendMatched(page: Vector[ScheduledPair[K, K]]): CountMatchedGroups[K] =
    phase match
      case CountPhase.Matched() => groups.append(page)
      case _                    => CountMatchedGroups.empty

/** The schedule is focal-major, so only one focal group is unfinished at a time. */
private[plan] final case class CountMatchedGroups[K](
    complete: Vector[(K, Vector[K])],
    current: Option[(K, Vector[K])]
):
  def finish: Vector[(K, Vector[K])] = current match
    case Some((key, values)) if values.size > 1 => complete :+ (key -> values)
    case _                                      => complete

  def append(page: Vector[ScheduledPair[K, K]]): CountMatchedGroups[K] =
    page.foldLeft(this) { (state, pair) =>
      state.current match
        case Some((key, values)) if key == pair.left =>
          state.copy(current = Some(key -> (values :+ pair.right)))
        case _ => CountMatchedGroups(state.finish, Some(pair.left -> Vector(pair.right)))
    }

private[plan] object CountMatchedGroups:
  def empty[K]: CountMatchedGroups[K] = CountMatchedGroups(Vector.empty, None)

private[plan] enum CountPhase[K]:
  case Matched()
  case Diagnostics(
      matched: DesignCounts,
      cardinality: MatchedCardinality[K],
      cursor: CountRefusalCursor[K]
  )
  case Control(
      matched: DesignCounts,
      cardinality: MatchedCardinality[K],
      refusal: Option[PlanError]
  )

object CountCursor:
  /** Begin without pair visits or error-operand sorting. */
  def of[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      work: PreparedStudy[K, U, P, S, D]
  ): Either[PlanError, CountCursor[K]] =
    if plan.input != work.inputReference then
      Left(PlanError.ArtifactMismatch(plan.input.digest, work.inputReference.digest))
    else if plan.description != work.description then
      Left(PlanError.ChangedPreparedPlan(work.methodId, work.layoutId))
    else
      Right(
        new CountCursor(
          work.matched.start,
          work.controls,
          CountPhase.Matched(),
          CountMatchedGroups.empty,
          Set.empty,
          work.countCardinality,
          work.input.trials.rows.size,
          work.estimates.size,
          0L,
          work.inputReference,
          work.description,
          work.countIdentity,
          work.keysPerDesign
        )
      )

  given [K]: Stepwise[CountCursor[K], StudyDesign, PlanError, StudyCounts[K]] with
    def stage(cursor: CountCursor[K]): StudyDesign = cursor.stage
    def advance(cursor: CountCursor[K], quanta: WorkQuanta): Either[
      PlanError,
      WorkStep[StudyDesign, CountCursor[K], StudyCounts[K]]
    ] = cursor.advance(quanta)
