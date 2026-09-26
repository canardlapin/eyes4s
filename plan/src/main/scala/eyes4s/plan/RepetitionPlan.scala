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
import eyes4s.design.*
import eyes4s.kernel.*

enum RepetitionRule derives CanEqual:
  case SameParticipant, DifferentParticipant, SameStimulus, DifferentStimulus, SameOccasion,
    DifferentOccasion

enum RepetitionPlanError derives CanEqual:
  case ProjectionIds(values: Vector[DefinitionId])
  case DuplicateLayout(id: DefinitionId)
  case MissingLayout(id: DefinitionId)
  case Rules(role: String, values: Vector[RepetitionRule])
  case OverlappingRelations(matched: Vector[RepetitionRule], controls: Vector[RepetitionRule])
  case Grid(row: Int, underlying: SurfaceError)
  case Specification(underlying: EvaluationSpecError)
  def message: String = this match
    case ProjectionIds(v)    => s"Repetition projection identities must be distinct: $v."
    case DuplicateLayout(id) => s"Repetition layout $id is already registered."
    case MissingLayout(id)   => s"Repetition layout $id has no typed registration."
    case Rules(role, v)      =>
      s"Repetition $role rules must be unique, nonempty and consistent: $v."
    case OverlappingRelations(a, b) =>
      s"Repetition matched=$a and controls=$b can overlap; name disjoint relations."
    case Grid(i, e)       => s"Repetition row=$i: ${e.message}"
    case Specification(e) => e.message

/** Finite conjunction vocabulary. Directed self-exclusion is fixed in this schema. */
final class RepetitionRelations private (
    val matched: Vector[RepetitionRule],
    val controls: Vector[RepetitionRule]
)
object RepetitionRelations:
  val withinParticipant: RepetitionRelations = new RepetitionRelations(
    Vector(
      RepetitionRule.SameParticipant,
      RepetitionRule.DifferentOccasion,
      RepetitionRule.SameStimulus
    ),
    Vector(
      RepetitionRule.SameParticipant,
      RepetitionRule.DifferentOccasion,
      RepetitionRule.DifferentStimulus
    )
  )
  val conditionGroups: RepetitionRelations = new RepetitionRelations(
    Vector(RepetitionRule.SameOccasion),
    Vector(RepetitionRule.DifferentOccasion)
  )
  def of(
      matched: Vector[RepetitionRule],
      controls: Vector[RepetitionRule]
  ): Either[RepetitionPlanError, RepetitionRelations] =
    def conflict(rules: Vector[RepetitionRule]) =
      rules.exists(r => rules.exists(s => r.ordinal / 2 == s.ordinal / 2 && r != s))
    Vector("matched" -> matched, "controls" -> controls).find { (_, r) =>
      r.isEmpty || r.distinct.size != r.size || conflict(r)
    } match
      case Some((role, r))                        => Left(RepetitionPlanError.Rules(role, r))
      case None if !conflict(matched ++ controls) =>
        Left(RepetitionPlanError.OverlappingRelations(matched, controls))
      case None => Right(new RepetitionRelations(matched, controls))

/** A typed registration captures projections; wire values carry identities, never closures. */
final class RepetitionLayout[K] private (
    val id: DefinitionId,
    val participantId: DefinitionId,
    val stimulusId: DefinitionId,
    val occasionId: DefinitionId,
    interpret: RepetitionRule => Relation[K, K]
)(using val digest: KeyDigest[K], val ordering: Ordering[K]):
  def design(
      rules: Vector[RepetitionRule],
      selection: Selection
  ): PairDesign.WithinDirected[K] =
    PairDesign.WithinDirected(
      rules.foldLeft(Relation.all[K, K])((r, p) => r.and(interpret(p))),
      SelfPolicy.Exclude,
      selection
    )

object RepetitionLayout:
  def of[K: KeyDigest: Ordering, P, I, O](
      id: DefinitionId,
      participantId: DefinitionId,
      participant: Projection[K, P],
      stimulusId: DefinitionId,
      stimulus: Projection[K, I],
      occasionId: DefinitionId,
      occasion: Projection[K, O]
  ): Either[RepetitionPlanError, RepetitionLayout[K]] =
    val ids = Vector(participantId, stimulusId, occasionId)
    if ids.distinct.size != ids.size then Left(RepetitionPlanError.ProjectionIds(ids))
    else
      Right(
        new RepetitionLayout(
          id,
          participantId,
          stimulusId,
          occasionId,
          {
            case RepetitionRule.SameParticipant      => Relation.sameOn(participant)
            case RepetitionRule.DifferentParticipant => Relation.differentOn(participant)
            case RepetitionRule.SameStimulus         => Relation.sameOn(stimulus)
            case RepetitionRule.DifferentStimulus    => Relation.differentOn(stimulus)
            case RepetitionRule.SameOccasion         => Relation.sameOn(occasion)
            case RepetitionRule.DifferentOccasion    => Relation.differentOn(occasion)
          }
        )
      )

final class RepetitionRegistry[K] private (val layouts: Vector[RepetitionLayout[K]]):
  def register(
      layout: RepetitionLayout[K]
  ): Either[RepetitionPlanError, RepetitionRegistry[K]] =
    if layouts.exists(_.id == layout.id) then
      Left(RepetitionPlanError.DuplicateLayout(layout.id))
    else Right(new RepetitionRegistry(layouts :+ layout))
  def resolve(id: DefinitionId): Either[RepetitionPlanError, RepetitionLayout[K]] =
    layouts.find(_.id == id).toRight(RepetitionPlanError.MissingLayout(id))
object RepetitionRegistry:
  def empty[K]: RepetitionRegistry[K] = new RepetitionRegistry(Vector.empty)

/** Supplied-map all-occasion recipe. This does not infer KDE or serialize arbitrary callbacks. */
final class RepetitionPlan[K, U <: Unit2D] private (
    val layout: RepetitionLayout[K],
    val relations: RepetitionRelations,
    val method: MapSimilarityMethod,
    val controls: Selection,
    val policy: FailurePolicy,
    val grid: Grid[U],
    val trials: Trials[K, Unit, Mass[U]],
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val specification: EvaluationSpec
):
  def run: RepetitionPlanResult[K] =
    given KeyDigest[K] = layout.digest
    given Ordering[K]  = layout.ordering
    val comparison     = method.instance[U]
    val info           = EvaluationInfo.comparison(comparison, specification)
    val matched        = pair(trials, layout.design(relations.matched, Selection.All))
    val control        = pair(trials, layout.design(relations.controls, controls))
    new RepetitionPlanResult(
      evaluatePairs(matched, inputHash, info)(comparison.compare),
      evaluatePairs(control, inputHash, info)(comparison.compare),
      policy,
      planHash
    )

object RepetitionPlan:
  def of[K, U <: Unit2D: UnitLabel](
      layout: RepetitionLayout[K],
      relations: RepetitionRelations,
      method: MapSimilarityMethod,
      controls: Selection,
      policy: FailurePolicy,
      grid: Grid[U],
      trials: Trials[K, Unit, Mass[U]]
  ): Either[RepetitionPlanError, RepetitionPlan[K, U]] =
    trials.rows.zipWithIndex
      .foldLeft[Either[RepetitionPlanError, Unit]](Right(())) { case (acc, (row, i)) =>
        acc.flatMap(_ =>
          Agreement
            .grids(grid, row.value.grid)
            .left
            .map(RepetitionPlanError.Grid(i, _))
            .map(_ => ())
        )
      }
      .flatMap { _ =>
        val geometry = Vector(
          summon[UnitLabel[U]].symbol,
          grid.id.name,
          grid.frame.id.name,
          grid.frame.spec.yAxis.toString
        ).map(ContentHash.ofString) :+
          ContentHash.of(
            IArray(
              grid.frame.spec.xMin,
              grid.frame.spec.xMax,
              grid.frame.spec.yMin,
              grid.frame.spec.yMax,
              grid.nx.toDouble,
              grid.ny.toDouble
            )
          )
        val input = ContentHash.combineAll(geometry ++ trials.rows.map { row =>
          ContentHash.combineAll(
            Vector(
              layout.digest.digest(row.key),
              ContentHash.of(row.value.values),
              row.value.provenance.digest
            )
          )
        })
        val identity =
          Vector(layout.id, layout.participantId, layout.stimulusId, layout.occasionId).map(
            id => s"${id.name}@${id.version}"
          )
        val selected = controls match
          case Selection.All                        => "all"
          case Selection.BottomK(cap, seed, sample) =>
            s"bottomK:${cap.value}:${seed.value}:${sample.value}"
        val meanings = identity ++ Vector(
          method.toString,
          "map-method-version:1",
          relations.matched.mkString(","),
          relations.controls.mkString(","),
          selected,
          policy.toString,
          "directed-exclude-self"
        )
        val hash = ContentHash.combineAll(input +: meanings.map(ContentHash.ofString))
        EvaluationSpec
          .of(
            s"repetition-${method.toString}",
            "1",
            Vector("plan" -> Provenance.Param.Text(hash.render)),
            Vector("value"),
            EvaluationGeometry.onGrid(grid),
            EvaluationTime.OrderFree
          )
          .left
          .map(RepetitionPlanError.Specification.apply)
          .map { spec =>
            new RepetitionPlan(
              layout,
              relations,
              method,
              controls,
              policy,
              grid,
              trials,
              input,
              hash,
              spec
            )
          }
      }

final class RepetitionPlanResult[K] private[plan] (
    val matched: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
    val controls: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
    val policy: FailurePolicy,
    val planHash: ContentHash
)(using Ordering[K]):
  def contrasts: Either[ContrastError[K], Contrast[K, Similarity, SignedDifference]] =
    contrast(matched.meanByLeft(policy), controls.meanByLeft(policy))
