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

/** One equality or inequality on a key projection: participant, stimulus or occasion.
  *
  * Cases come in same/different pairs (consecutive ordinals), and a conjunction may not
  * name both members of a pair; [[RepetitionRelations.of]] relies on that layout. A finite
  * vocabulary, rather than a closure, is what lets a saved plan record its relations as data.
  */
enum RepetitionRule derives CanEqual:
  case SameParticipant, DifferentParticipant, SameStimulus, DifferentStimulus, SameOccasion,
    DifferentOccasion

/** Why a repetition layout, relation set or plan was refused. Each case names its operands:
  * the colliding projection or layout identities, the role (`matched` or `controls`) and its
  * rules, the trial row whose grid disagrees, or the underlying specification error.
  */
enum RepetitionPlanError derives CanEqual:
  case ProjectionIds(values: Vector[DefinitionId])
  case DuplicateLayout(id: DefinitionId)
  case MissingLayout(id: DefinitionId)
  case Rules(role: String, values: Vector[RepetitionRule])
  case OverlappingRelations(matched: Vector[RepetitionRule], controls: Vector[RepetitionRule])
  case Grid(row: Int, underlying: SurfaceError)
  case Specification(underlying: EvaluationSpecError)

  /** A stored `role` analysis (`matched` or `controls`) was not computed by
    * this plan: its `field` (its input hash or its evaluation) differs.
    */
  case ResultMismatch(role: String, field: String)
  def message: String = this match
    case ProjectionIds(v)    => s"Repetition projection identities must be distinct: $v."
    case DuplicateLayout(id) => s"Repetition layout $id is already registered."
    case MissingLayout(id)   => s"Repetition layout $id has no typed registration."
    case Rules(role, v)      =>
      s"Repetition $role rules must be unique, nonempty and consistent: $v."
    case OverlappingRelations(a, b) =>
      s"Repetition matched=$a and controls=$b can overlap; name disjoint relations."
    case Grid(i, e)                  => s"Repetition row=$i: ${e.message}"
    case Specification(e)            => e.message
    case ResultMismatch(role, field) =>
      s"The stored $role analysis was not computed by this repetition plan: its $field differs."

/** Which participants a named repetition relation may pair. Always explicit. */
enum ParticipantScope derives CanEqual:
  /** Both roles pair trials of the same participant only. */
  case WithinParticipant

  /** Both roles pair trials of different participants only. */
  case AcrossParticipants

  /** No participant restriction: pairs cross participant boundaries, as the reference does. */
  case Pooled

  def rule: Option[RepetitionRule] = this match
    case WithinParticipant  => Some(RepetitionRule.SameParticipant)
    case AcrossParticipants => Some(RepetitionRule.DifferentParticipant)
    case Pooled             => None

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

  /** The reference condition-grouping estimand: each trial against every other
    * trial of its own occasion (matched) and every trial of another occasion
    * (controls), with the participant scope stated by the caller.
    *
    * This is NOT a reinstatement contrast. Controls include same-stimulus pairs
    * across occasions -- the very pairs a reinstatement analysis matches -- so
    * with [[ParticipantScope.Pooled]] it reproduces eyesim's
    * `repetitive_similarity(condition_var = ...)` and the inverted contrast of
    * eyesim issue #28. Use [[withinParticipant]] for reinstatement. There is no
    * default scope (recorded decision: participant scope is required for
    * repetition designs).
    */
  def referenceConditionGrouping(scope: ParticipantScope): RepetitionRelations =
    new RepetitionRelations(
      scope.rule.toVector :+ RepetitionRule.SameOccasion,
      scope.rule.toVector :+ RepetitionRule.DifferentOccasion
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

/** The typed layouts a restored plan may resolve, keyed by unique [[DefinitionId]].
  * Registration refuses a duplicate identity (`DuplicateLayout`), and resolution of an
  * unregistered one is `MissingLayout`, so a saved identity never binds to the wrong projections.
  */
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
  /** The plan as data: its input and layout identities, grid, method,
    * relations, control selection, failure policy and pair orientation.
    */
  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    def rules(r: Vector[RepetitionRule]) = r.map(rule => Text(rule.toString))
    Vector(
      "repetition.input" -> Vector(Text(inputHash.render)),
      "layout" -> Vector(layout.id, layout.participantId, layout.stimulusId, layout.occasionId)
        .map(id => Text(s"${id.name}@${id.version}")),
      "grid"     -> Vector(Text(grid.id.name), Num(grid.nx.toDouble), Num(grid.ny.toDouble)),
      "method"   -> Vector(Text(method.token)),
      "matched"  -> rules(relations.matched),
      "controls" -> rules(relations.controls),
      "controlSelection" -> RepetitionViews.selection(controls),
      "failurePolicy"    -> Vector(policy match
        case FailurePolicy.RequireAll        => Text("RequireAll")
        case FailurePolicy.SuccessfulOnly(m) => Num(m.value.toDouble)),
      "pairing" -> Vector(Text("directed-exclude-self"))
    )

  /** The plan's input: its supplied maps, by the content hash they carry. */
  def inputRef: ArtifactRef[Trials[K, Unit, Mass[U]]] = ArtifactRef.of(inputHash)

  /** Typed availability: blocked without the plan's maps or with others; the
    * comparisons and their reduction are left to execution.
    */
  def preflight(
      available: Option[ArtifactRef[Trials[K, Unit, Mass[U]]]]
  ): AnalysisReport[K, Trials[K, Unit, Mass[U]]] =
    AnalysisReport.of(
      RecipeFamily.Repetition,
      description,
      inputRef,
      available,
      Vector.empty,
      Vector(UncheckedAspect.PairComparison, UncheckedAspect.FailurePolicyReduction)
    )

  def diff(other: RepetitionPlan[K, U]): Vector[PlanChange] =
    PlanChange.between(description, other.description)

  /** Every described field with its meaning and view; see [[RecipeInspection]]. */
  def inspect: Either[DescriptorError, RecipeInspection] = RecipeDescriptors.repetition(this)

  def run: RepetitionPlanResult[K] =
    given KeyDigest[K] = layout.digest
    given Ordering[K]  = layout.ordering
    val comparison     = method.similarity[U]
    val info           = EvaluationInfo.comparison(comparison, specification)
    val matched        = pair(trials, layout.design(relations.matched, Selection.All))
    val control        = pair(trials, layout.design(relations.controls, controls))
    new RepetitionPlanResult(
      evaluatePairs(matched, inputHash, info)(comparison.compare),
      evaluatePairs(control, inputHash, info)(comparison.compare),
      policy,
      planHash
    )

  /** The same run as resumable work: both pairings are made here, and the
    * cursor evaluates the matched pairs, then the control pairs, at most a pair
    * quantum of comparisons per step. `Stepwise.complete` at any quanta equals
    * [[run]].
    */
  def work: RepetitionCursor[K] =
    given KeyDigest[K] = layout.digest
    given Ordering[K]  = layout.ordering
    val comparison     = method.similarity[U]
    val info           = EvaluationInfo.comparison(comparison, specification)
    def evaluation(rules: Vector[RepetitionRule], selection: Selection) =
      PairedEvaluation.start(pair(trials, layout.design(rules, selection)), inputHash, info)(
        comparison.compare
      )
    new RepetitionCursor(
      RepetitionPhase.Matched(
        evaluation(relations.matched, Selection.All),
        evaluation(relations.controls, controls)
      ),
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
        // Spelled out rather than taken from `toString`: saved v1 plans carry this
        // hash, so these strings are frozen wire identity, not a debugging render.
        val policyIdentity = policy match
          case FailurePolicy.RequireAll              => "RequireAll"
          case FailurePolicy.SuccessfulOnly(minimum) => s"SuccessfulOnly(${minimum.value})"
        val meanings = identity ++ Vector(
          method.token,
          "map-method-version:1",
          relations.matched.mkString(","),
          relations.controls.mkString(","),
          selected,
          policyIdentity,
          "directed-exclude-self"
        )
        val hash = ContentHash.combineAll(input +: meanings.map(ContentHash.ofString))
        EvaluationSpec
          .of(
            s"repetition-${method.token}",
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

/** The directed matched and control analyses of one [[RepetitionPlan]] run, the failure
  * policy their per-trial means use, and the content hash of the plan that produced them.
  * `contrasts` reduces both by left key under that policy before subtracting.
  */
final class RepetitionPlanResult[K] private[plan] (
    val matched: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
    val controls: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
    val policy: FailurePolicy,
    val planHash: ContentHash
)(using Ordering[K]):
  def contrasts: Either[ContrastError[K], Contrast[K, Similarity, SignedDifference]] =
    contrast(matched.meanByLeft(policy), controls.meanByLeft(policy))

/** The stage a repetition cursor's next step works on. */
enum RepetitionStage derives CanEqual:
  /** Comparing the matched pairs. */
  case Matched

  /** Comparing the control pairs. */
  case Control

/** Where a repetition run stands: the matched pairs in progress with the
  * control pairs waiting, or the matched analysis done and the control pairs
  * in progress.
  */
private[plan] enum RepetitionPhase[K]:
  case Matched(
      matched: PairedEvaluation[K, K, CompareError, Similarity],
      control: PairedEvaluation[K, K, CompareError, Similarity]
  )
  case Control(
      matched: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
      control: PairedEvaluation[K, K, CompareError, Similarity]
  )

/** An immutable position inside a [[RepetitionPlan]] run
  * ([[RepetitionPlan.work]]): each `advance` evaluates at most `quanta.pairs`
  * pairs of the current stage. It never fails.
  */
final class RepetitionCursor[K] private[plan] (
    private val phase: RepetitionPhase[K],
    private val policy: FailurePolicy,
    private val planHash: ContentHash
)(using Ordering[K]):
  def stage: RepetitionStage = phase match
    case RepetitionPhase.Matched(_, _) => RepetitionStage.Matched
    case RepetitionPhase.Control(_, _) => RepetitionStage.Control

  /** The pairs `stage` compares in all: known from the start, since both
    * pairings are made before the first step.
    */
  def totalPairs(stage: RepetitionStage): Long = (stage, phase) match
    case (RepetitionStage.Matched, RepetitionPhase.Matched(m, _)) => m.totalPairs.toLong
    case (RepetitionStage.Matched, RepetitionPhase.Control(m, _)) => m.rows.size.toLong
    case (RepetitionStage.Control, RepetitionPhase.Matched(_, c)) => c.totalPairs.toLong
    case (RepetitionStage.Control, RepetitionPhase.Control(_, c)) => c.totalPairs.toLong

  def advance(
      quanta: WorkQuanta
  ): Either[Nothing, WorkStep[RepetitionStage, RepetitionCursor[K], RepetitionPlanResult[K]]] =
    def next(p: RepetitionPhase[K]) = new RepetitionCursor(p, policy, planHash)
    Right(phase match
      case RepetitionPhase.Matched(matched, control) =>
        matched.advance(quanta.pairs) match
          case PairedPage.More(units, more) =>
            WorkStep.More(
              RepetitionStage.Matched,
              units,
              next(RepetitionPhase.Matched(more, control))
            )
          case PairedPage.Done(units, analysis) =>
            WorkStep.More(
              RepetitionStage.Matched,
              units,
              next(RepetitionPhase.Control(analysis, control))
            )
      case RepetitionPhase.Control(matched, control) =>
        control.advance(quanta.pairs) match
          case PairedPage.More(units, more) =>
            WorkStep.More(
              RepetitionStage.Control,
              units,
              next(RepetitionPhase.Control(matched, more))
            )
          case PairedPage.Done(units, analysis) =>
            WorkStep.Done(units, new RepetitionPlanResult(matched, analysis, policy, planHash)))

object RepetitionCursor:
  given stepwise[K]: Stepwise[
    RepetitionCursor[K],
    RepetitionStage,
    Nothing,
    RepetitionPlanResult[K]
  ] with
    def stage(cursor: RepetitionCursor[K]): RepetitionStage      = cursor.stage
    def advance(cursor: RepetitionCursor[K], quanta: WorkQuanta) = cursor.advance(quanta)

object RepetitionPlanResult:
  /** A stored result of `plan`, checked: each analysis was evaluated on the
    * plan's input with the plan's evaluation (its method under the plan's
    * specification), else `ResultMismatch` naming the role and field.
    */
  def reconstruct[K, U <: Unit2D](
      plan: RepetitionPlan[K, U],
      matched: DirectedPairwiseAnalysis[K, K, CompareError, Similarity],
      controls: DirectedPairwiseAnalysis[K, K, CompareError, Similarity]
  ): Either[RepetitionPlanError, RepetitionPlanResult[K]] =
    val info = EvaluationInfo.comparison(plan.method.similarity[U], plan.specification)
    Vector("matched" -> matched, "controls" -> controls)
      .collectFirst {
        case (role, a) if a.provenance.inputs != plan.inputHash =>
          RepetitionPlanError.ResultMismatch(role, "input")
        case (role, a) if a.evaluation != info =>
          RepetitionPlanError.ResultMismatch(role, "evaluation")
      }
      .toLeft(
        new RepetitionPlanResult(matched, controls, plan.policy, plan.planHash)(using
          plan.layout.ordering
        )
      )
