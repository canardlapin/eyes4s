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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.compare.Similarity
import eyes4s.core.Weight
import eyes4s.design.{FailurePolicy, MinimumSuccessful as CoreMinimum, SignedDifference}
import eyes4s.kernel.*
import eyes4s.plan.{
  ComparisonMethod,
  ComparisonMethods,
  DefinitionId,
  Diagnostic,
  PlanError,
  ControlReferences,
  GridCells,
  InitialFixationPolicy,
  MatchedReferences,
  OccurrenceChoice,
  OffWindowPolicy,
  PreparedStudy,
  StudyEstimate,
  StudyFormContext,
  StudyInput,
  StudyPairing,
  StudyPhases,
  StudyPlan,
  StudyRecipe,
  StudyScale,
  TrialKey as CoreKey,
  TrialKeyDefinitions,
  TrialOccurrence,
  UnmatchedFocalPolicy
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.surface.EdgePolicy

/** The eyes4s study an analysis revision configures (S3.7 slice 2): the
  * revision's recipe built into a `StudyPlan` through eyes4s's own
  * `StudyRecipe.plan`, over the admitted input of its dataset.
  */
object RealPlan:
  type Plan = StudyPlan[CoreKey, Unit2D.Px, Unit, Similarity, SignedDifference]
  type Work = PreparedStudy[CoreKey, Unit2D.Px, Unit, Similarity, SignedDifference]

  /** The identities the backend gives the frames and grid it builds. */
  val WindowFrame: FrameId = FrameId("window")
  val GridName: GridId     = GridId("studio-grid")

  /** The plan `recipe` describes over `input`, admitted on `screen`. A recipe
    * the real backend cannot build yet, or one eyes4s refuses, is
    * `Unavailable` for `revision`.
    */
  def plan(
      revision: AnalysisRevision,
      recipe: Recipe,
      screen: Frame[Unit2D.Px],
      input: StudyInput[CoreKey, Unit2D.Px]
  ): Either[BackendError, (Plan, ComparisonMethod)] =
    // Until the S3.7 protocol minor's typed plan refusal, a refusal is
    // `Unavailable` naming the revision, the recipe field and why: eyes4s's
    // own message (with its diagnostic code for a plan error), or the
    // recipe value the real backend cannot state to eyes4s.
    def refused(field: String, why: String) =
      BackendError.Unavailable(DiagnosticLocus.Artifact(s"${revision.label} $field: $why"))
    for
      // An inventory dataset admits eyes4s TrialKeys: only their layout fits.
      _ <- Either.cond(
        recipe.layout == DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
        (),
        refused(
          "layout",
          s"${recipe.layout} does not fit an inventory dataset's keys " +
            s"(${DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout)})"
        )
      )
      id <- DefinitionId
        .of(recipe.method.definition.name, recipe.method.definition.version)
        .leftMap(e => refused("method", e.message))
      method <- ComparisonMethods
        .resolve(id)
        .toRight(refused("method", s"$id is not a registered comparison method"))
      // Every registered map method takes no parameters.
      _ <- Either.cond(
        recipe.method.parameters.isEmpty,
        (),
        refused(
          "method",
          s"$id takes no parameters; the recipe states ${recipe.method.parameters}"
        )
      )
      phases <- StudyPhases
        .of(recipe.phases.focal.label, recipe.phases.reference.label)
        .leftMap(e => refused("phases", e.message))
      grid <- GridCells
        .of(recipe.grid.columns, recipe.grid.rows)
        .leftMap(e => refused("grid", e.message))
      window <- recipe.window.traverse(w =>
        Bounds
          .of[Unit2D.Px](w.xMin, w.yMin, w.xMax, w.yMax)
          .flatMap(Subframe.of(screen, WindowFrame, _))
          .leftMap(e => refused("window", e.message))
      )
      angular <- recipe.angularScale.traverse(p =>
        LinearAngularScale.of(screen, p.value).leftMap(e => refused("angular scale", e.message))
      )
      scales <- recipe.scales.values.traverse(s =>
        eyes4s.kernel.Sigma
          .deg(s.degrees)
          .leftMap(e => refused("scales", e.message))
          .map(sigma =>
            StudyScale.Angular[Unit2D.Px](StudyEstimate.Gaussian(sigma, EdgePolicy.Truncate))
          )
      )
      failures <- failure(recipe.failurePolicy).toRight(
        refused("failure policy", s"${recipe.failurePolicy} is not an eyes4s failure policy")
      )
      pairing <- pairingOf(recipe).toRight(
        refused("pairing", s"${recipe.matched} names no eyes4s occurrence")
      )
      initial <- initialFixations(recipe).toRight(
        refused(
          "initial fixations",
          s"${recipe.initialFixations.render} needs the cross position, which the recipe does not state"
        )
      )
      studyRec = StudyRecipe(
        StudyFormContext(screen, window.as(WindowFrame), GridName),
        phases,
        weight(recipe.weighting),
        failures,
        grid,
        window,
        recipe.offWindow.map(offWindow),
        angular,
        scales,
        pairing,
        initial
      )
      plan <- studyRec
        .plan(
          input.reference,
          CoreKey.layout(TrialKeyDefinitions.trialLayout),
          method.study[Unit2D.Px],
          ()
        )
        .leftMap(e => refused("plan", RealPlan.reason(Diagnostic.of(e))))
    yield (plan, method)

  /** eyes4s's diagnostic of a refusal: its code and message. */
  def reason(d: Diagnostic[?]): String = s"${d.code.render}: ${d.message}"

  def reason(e: PlanError): String = reason(Diagnostic.of(e))

  private def weight(choice: WeightChoice): Weight = choice match
    case WeightChoice.Uniform  => Weight.Uniform
    case WeightChoice.Duration => Weight.Duration

  private def failure(choice: FailureChoice): Option[FailurePolicy] = choice match
    case FailureChoice.RequireAll        => Some(FailurePolicy.RequireAll)
    case FailureChoice.SuccessfulOnly(m) =>
      CoreMinimum.of(m.value).toOption.map(FailurePolicy.SuccessfulOnly(_))

  private def offWindow(choice: OffWindowChoice): OffWindowPolicy = choice match
    case OffWindowChoice.Exclude   => OffWindowPolicy.Exclude
    case OffWindowChoice.FailTrial => OffWindowPolicy.FailTrial

  private def occurrence(pick: OccurrencePick): Option[OccurrenceChoice] = pick match
    case OccurrencePick.First => Some(OccurrenceChoice.First)
    case OccurrencePick.Last  => Some(OccurrenceChoice.Last)
    case OccurrencePick.At(o) =>
      TrialOccurrence.of(o.value).toOption.map(OccurrenceChoice.At(_))

  private def pairingOf(recipe: Recipe): Option[StudyPairing] =
    val matched = recipe.matched match
      case MatchedChoice.RequireOne     => Some(MatchedReferences.RequireOne)
      case MatchedChoice.SameOccurrence => Some(MatchedReferences.SameOccurrence)
      case MatchedChoice.Select(pick)   => occurrence(pick).map(MatchedReferences.Select(_))
      case MatchedChoice.MeanOfAll      => Some(MatchedReferences.MeanOfAll)
    val controls = recipe.controls match
      case ControlChoice.SameSelection  => ControlReferences.SameSelection
      case ControlChoice.AllOccurrences => ControlReferences.AllOccurrences
    val unmatched = recipe.unmatched match
      case UnmatchedChoice.ReportNoMatch => UnmatchedFocalPolicy.ReportNoMatch
      case UnmatchedChoice.Refuse        => UnmatchedFocalPolicy.Refuse
    matched.map(StudyPairing(_, controls, unmatched))

  /** The recipe's initial-fixation choice. Dropping fixations near the cross
    * needs the cross's position, which the recipe does not state; the real
    * backend refuses it until the recipe or the dataset declares one.
    */
  private def initialFixations(recipe: Recipe): Option[InitialFixationPolicy[Unit2D.Px]] =
    recipe.initialFixations match
      case InitialFixationChoice.KeepAll   => Some(InitialFixationPolicy.keepAll)
      case InitialFixationChoice.DropFirst => Some(InitialFixationPolicy.dropFirst)
      case InitialFixationChoice.DropLeadingNearCross(_) => None
