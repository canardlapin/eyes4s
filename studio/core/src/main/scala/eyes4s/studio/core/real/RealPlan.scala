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
    val refused = BackendError.Unavailable(DiagnosticLocus.Revision(revision))
    def ok[E, A](e: Either[E, A]): Either[BackendError, A] = e.leftMap(_ => refused)
    for
      // An inventory dataset admits eyes4s TrialKeys: only their layout fits.
      _ <- Either.cond(
        recipe.layout == DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
        (),
        refused
      )
      method <- ok(
        DefinitionId
          .of(recipe.method.definition.name, recipe.method.definition.version)
          .flatMap(id => ComparisonMethods.resolve(id).toRight(id))
      )
      // Every registered map method takes no parameters.
      _      <- Either.cond(recipe.method.parameters.isEmpty, (), refused)
      phases <- ok(StudyPhases.of(recipe.phases.focal.label, recipe.phases.reference.label))
      grid   <- ok(GridCells.of(recipe.grid.columns, recipe.grid.rows))
      window <- recipe.window.traverse(w =>
        ok(
          Bounds
            .of[Unit2D.Px](w.xMin, w.yMin, w.xMax, w.yMax)
            .flatMap(Subframe.of(screen, WindowFrame, _))
        )
      )
      angular <- recipe.angularScale.traverse(p => ok(LinearAngularScale.of(screen, p.value)))
      scales  <- recipe.scales.values.traverse(s =>
        ok(eyes4s.kernel.Sigma.deg(s.degrees)).map(sigma =>
          StudyScale.Angular[Unit2D.Px](StudyEstimate.Gaussian(sigma, EdgePolicy.Truncate))
        )
      )
      failures <- failure(recipe.failurePolicy).toRight(refused)
      pairing  <- pairingOf(recipe).toRight(refused)
      initial  <- initialFixations(recipe).toRight(refused)
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
      plan <- ok(
        studyRec.plan(
          input.reference,
          CoreKey.layout(TrialKeyDefinitions.trialLayout),
          method.study[Unit2D.Px],
          ()
        )
      )
    yield (plan, method)

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
