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
import eyes4s.codec.{FormCodecs, StudyCodecDefinitions}
import eyes4s.core.Weight
import eyes4s.design.FailurePolicy
import eyes4s.kernel.{Frame, Pt, Unit2D}
import eyes4s.plan.*
import eyes4s.plan.{TrialKey as CoreKey}
import eyes4s.studio.core.backend.{TrialKey as _, *}
import eyes4s.studio.core.document.*
import eyes4s.surface.EdgePolicy

/** The backend interprets its library's form. Studio neither derives scientific
  * facts nor accepts assignments supplied by an editor: a rule is rebound to
  * the admitted dataset's pre-admission inventory on every edit.
  */
object RealRecipeForm:
  private val selectionId = ExpectedUnmatchedForm.view.id

  def serve(
      request: RecipeEditRequest,
      screen: Frame[Unit2D.Px],
      input: StudyInput[CoreKey, Unit2D.Px],
      inventory: Option[InventoryLedger]
  ): Either[BackendError, RecipeFormResult] =
    def refused(reason: String) = BackendError.RegistryRefused(
      DiagnosticLocus.Revision(request.revision),
      reason
    )
    for
      configured <- RealPlan.plan(
        request.revision,
        request.recipe.copy(
          expectedUnmatched = None,
          initialFixations = request.recipe.initialFixations match
            case _: InitialFixationChoice.DropLeadingNearCross => InitialFixationChoice.KeepAll
            case other                                         => other
        ),
        screen,
        input,
        inventory
      )
      (base, _) = configured
      inspection <- base.inspect.leftMap(e => refused(e.message))
      form = StudyForm[Unit2D.Px](
        StudyFormContext(screen, Some(RealPlan.WindowFrame), RealPlan.GridName)
      )
      views <- form.views
        .traverse(withoutAssignments)
        .leftMap(e => refused(e.message))
      view <- FormView.of(StudyCodecDefinitions.studyV4, views).leftMap(e => refused(e.message))
      declared <- request.recipe.expectedUnmatched.traverse(rule =>
        ExpectedUnmatched
          .of(rule.column, rule.values, Vector.empty)
          .leftMap(e => refused(e.message))
      )
      legacy <- request.recipe.initialFixations match
        case InitialFixationChoice.DropLeadingNearCross(radius) =>
          InitialFixationPolicy
            .dropLeadingInClosedDisc(Pt[Unit2D.Px](0, 0), radius.degrees)
            .leftMap(e => refused(e.message))
            .map(Some(_))
        case _ => Right(None)
      initialRaw = legacy.fold(form.initialFixations.raw(base.initialFixations))(policy =>
        form.initialFixations.raw(policy) match
          case RawValue.Variant(token, parts) =>
            RawValue.Variant(
              token,
              parts.map {
                case (id, _) if id.value == "crossX" || id.value == "crossY" =>
                  id -> RawValue.Number("")
                case part => part
              }
            )
          case raw => raw
      )
      seeded = form
        .values(base)
        .updated(form.initialFixations.view.id, initialRaw)
        .updated(selectionId, ExpectedUnmatchedForm.field.raw(declared))
      raw <- request.values
        .fold(Right(stripAssignments(seeded)))(FormCodecs.values.decode)
        .leftMap(e => refused(e.message))
      unknown = raw.values.keys.filterNot(id => views.exists(_.id == id)).toVector
      parsed  =
        if unknown.nonEmpty then
          Left(unknown.map(id => RecipeProblem(id.value, s"Unknown field ${id.value}")))
        else
          form
            .parse(addAssignments(raw))
            .leftMap(_.toVector.map(e => RecipeProblem(e.field.value, e.message)))
            .flatMap(fromStudy(request.recipe, _))
      accepted = parsed.flatMap { recipe =>
        RealPlan
          .plan(request.revision, recipe, screen, input, inventory)
          .leftMap(e => Vector(RecipeProblem("recipe", e.message)))
          .map((plan, _) => (recipe, plan))
      }
      encodedView   <- FormCodecs.view.encode(view).leftMap(e => refused(e.message))
      encodedValues <- FormCodecs.values.encode(raw).leftMap(e => refused(e.message))
      result     = accepted.toOption
      plan       = result.fold(base)(_._2)
      difference = request.compareTo.traverse(previous =>
        RealPlan
          .plan(request.revision, previous, screen, input, inventory)
          .map((before, _) => before.structuralDiff(plan).map(_.render))
      )
      facts    = StudyAdvice.facts(plan)
      sentence = StudyText.sentence(plan).tokens.map {
        case Token.Value(field, role, shown) =>
          RecipeToken(shown, Some(field.value), Some(role.toString))
        case token => RecipeToken(token.text, None, None)
      }
      warnings = StudyAdvice.advisories(plan).map {
        case StudyAdvisory.SigmaBelowCells(i, actual, limit) =>
          RecipeScaleWarning(i, "belowCells", actual, limit)
        case StudyAdvisory.SigmaNearUniform(i, actual, limit) =>
          RecipeScaleWarning(i, "nearUniform", actual, limit)
      }
    yield RecipeFormResult(
      encodedView,
      encodedValues,
      result.map(_._1),
      sentence,
      facts.cellDegrees.map(c => RecipeCell(c.x, c.y)),
      warnings,
      accepted.left.toOption.getOrElse(Vector.empty),
      inspection.conventions,
      difference.toOption.flatten.getOrElse(Vector.empty),
      difference.left.toOption.map(_.message)
    )

  /** Assignments are derived from the dataset, not edited alongside the rule. */
  private def withoutAssignments(view: FieldView): Either[DescriptorError, FieldView] =
    if view.id != selectionId then Right(view)
    else
      view.kind match
        case FieldKind.Optional(inner, absent) =>
          inner.kind match
            case FieldKind.Group(parts, rule) =>
              for
                group <- FieldView.of(
                  inner.id,
                  inner.version,
                  inner.meaning,
                  FieldKind.Group(parts.filterNot(_.id.value == "assignments"), rule),
                  inner.default
                )
                outer <- FieldView.of(
                  view.id,
                  view.version,
                  view.meaning,
                  FieldKind.Optional(group, absent),
                  view.default
                )
              yield outer
            case _ => Right(view)
        case _ => Right(view)

  private def stripAssignments(values: FormValues): FormValues = values.get(selectionId) match
    case RawValue.Group(parts) =>
      values.updated(selectionId, RawValue.Group(parts.filterNot(_._1.value == "assignments")))
    case _ => values

  private def addAssignments(values: FormValues): FormValues = values.get(selectionId) match
    case RawValue.Group(parts) =>
      // The id is taken from the served library descriptor, not a parallel table.
      ExpectedUnmatchedForm.view.kind match
        case FieldKind.Optional(inner, _) =>
          inner.kind match
            case FieldKind.Group(fields, _) =>
              fields.find(_.id.value == "assignments").fold(values) { assignment =>
                values.updated(
                  selectionId,
                  RawValue.Group(
                    parts.filterNot(_._1 == assignment.id) :+ (assignment.id -> RawValue
                      .Items(Vector.empty))
                  )
                )
              }
            case _ => values
        case _ => values
    case _ => values

  private def fromStudy(
      base: Recipe,
      s: StudyRecipe[Unit2D.Px]
  ): Either[Vector[RecipeProblem], Recipe] =
    def problem[A](field: String, e: Either[DocumentError, A]) =
      e.leftMap(error => Vector(RecipeProblem(field, error.message)))
    def unsupported[A](field: String, value: Any): Either[Vector[RecipeProblem], A] =
      Left(Vector(RecipeProblem(field, s"This recipe cannot store $value")))
    for
      grid   <- problem("grid", GridSize.of(s.grid.columns, s.grid.rows))
      scales <- s.scales
        .traverse {
          case StudyScale.Angular(StudyEstimate.Gaussian(sigma, EdgePolicy.Truncate)) =>
            problem("scales", eyes4s.studio.core.document.Sigma.of(sigma.value))
          case other => unsupported("scales", other)
        }
        .flatMap(xs => problem("scales", ScaleSet.of(xs)))
      angular <- s.angularScale.traverse(a =>
        problem("angularScale", DeclaredPixelsPerDegree.of(a.unitsPerDegree))
      )
      window <- s.window.traverse(w =>
        problem(
          "window",
          AnalysisWindow.of(w.region.xMin, w.region.yMin, w.region.xMax, w.region.yMax)
        )
      )
      failures <- s.failurePolicy match
        case FailurePolicy.RequireAll        => Right(FailureChoice.RequireAll)
        case FailurePolicy.SuccessfulOnly(n) =>
          problem("failurePolicy", MinimumSuccessful.of(n.value))
            .map(FailureChoice.SuccessfulOnly(_))
      matched <- s.pairing.matched match
        case MatchedReferences.RequireOne     => Right(MatchedChoice.RequireOne)
        case MatchedReferences.SameOccurrence => Right(MatchedChoice.SameOccurrence)
        case MatchedReferences.MeanOfAll      => Right(MatchedChoice.MeanOfAll)
        case MatchedReferences.Select(pick)   =>
          pick match
            case OccurrenceChoice.First => Right(MatchedChoice.Select(OccurrencePick.First))
            case OccurrenceChoice.Last  => Right(MatchedChoice.Select(OccurrencePick.Last))
            case OccurrenceChoice.At(n) =>
              problem("pairing", eyes4s.studio.core.document.Occurrence.of(n.value)).map(n =>
                MatchedChoice.Select(OccurrencePick.At(n))
              )
      initial <- s.initialFixations match
        case InitialFixationPolicy.KeepAll()   => Right(InitialFixationChoice.KeepAll)
        case InitialFixationPolicy.DropFirst() => Right(InitialFixationChoice.DropFirst)
        case InitialFixationPolicy.DropLeadingInClosedDisc(cross, radius) =>
          for
            point <- problem("initialFixations", ScreenCross.of(cross.x, cross.y))
            r     <- problem("initialFixations", CrossRadius.of(radius))
          yield InitialFixationChoice.DropLeadingAtScreenCross(point, r)
      expected <- s.expectedUnmatched.traverse(selection =>
        ExpectedUnmatchedRule
          .of(selection.column, selection.values)
          .leftMap(e => Vector(RecipeProblem("expectedUnmatched", e.message)))
      )
    yield base.copy(
      phases = PhasePair(Phase(s.phases.focal), Phase(s.phases.reference)),
      weighting =
        if s.weight == Weight.Duration then WeightChoice.Duration else WeightChoice.Uniform,
      failurePolicy = failures,
      grid = grid,
      window = window,
      offWindow = s.offWindow.map {
        case OffWindowPolicy.Exclude   => OffWindowChoice.Exclude
        case OffWindowPolicy.FailTrial => OffWindowChoice.FailTrial
      },
      scales = scales,
      angularScale = angular,
      matched = matched,
      controls = s.pairing.controls match
        case ControlReferences.SameSelection  => ControlChoice.SameSelection
        case ControlReferences.AllOccurrences => ControlChoice.AllOccurrences
      ,
      unmatched = s.pairing.unmatched match
        case UnmatchedFocalPolicy.ReportNoMatch => UnmatchedChoice.ReportNoMatch
        case UnmatchedFocalPolicy.Refuse        => UnmatchedChoice.Refuse
      ,
      initialFixations = initial,
      expectedUnmatched = expected
    )
