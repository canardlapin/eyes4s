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

import cats.data.NonEmptyVector
import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.TrajectoryEndpoint
import eyes4s.design.*
import eyes4s.kernel.*

object ScanpathStudyViews:
  private def enumView[A](id: String, meaning: String, values: Vector[A]): FieldView =
    RecipeViews.view(
      id,
      meaning,
      FieldKind.Choice(
        ChoiceSource.Fixed(values.map(value => ChoiceOption(value.toString, value.toString)))
      )
    )
  private def number(
      id: String,
      meaning: String,
      quantity: Quantity,
      shape: NumberShape = NumberShape.Real,
      bounds: NumericBounds = NumericBounds.positive
  ) =
    RecipeViews.number(id, meaning, quantity, shape, bounds)
  def method(unit: PlanarUnit): FieldView =
    val overlap = Vector(
      RecipeViews.view(
        "queries",
        "Ordered exact microsecond queries on the form context clock; repeated times are retained",
        FieldKind.Repeated(RecipeViews.signedMicros("time", "Requested query instant"), 1, None)
      ),
      number(
        "threshold",
        "Strict spatial overlap threshold",
        Quantity.Planar(unit),
        bounds = NumericBounds.nonNegative
      ),
      enumView("distance", "Spatial ground distance", FixationGroundDistance.values.toVector),
      enumView(
        "endpoint",
        "Latest-onset support at trajectory endpoints",
        TrajectoryEndpoint.values.toVector
      ),
      enumView(
        "missing",
        "Denominator and refusal when a query lacks support",
        MissingOverlapPolicy.values.toVector
      )
    )
    val transport = Vector(
      number("xScale", "Spatial x normalization scale", Quantity.Planar(unit)),
      number("yScale", "Spatial y normalization scale", Quantity.Planar(unit)),
      number(
        "timeScaleMicros",
        "Onset-time normalization scale in microseconds",
        Quantity.Duration,
        NumberShape.Int64
      ),
      number(
        "timeWeight",
        "Relative contribution of normalized onset time",
        Quantity.Dimensionless,
        bounds = NumericBounds.nonNegative
      ),
      number("lambda", "Entropic transport regularization", Quantity.Dimensionless),
      number(
        "maximumIterations",
        "Maximum whole solver iterations",
        Quantity.Count(Counted.Occurrences),
        NumberShape.Int32
      ),
      number("marginalTolerance", "Maximum accepted marginal residual", Quantity.Dimensionless),
      number(
        "maximumCosts",
        "Maximum allocated pair costs",
        Quantity.Count(Counted.Cells),
        NumberShape.Int32
      ),
      number(
        "maximumWork",
        "Maximum cost-cell iterations",
        Quantity.Count(Counted.Cells),
        NumberShape.Int64
      )
    )
    RecipeViews.view(
      "method",
      "Whole-pair scanpath comparison and every scientific parameter",
      FieldKind.Variant(
        Vector(
          VariantCase(
            "multiMatch",
            "MultiMatch",
            Vector(
              enumView(
                "cardinality",
                "Native two-fixation or baseline three-fixation admission",
                MultiMatchCardinality.values.toVector
              ),
              number(
                "maximumAlignmentCells",
                "Maximum transition lattice cells before allocation",
                Quantity.Count(Counted.Cells),
                NumberShape.Int64
              )
            )
          ),
          VariantCase("overlap", "Fixation overlap", overlap),
          VariantCase("transport", "Fixation transport", transport)
        )
      )
    )
  val budget: FieldView = RecipeViews.view(
    "budget",
    "ConservativeRawPairsV1 admission checks raw rows/product before either pairing is allocated",
    FieldKind.Group(
      Vector(
        number(
          "sourceRows",
          "Maximum raw focal plus reference rows",
          Quantity.Count(Counted.Occurrences),
          NumberShape.Int32,
          NumericBounds.nonNegative
        ),
        number(
          "candidatePairs",
          "Maximum raw focal/reference product",
          Quantity.Count(Counted.Scores),
          NumberShape.Int64,
          NumericBounds.nonNegative
        ),
        number(
          "selectedPairs",
          "Maximum raw product reserved for either selected pairing",
          Quantity.Count(Counted.Scores),
          NumberShape.Int32,
          NumericBounds.nonNegative
        )
      ),
      GroupRule.Independent
    )
  )

final case class ScanpathStudyRecipe[U <: Unit2D](
    phases: StudyPhases,
    method: ScanpathMethod[U],
    pairing: StudyPairing,
    controls: Selection,
    failurePolicy: FailurePolicy,
    budget: PairScheduleBudget
):
  def plan[K](
      template: ScanpathStudyPlan[K, U]
  )(using UnitLabel[U]): Either[PlanError, ScanpathStudyPlan[K, U]] =
    ScanpathStudyPlan.of(
      template.layout,
      template.input,
      template.frame,
      phases.focal,
      phases.reference,
      method,
      pairing,
      controls,
      failurePolicy,
      budget
    )

/** The query clock is an explicit context value, used only by overlap.
  * Input, layout and expected frame remain checked plan context.
  */
final class ScanpathStudyForm[U <: Unit2D](val queryClock: ClockId)(using unit: UnitLabel[U]):
  import RawParts.*
  private type E = RecipeParameterError
  private def err(error: E): String = error.message
  private def scanpath[A](value: Either[ScanpathComparisonError, A]): Either[E, A] =
    value.left.map(RecipeParameterError.Scanpath.apply)
  private def fixation[A](value: Either[FixationComparisonError, A]): Either[E, A] =
    scanpath(value.left.map(ScanpathComparisonError.Fixations.apply))
  private def enumeration[A](
      field: String,
      raw: RawValue,
      id: String,
      values: Vector[A]
  ): R[A] =
    part(field, raw, id).flatMap(token(field, _)).flatMap { found =>
      values.find(_.toString == found).toRight(unknown(field, found, values.map(_.toString)))
    }
  private val shared                                   = new RepetitionForm
  val controlSelection: StructuredField[E, Selection]  = shared.controlSelection
  val failurePolicy: StructuredField[E, FailurePolicy] = shared.failurePolicy
  val phases: StructuredField[E, StudyPhases]          = StructuredField.of[E, StudyPhases](
    RecipeViews.phases("phases", "Focal and reference phases")
  )(
    raw =>
      for
        focal     <- tokenPart("phases", raw, "focal")
        reference <- tokenPart("phases", raw, "reference")
        phases    <- StudyPhases.of(focal, reference).left.map(RecipeParameterError.Plan.apply)
      yield phases,
    value =>
      group(
        "focal"     -> RawValue.Choice(value.focal),
        "reference" -> RawValue.Choice(value.reference)
      ),
    err
  )
  val pairing: StructuredField[E, StudyPairing] =
    StructuredField.of[E, StudyPairing](RecipeViews.pairing)(
      raw =>
        for
          matched  <- part("pairing", raw, "matched").flatMap(StudyForm.matched)
          controls <- part("pairing", raw, "controls").flatMap(
            choose("pairing", _, ControlReferences.values.toVector)
          )
          unmatched <- part("pairing", raw, "unmatched").flatMap(
            choose("pairing", _, UnmatchedFocalPolicy.values.toVector)
          )
        yield StudyPairing(matched, controls, unmatched),
      value =>
        group(
          "matched"   -> StudyForm.matchedRaw(value.matched),
          "controls"  -> choice(value.controls),
          "unmatched" -> choice(value.unmatched)
        ),
      err
    )
  val budget: StructuredField[E, PairScheduleBudget] =
    StructuredField.of[E, PairScheduleBudget](ScanpathStudyViews.budget)(
      raw =>
        for
          rows       <- int("budget", raw, "sourceRows")
          candidates <- long("budget", raw, "candidatePairs")
          selected   <- int("budget", raw, "selectedPairs")
          result     <- PairScheduleBudget
            .of(rows, candidates, selected)
            .left
            .map(error => RecipeParameterError.Plan(PlanError.Schedule(error)))
        yield result,
      value =>
        group(
          "sourceRows"     -> number(value.maxSourceRows),
          "candidatePairs" -> number(value.maxCandidatePairs),
          "selectedPairs"  -> number(value.maxSelectedPairs)
        ),
      err
    )
  val method: StructuredField[E, ScanpathMethod[U]] =
    StructuredField.of[E, ScanpathMethod[U]](ScanpathStudyViews.method(unit.planar))(
      raw =>
        token("method", raw).flatMap {
          case "multiMatch" =>
            for
              cardinality <- enumeration(
                "method",
                raw,
                "cardinality",
                MultiMatchCardinality.values.toVector
              )
              cells <- long("method", raw, "maximumAlignmentCells").flatMap(n =>
                scanpath(AlignmentCellLimit.of(n))
              )
            yield ScanpathMethod.MultiMatch[U](cardinality, cells)
          case "overlap" =>
            for
              rawQueries <- part("method", raw, "queries")
              queries    <- rawQueries match
                case RawValue.Items(values) =>
                  values.traverse(value => longOf("method", value).map(Instant.micros))
                case _ => Left(missing("method", "queries"))
              threshold <- real("method", raw, "threshold").flatMap(n =>
                fixation(OverlapThreshold.of[U](n))
              )
              distance <- enumeration(
                "method",
                raw,
                "distance",
                FixationGroundDistance.values.toVector
              )
              endpoint <- enumeration(
                "method",
                raw,
                "endpoint",
                TrajectoryEndpoint.values.toVector
              )
              missingPolicy <- enumeration(
                "method",
                raw,
                "missing",
                MissingOverlapPolicy.values.toVector
              )
              spec <- fixation(
                FixationOverlapSpec.of(
                  queryClock,
                  queries,
                  threshold,
                  distance,
                  endpoint,
                  missingPolicy
                )
              )
            yield ScanpathMethod.Overlap(spec)
          case "transport" =>
            for
              x          <- real("method", raw, "xScale")
              y          <- real("method", raw, "yScale")
              time       <- long("method", raw, "timeScaleMicros")
              weight     <- real("method", raw, "timeWeight")
              lambda     <- real("method", raw, "lambda")
              iterations <- int("method", raw, "maximumIterations")
              tolerance  <- real("method", raw, "marginalTolerance")
              costs      <- int("method", raw, "maximumCosts")
              work       <- long("method", raw, "maximumWork")
              config     <- fixation(
                FixationTransportConfig.of[U](
                  x,
                  y,
                  Span.micros(time),
                  weight,
                  lambda,
                  iterations,
                  tolerance,
                  costs,
                  work
                )
              )
            yield ScanpathMethod.Transport(config)
          case other =>
            Left(unknown("method", other, Vector("multiMatch", "overlap", "transport")))
        },
      {
        case ScanpathMethod.MultiMatch(cardinality, cells) =>
          variant(
            "multiMatch",
            "cardinality"           -> RawValue.Choice(cardinality.toString),
            "maximumAlignmentCells" -> number(cells.value)
          )
        case ScanpathMethod.Overlap(spec) =>
          variant(
            "overlap",
            "queries"   -> RawValue.Items(spec.queries.map(at => number(at.toMicros))),
            "threshold" -> number(spec.threshold.value),
            "distance"  -> RawValue.Choice(spec.distance.toString),
            "endpoint"  -> RawValue.Choice(spec.endpoint.toString),
            "missing"   -> RawValue.Choice(spec.missing.toString)
          )
        case ScanpathMethod.Transport(config) =>
          variant(
            "transport",
            "xScale"            -> number(config.xScale),
            "yScale"            -> number(config.yScale),
            "timeScaleMicros"   -> number(config.timeScale.toMicros),
            "timeWeight"        -> number(config.timeWeight),
            "lambda"            -> number(config.lambda),
            "maximumIterations" -> number(config.maximumIterations),
            "marginalTolerance" -> number(config.marginalTolerance),
            "maximumCosts"      -> number(config.maximumCosts),
            "maximumWork"       -> number(config.maximumWork)
          )
      },
      err
    )
  val fields: Vector[FormField[E, ?]] =
    Vector(phases, method, pairing, controlSelection, failurePolicy, budget)
  def views: Vector[FieldView]                                          = fields.map(_.view)
  def validate(id: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_.view.id == id)
      .toRight(FieldError.UnknownField(id))
      .flatMap(_.parse(raw).map(_ => ()))
  def parse(values: FormValues): Either[FormParse.Refusals, ScanpathStudyRecipe[U]] =
    val phases1   = phases.parse(values.get(phases.view.id))
    val method1   = method.parse(values.get(method.view.id))
    val pairing1  = pairing.parse(values.get(pairing.view.id))
    val controls1 = controlSelection.parse(values.get(controlSelection.view.id))
    val failure1  = failurePolicy.parse(values.get(failurePolicy.view.id))
    val budget1   = budget.parse(values.get(budget.view.id))
    FormParse
      .errors(phases1, method1, pairing1, controls1, failure1, budget1)
      .toLeft(())
      .flatMap { _ =>
        (for
          p <- phases1; m <- method1; pairing <- pairing1; c <- controls1; f <- failure1;
          b <- budget1
        yield ScanpathStudyRecipe(p, m, pairing, c, f, b)).left.map(NonEmptyVector.one)
      }
  def values[K](plan: ScanpathStudyPlan[K, U]): FormValues =
    FormValues.of(
      phases.view.id -> group(
        "focal"     -> RawValue.Choice(plan.focalPhase),
        "reference" -> RawValue.Choice(plan.referencePhase)
      ),
      method.view.id           -> method.raw(plan.method),
      pairing.view.id          -> pairing.raw(plan.pairing),
      controlSelection.view.id -> controlSelection.raw(plan.controls),
      failurePolicy.view.id    -> failurePolicy.raw(plan.failurePolicy),
      budget.view.id           -> budget.raw(plan.budget)
    )

object ScanpathStudyDescriptions:
  def inspect[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U]
  ): Either[DescriptorError, RecipeInspection] =
    val meanings = Map(
      "input"  -> "Complete typed scanpath input hash",
      "layout" -> "Registered key layout and version",
      "unit"   -> "Spatial coordinate unit",
      "frame"  -> "Expected nominal frame, bounds and y-axis",
      "preparationAdmission" -> "Conservative raw product admission before either pairing is allocated",
      "focalPhase"     -> "Focal query phase",
      "referencePhase" -> "Reference phase",
      "failurePolicy"  -> "Minimum successful scores required per focal/component",
      "budget" -> "Source rows, raw candidate pairs and raw selected-pair reservation ceilings",
      "controlSelection" -> "All eligible controls or reproducible keyed bottom-k selection",
      "pairing"          -> "Matched-reference, control-pool and unmatched-focal conventions",
      "method"           -> "Registered whole-pair comparison and version"
    )
    val methodMeanings = Map(
      "kind"                  -> "Scientific comparison",
      "cardinality"           -> "Minimum fixation admission",
      "maximumAlignmentCells" -> "Transition-lattice allocation ceiling",
      "alignmentConvention"   -> "Canonical operand and exact-cost traceback convention",
      "clock"                 -> "Nominal clock of the requested overlap queries",
      "queries"               -> "Ordered exact microsecond queries, including repeats",
      "threshold"             -> "Strict spatial threshold",
      "distance"              -> "Spatial ground distance",
      "normConvention"        -> "Named binary64 operation order for the selected distance",
      "endpoint"              -> "Latest-onset endpoint support",
      "missing"               -> "Missing-query denominator policy",
      "xScale"                -> "X normalization scale",
      "yScale"                -> "Y normalization scale",
      "timeScaleMicros"       -> "Onset normalization span",
      "timeWeight"            -> "Relative onset-time cost weight",
      "lambda"                -> "Entropic regularization",
      "maximumIterations"     -> "Whole solver iteration limit",
      "marginalTolerance"     -> "Marginal convergence tolerance",
      "maximumCosts"          -> "Cost-matrix allocation limit",
      "maximumWork"           -> "Cost-cell iteration limit"
    )
    plan.description
      .traverse { (name, values) =>
        val meaning = if name.startsWith("method.") then
          methodMeanings.get(name.stripPrefix("method."))
        else meanings.get(name)
        meaning
          .toRight(DescriptorError.UnexplainedFields(Vector(name)))
          .map(text =>
            new InspectedParameter(
              ParameterInfo.literal(RecipeViews.reference(name, text)),
              values
            )
          )
      }
      .map(fields =>
        new RecipeInspection(
          fields,
          Vector(
            "Preparation reserves the raw focal/reference product before allocating matched/control pairings; filtering does not relax admission",
            "Preparation and final reduction are whole operations; pair quanta cap whole comparisons only",
            "Both trial keys, pairing refusals, method failures, exact query ledgers and transport diagnostics remain available",
            "MultiMatch baseline admission is explicit; overlap is latest-onset and transport is finite entropic transport, not exact EMD",
            "Observed clocks are never synchronized implicitly; supplied paths must satisfy the selected method's identity requirements"
          ),
          ExecutionCapability.SynchronousWholeOperation
        )
      )
