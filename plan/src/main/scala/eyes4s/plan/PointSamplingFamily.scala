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
import eyes4s.core.TrajectoryEndpoint
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.surface.*

/** Editable choices of the static point-sampling recipe. The timeline, layout
  * and scientific inputs are supplied by the form's context.
  */
object PointSamplingViews:
  import RecipeViews as V
  import FieldKind.*

  private[plan] def named[A]: Labelled[A] = new Labelled[A]:
    def token(value: A): String = value.toString
    def label(value: A): String = value.toString
  given Labelled[DensityNormalization] = named
  given Labelled[DensityLookupPolicy]  = named
  given Labelled[TrajectoryEndpoint]   = named
  given Labelled[PointBinEndpoint]     = named

  val queries: FieldView = V.view(
    "queries",
    "Ordered absolute query instants; duplicates and empty queries are retained",
    Repeated(
      V.signedMicros("instant", "Exact microseconds on the declared query clock"),
      0,
      None
    )
  )
  private val boundaries = V.view(
    "boundaries",
    "Strictly increasing bin boundaries",
    Repeated(V.signedMicros("boundary", "Exact microseconds on the query clock"), 2, None)
  )
  val bins: FieldView = V.view(
    "bins",
    "Optional half-open bins and final endpoint convention",
    Optional(
      V.view(
        "definition",
        "Bin boundaries and endpoint policy",
        Group(
          Vector(
            boundaries,
            V.choice(
              "endpoint",
              "Whether the last boundary belongs to the last bin",
              PointBinEndpoint.values.toVector
            )
          ),
          GroupRule.Independent
        )
      ),
      "No binned summaries are requested"
    )
  )
  val normalization: FieldView = V.choice(
    "normalization",
    "Template normalization before lookup",
    DensityNormalization.values.toVector
  )
  val lookup: FieldView = V.choice(
    "lookup",
    "Spatial lookup and out-of-frame convention",
    DensityLookupPolicy.values.toVector
  )
  val trajectoryEndpoint: FieldView = V.choice(
    "trajectoryEndpoint",
    "Temporal support of fixation-onset sampling",
    TrajectoryEndpoint.values.toVector
  )
  val controlSelection: FieldView = V.view(
    "controlSelection",
    "Disable controls, use every eligible distinct template, or a keyed bottom-k sample",
    Variant(
      Vector(
        VariantCase("disabled", "No control sampling", Vector.empty),
        VariantCase("all", "Every eligible distinct control template", Vector.empty),
        VariantCase(
          "bottomK",
          "At most cap distinct control templates per focal trial",
          Vector(
            V.atLeastOne("cap", "Control templates retained per trial", Counted.Occurrences),
            V.number(
              "seed",
              "Exact 64-bit sampling seed",
              Quantity.Dimensionless,
              NumberShape.Int64
            ),
            V.text("sample", "Sample identity")
          )
        )
      )
    )
  )
  private def policy(id: String, meaning: String): FieldView = V.view(
    id,
    meaning,
    Optional(
      V.atLeastOne("minimum", "Minimum successful values", Counted.Occurrences),
      "Require every requested value"
    )
  )
  val controlPolicy: FieldView =
    policy("controlPolicy", "Reduction of selected controls at each query")
  val binPolicy: FieldView = policy("binPolicy", "Reduction of query values within each bin")
  val fields: Vector[FieldView] = Vector(
    queries,
    bins,
    normalization,
    lookup,
    trajectoryEndpoint,
    controlSelection,
    controlPolicy,
    binPolicy
  )

/** Parsed scientific choices. Whole-recipe construction checks bins against
  * the supplied query timeline rather than inventing a clock from form text.
  */
final case class PointSamplingRecipe(
    queries: Vector[Instant],
    bins: Option[PointBins],
    normalization: DensityNormalization,
    lookup: DensityLookupPolicy,
    trajectoryEndpoint: TrajectoryEndpoint,
    controls: PointControlSelection,
    controlPolicy: FailurePolicy,
    binPolicy: FailurePolicy
):
  def spec(clock: ClockId): Either[PointSamplingError[Nothing], PointSamplingSpec] =
    PointSamplingSpec.of(
      clock,
      queries,
      bins,
      normalization,
      lookup,
      trajectoryEndpoint,
      controls,
      controlPolicy,
      binPolicy
    )
  def plan[K, U <: Unit2D: UnitLabel](
      template: PointSamplingPlan[K, U]
  ): Either[PointSamplingError[Nothing], PointSamplingPlan[K, U]] =
    spec(template.spec.clock)
      .map(PointSamplingPlan.of(template.layout, template.source, template.templates, _))

/** Shape-checked, exact-integer form for the eight editable sampling fields. */
final class PointSamplingForm(val clock: ClockId):
  import RawParts.*
  import PointSamplingViews.given
  private type E = RecipeParameterError
  private def err(e: E): String                                                      = e.message
  private def domain[A](value: Either[PointSamplingError[Nothing], A]): Either[E, A] =
    value.left.map(RecipeParameterError.PointSampling.apply)
  private def times(id: String, raw: RawValue): Either[E, Vector[Instant]] = raw match
    case RawValue.Items(values) => values.traverse(longOf(id, _).map(Instant.micros))
    case _                      => Left(missing(id, "items"))
  private def rawTimes(values: Vector[Instant]): RawValue =
    RawValue.Items(values.map(v => number(v.toMicros)))
  val queries: StructuredField[E, Vector[Instant]] = StructuredField.of[E, Vector[Instant]](
    PointSamplingViews.queries
  )(times("queries", _), rawTimes, err)
  val bins: StructuredField[E, Option[PointBins]] =
    StructuredField.of[E, Option[PointBins]](PointSamplingViews.bins)(
      raw =>
        optional(raw).traverse { value =>
          for
            boundaries <- part("bins", value, "boundaries").flatMap(times("bins", _))
            endpoint   <- part("bins", value, "endpoint").flatMap(
              choose("bins", _, PointBinEndpoint.values.toVector)
            )
            bins <- domain(PointBins.of(clock, boundaries, endpoint))
          yield bins
        },
      _.fold[RawValue](RawValue.Absent)(b =>
        group("boundaries" -> rawTimes(b.boundaries), "endpoint" -> choice(b.endpoint))
      ),
      err
    )
  private def enumField[A: Labelled](
      view: FieldView,
      values: Vector[A]
  ): StructuredField[E, A] =
    StructuredField.of[E, A](view)(choose(view.id.value, _, values), choice(_), err)
  val normalization =
    enumField(PointSamplingViews.normalization, DensityNormalization.values.toVector)
  val lookup = enumField(PointSamplingViews.lookup, DensityLookupPolicy.values.toVector)
  val trajectoryEndpoint =
    enumField(PointSamplingViews.trajectoryEndpoint, TrajectoryEndpoint.values.toVector)
  val controlSelection: StructuredField[E, PointControlSelection] =
    StructuredField.of[E, PointControlSelection](PointSamplingViews.controlSelection)(
      raw =>
        token("controlSelection", raw).flatMap {
          case "disabled" => Right(PointControlSelection.Disabled)
          case "all"      => Right(PointControlSelection.Candidates(Selection.All))
          case "bottomK"  =>
            for
              cap    <- int("controlSelection", raw, "cap")
              limit  <- PairLimit.of(cap).left.map(RecipeParameterError.Pairing.apply)
              seed   <- long("controlSelection", raw, "seed")
              sample <- tokenPart("controlSelection", raw, "sample")
            yield PointControlSelection.Candidates(
              Selection.BottomK(limit, Seed(seed), SampleId(sample))
            )
          case other =>
            Left(unknown("controlSelection", other, Vector("disabled", "all", "bottomK")))
        },
      {
        case PointControlSelection.Disabled                  => variant("disabled")
        case PointControlSelection.Candidates(Selection.All) => variant("all")
        case PointControlSelection.Candidates(Selection.BottomK(cap, seed, sample)) =>
          variant(
            "bottomK",
            "cap"    -> number(cap.value),
            "seed"   -> number(seed.value),
            "sample" -> RawValue.Text(sample.value)
          )
      },
      err
    )
  private def policy(view: FieldView): StructuredField[E, FailurePolicy] =
    StructuredField.of[E, FailurePolicy](view)(
      raw =>
        optional(raw).fold[Either[E, FailurePolicy]](Right(FailurePolicy.RequireAll))(n =>
          intOf(view.id.value, n).flatMap(
            FailurePolicy.successfulOnly(_).left.map(RecipeParameterError.Reduction.apply)
          )
        ),
      {
        case FailurePolicy.RequireAll              => RawValue.Absent
        case FailurePolicy.SuccessfulOnly(minimum) => number(minimum.value)
      },
      err
    )
  val controlPolicy                   = policy(PointSamplingViews.controlPolicy)
  val binPolicy                       = policy(PointSamplingViews.binPolicy)
  val fields: Vector[FormField[E, ?]] = Vector(
    queries,
    bins,
    normalization,
    lookup,
    trajectoryEndpoint,
    controlSelection,
    controlPolicy,
    binPolicy
  )
  def views: Vector[FieldView]                                             = fields.map(_.view)
  def validate(field: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_.view.id == field)
      .toRight(FieldError.UnknownField(field))
      .flatMap(_.parse(raw).map(_ => ()))
  def parse(values: FormValues): Either[FormParse.Refusals, PointSamplingRecipe] =
    val q  = queries.parse(values.get(queries.view.id));
    val b  = bins.parse(values.get(bins.view.id))
    val n  = normalization.parse(values.get(normalization.view.id));
    val l  = lookup.parse(values.get(lookup.view.id))
    val e  = trajectoryEndpoint.parse(values.get(trajectoryEndpoint.view.id));
    val c  = controlSelection.parse(values.get(controlSelection.view.id))
    val cp = controlPolicy.parse(values.get(controlPolicy.view.id));
    val bp = binPolicy.parse(values.get(binPolicy.view.id))
    FormParse.errors(q, b, n, l, e, c, cp, bp).toLeft(()).flatMap { _ =>
      (for q1 <- q; b1 <- b; n1 <- n; l1 <- l; e1 <- e; c1 <- c; cp1 <- cp; bp1 <- bp
      yield PointSamplingRecipe(q1, b1, n1, l1, e1, c1, cp1, bp1)).left
        .map(cats.data.NonEmptyVector.one)
    }
  def values[K, U <: Unit2D](plan: PointSamplingPlan[K, U]): FormValues =
    val s = plan.spec
    FormValues.of(
      queries.view.id            -> queries.raw(s.queries),
      bins.view.id               -> bins.raw(s.bins),
      normalization.view.id      -> normalization.raw(s.normalization),
      lookup.view.id             -> lookup.raw(s.lookup),
      trajectoryEndpoint.view.id -> trajectoryEndpoint.raw(s.endpoint),
      controlSelection.view.id   -> controlSelection.raw(s.controls),
      controlPolicy.view.id      -> controlPolicy.raw(s.controlPolicy),
      binPolicy.view.id          -> binPolicy.raw(s.binPolicy)
    )

/** Pure description and inspection of the fixed static-sampling method. */
object PointSamplingDescriptions:
  def description[K, U <: Unit2D](
      plan: PointSamplingPlan[K, U]
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    val s                                                  = plan.spec
    def policy(p: FailurePolicy): Vector[Provenance.Param] = p match
      case FailurePolicy.RequireAll        => Vector(Text("RequireAll"))
      case FailurePolicy.SuccessfulOnly(m) =>
        Vector(Text("SuccessfulOnly"), Num(m.value.toDouble))
    Vector(
      "input"   -> Vector(Text(plan.inputHash.render)),
      "layout"  -> Vector(Text(s"${plan.layout.id.name}@${plan.layout.id.version}")),
      "method"  -> Vector(Text(PointSamplingSpec.method)),
      "clock"   -> Vector(Text(s.clock.name)),
      "queries" -> s.queries.map(t => Text(t.toMicros.toString)),
      "bins"    -> s.bins.fold(Vector[Provenance.Param](Text("none")))(b =>
        Vector(Text(b.clock.name), Text(b.endpoint.toString)) ++ b.boundaries.map(t =>
          Text(t.toMicros.toString)
        )
      ),
      "normalization"      -> Vector(Text(s.normalization.toString)),
      "lookup"             -> Vector(Text(s.lookup.toString)),
      "trajectoryEndpoint" -> Vector(Text(s.endpoint.toString)),
      "controlSelection"   -> (s.controls match
        case PointControlSelection.Disabled              => Vector(Text("disabled"))
        case PointControlSelection.Candidates(selection) =>
          RepetitionViews.selection(selection)),
      "controlPolicy" -> policy(s.controlPolicy),
      "binPolicy"     -> policy(s.binPolicy)
    )
  def inspect[K, U <: Unit2D](
      plan: PointSamplingPlan[K, U]
  ): Either[DescriptorError, RecipeInspection] =
    val views = Vector(
      RecipeViews.reference(
        "input",
        "Combined content identity of ordered source scanpaths and template maps"
      ),
      RecipeViews.reference("layout", "Versioned typed study projections"),
      RecipeViews.reference("method", "Versioned arithmetic-mean static-sampling recipe"),
      RecipeViews.reference("clock", "Query timeline identity")
    ) ++ PointSamplingViews.fields
    val values   = description(plan)
    val metadata = views.map(v => v.id.value -> ParameterInfo.literal(v)).toMap
    values
      .traverse { (id, params) =>
        metadata
          .get(id)
          .toRight(DescriptorError.UnexplainedFields(Vector(id)))
          .map(new InspectedParameter(_, params))
      }
      .map(
        new RecipeInspection(
          _,
          Vector(
            "Source-order fixation-onset sampling; exact query microseconds",
            "Controls are distinct eligible templates sampled along the focal path; keyed bottom-k is deterministic",
            "Half-open bins; explicit final endpoint and two nested failure policies",
            "Preparation and each complete focal trial are whole operations; cancellation occurs between them"
          ),
          ExecutionCapability.SynchronousWholeOperation
        )
      )

/** Matching is shared by preflight and execution, without preparing densities. */
private[plan] object PointSamplingChecks:
  def matches[K, U <: Unit2D](
      plan: PointSamplingPlan[K, U]
  ): Vector[Either[PointSamplingError[K], Int]] =
    val sources    = plan.source.trials.rows
    val templates  = plan.templates
    val layout     = plan.layout
    val duplicates = sources.zipWithIndex.groupBy(_._1.key).view.mapValues(_.map(_._2)).toMap
    sources.map { row =>
      val p          = layout.participant(row.key); val s = layout.stimulus(row.key)
      val candidates = templates.rows.zipWithIndex.collect {
        case (t, i) if layout.participant(t.key) == p && layout.stimulus(t.key) == s => i
      }
      if duplicates(row.key).size > 1 then
        Left(
          PointSamplingError
            .AtTrial(row.key, None, PointSamplingError.DuplicateSource(duplicates(row.key)))
        )
      else
        candidates match
          case Vector(i) => Right(i)
          case Vector()  =>
            Left(
              PointSamplingError.AtTrial(
                row.key,
                None,
                PointSamplingError.MissingTemplate(p, s)
              )
            )
          case indices =>
            Left(
              PointSamplingError.AtTrial(
                row.key,
                None,
                PointSamplingError.AmbiguousTemplate(p, s, indices)
              )
            )
    }

  def findings[K, U <: Unit2D](plan: PointSamplingPlan[K, U]): Vector[AnalysisFinding[K]] =
    plan.source.trials.rows.zip(matches(plan)).flatMap { (row, target) =>
      val refused: Either[PointSamplingError[K], Unit] = for
        index <- target
        template = plan.templates.rows(index)
        _ <- Agreement
          .clocks(row.value.clock, plan.spec.clock)
          .left
          .map(e =>
            PointSamplingError.AtTrial(row.key, Some(template.key), PointSamplingError.Clock(e))
          )
        _ <- Agreement
          .frames(template.value.grid.frame, row.value.frame)
          .left
          .map(e =>
            PointSamplingError
              .AtTrial(row.key, Some(template.key), PointSamplingError.Frames(e))
          )
      yield ()
      refused.left.toOption.map(error =>
        AnalysisFinding
          .DataDependent(Diagnostic.of(error), cats.data.NonEmptyVector.one(row.key))
      )
    }
