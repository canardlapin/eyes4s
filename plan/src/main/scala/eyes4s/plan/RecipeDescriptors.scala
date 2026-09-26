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
import eyes4s.detect.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.surface.EdgePolicy

enum RecipeParameterError derives CanEqual:
  case Geometry(error: GeometryError)
  case Time(error: TimeError)
  case Configuration(error: ConfigurationError)
  case Temporal(error: TemporalStudyError)
  case Reduction(error: ReductionPolicyError)
  case Synchronization(error: SyncEvidenceError)
  case Recording(error: RecordingPlanError)
  def message: String = this match
    case Geometry(e)        => e.message
    case Time(e)            => e.message
    case Configuration(e)   => e.message
    case Temporal(e)        => e.message
    case Reduction(e)       => e.message
    case Synchronization(e) => e.message
    case Recording(e)       => e.message

/** Concrete typed construction routes. No universal scientific defaults are imposed.
  *
  * A single-number parameter also carries a [[NumericField]] (`form`), which
  * parses a raw form value on its own: shape, then the declared bounds, then
  * the same domain constructor as `construct`.
  */
object RecipeParameters:
  import RecipeViews as V

  private def descriptor[R, A](view: FieldView)(
      construct: R => Either[RecipeParameterError, A]
  ): ParameterDescriptor[R, A, RecipeParameterError] =
    new ParameterDescriptor(ParameterInfo.literal(view), construct, _.message)

  /** A one-number parameter described by its form field; `construct` is the
    * domain constructor the field's third stage also runs.
    */
  private def numeric[N, R, A](field: NumericField[RecipeParameterError, N, A])(
      construct: R => Either[RecipeParameterError, A]
  ): ParameterDescriptor[R, A, RecipeParameterError] =
    new ParameterDescriptor(
      ParameterInfo.literal(field.view),
      construct,
      _.message,
      None,
      Some(field)
    )

  private def sigmaOf[U <: Unit2D](x: Double) =
    Sigma.of[U](x).left.map(RecipeParameterError.Geometry.apply)
  private def spanOf(n: Long): Either[RecipeParameterError, Span] = Right(Span.micros(n))
  private def residualOf(x: Span)                                 =
    SyncResidualLimit.of(x).left.map(RecipeParameterError.Synchronization.apply)
  private def ivtOf(x: Velocity[Deg]) =
    IvtThreshold.of(x).left.map(RecipeParameterError.Configuration.apply)
  private def minimumOf(x: Span) =
    MinimumEventDuration.of(x).left.map(RecipeParameterError.Configuration.apply)
  private def extentOf(x: Double) =
    Extent.square[Deg](x).map(_.width).left.map(RecipeParameterError.Geometry.apply)
  private def etaOf(x: Double) =
    EkThresholds.of(x, x).map(_.etaX).left.map(RecipeParameterError.Configuration.apply)
  private def samplesOf(x: Int) =
    EkMinimumSamples.of(x).left.map(RecipeParameterError.Configuration.apply)
  private def gapOf(x: Span) =
    InterpolationGap.of(x).left.map(RecipeParameterError.Configuration.apply)

  /** The typed form fields of the one-number parameters: each parses a raw
    * value on its own through its shape, its declared bounds and the same
    * domain constructor as the parameter's `construct`.
    */
  object forms:
    private def field[N: Numeral, A](
        id: String,
        meaning: String,
        quantity: Quantity,
        bounds: NumericBounds
    )(
        domain: N => Either[RecipeParameterError, A],
        number: A => N
    ): NumericField[RecipeParameterError, N, A] =
      NumericField.literal(id, meaning, quantity, bounds)(domain, number, _.message)

    private def planarSigma[U <: Unit2D](id: String, meaning: String)(using
        u: UnitLabel[U]
    ): NumericField[RecipeParameterError, Double, Sigma[U]] =
      field[Double, Sigma[U]](id, meaning, Quantity.Planar(u.planar), NumericBounds.positive)(
        sigmaOf[U],
        _.value
      )

    def sigma[U <: Unit2D: UnitLabel]: NumericField[RecipeParameterError, Double, Sigma[U]] =
      planarSigma[U]("sigma", "Gaussian standard deviation in the input frame's units")
    def sigmaX[U <: Unit2D: UnitLabel]: NumericField[RecipeParameterError, Double, Sigma[U]] =
      planarSigma[U]("sigmaX", "Gaussian standard deviation along the frame x axis")
    def sigmaY[U <: Unit2D: UnitLabel]: NumericField[RecipeParameterError, Double, Sigma[U]] =
      planarSigma[U]("sigmaY", "Gaussian standard deviation along the frame y axis")

    val residualLimit: NumericField[RecipeParameterError, Long, SyncResidualLimit] =
      field[Long, SyncResidualLimit](
        "residualLimitMicros",
        "Nonnegative synchronization residual rejection threshold",
        Quantity.Duration,
        NumericBounds.nonNegative
      )(n => spanOf(n).flatMap(residualOf), _.span.toMicros)

    val ivtThreshold: NumericField[RecipeParameterError, Double, IvtThreshold] =
      field[Double, IvtThreshold](
        "thresholdDegPerSecond",
        "Strictly positive physical I-VT velocity threshold",
        Quantity.Rate(PlanarUnit.Deg),
        NumericBounds.positive
      )(
        x =>
          Velocity.degPerSecond(x).left.map(RecipeParameterError.Geometry.apply).flatMap(ivtOf),
        _.velocity.value
      )

    val minimumDuration: NumericField[RecipeParameterError, Long, MinimumEventDuration] =
      field[Long, MinimumEventDuration](
        "minimumDurationMicros",
        "Minimum event duration; shorter candidates stay unclassified",
        Quantity.Duration,
        NumericBounds.positive
      )(n => spanOf(n).flatMap(minimumOf), _.span.toMicros)

    private def extentAxis(id: String, meaning: String) =
      field[Double, Double](
        id,
        meaning,
        Quantity.Planar(PlanarUnit.Deg),
        NumericBounds.positive
      )(
        extentOf,
        identity
      )
    val idtWidth: NumericField[RecipeParameterError, Double, Double] =
      extentAxis("extentWidthDeg", "Maximum horizontal range of an I-DT candidate")
    val idtHeight: NumericField[RecipeParameterError, Double, Double] =
      extentAxis("extentHeightDeg", "Maximum vertical range of an I-DT candidate")

    private def ekAxis(id: String, meaning: String) =
      field[Double, Double](id, meaning, Quantity.Rate(PlanarUnit.Deg), NumericBounds.positive)(
        etaOf,
        identity
      )
    val ekEtaX: NumericField[RecipeParameterError, Double, Double] =
      ekAxis(
        "etaXDegPerSecond",
        "Supplied horizontal Engbert-Kliegl ellipse threshold; not estimated during replay"
      )
    val ekEtaY: NumericField[RecipeParameterError, Double, Double] =
      ekAxis(
        "etaYDegPerSecond",
        "Supplied vertical Engbert-Kliegl ellipse threshold; not estimated during replay"
      )

    val ekMinimumSamples: NumericField[RecipeParameterError, Int, EkMinimumSamples] =
      field[Int, EkMinimumSamples](
        "minimumSamples",
        "Minimum consecutive above-ellipse samples",
        Quantity.Count(Counted.Samples),
        NumericBounds.atLeastOne
      )(samplesOf, _.value)

    val interpolationGap: NumericField[RecipeParameterError, Long, InterpolationGap] =
      field[Long, InterpolationGap](
        "interpolationGapMicros",
        "Largest missing interval eligible for interpolation; zero disables it",
        Quantity.Duration,
        NumericBounds.nonNegative
      )(n => spanOf(n).flatMap(gapOf), _.span.toMicros)
  end forms

  def sigma[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[Double, Sigma[U], RecipeParameterError] =
    numeric(forms.sigma[U])(sigmaOf[U])

  def bounds[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(Double, Double, Double, Double), Bounds[U], RecipeParameterError] =
    descriptor(V.box("bounds", "Finite increasing x/y bounds", u.planar, Vector.empty))(x =>
      Bounds.of[U](x._1, x._2, x._3, x._4).left.map(RecipeParameterError.Geometry.apply)
    )

  def grid[U <: Unit2D]
      : ParameterDescriptor[(GridId, Frame[U], Int, Int), Grid[U], RecipeParameterError] =
    descriptor(V.grid)(x =>
      Grid.of(x._1, x._2, x._3, x._4).left.map(RecipeParameterError.Geometry.apply)
    )

  def frame[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(FrameId, Bounds[U], YAxis), Frame[U], RecipeParameterError] =
    descriptor(
      V.frameBox(
        "frame",
        "Nominal identity, checked bounds and y-axis direction",
        u.planar,
        false
      )
    )(x => Right(Frame.of(x._1, x._2, x._3)))

  def gaussian[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(Double, EdgePolicy), StudyEstimate[U], RecipeParameterError] =
    descriptor(
      V.view(
        "gaussian",
        "Gaussian standard deviation and explicit edge policy",
        FieldKind.Group(V.estimateCases(u.planar)(1).parts, GroupRule.Independent)
      )
    )(x => sigma[U].construct(x._1).map(s => StudyEstimate.Gaussian(s, x._2)))

  def sigmaX[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[Double, Sigma[U], RecipeParameterError] =
    numeric(forms.sigmaX[U])(sigmaOf[U])

  def sigmaY[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[Double, Sigma[U], RecipeParameterError] =
    numeric(forms.sigmaY[U])(sigmaOf[U])

  def anisotropic[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(Double, Double, EdgePolicy), StudyEstimate[
    U
  ], RecipeParameterError] =
    descriptor(
      V.view(
        "anisotropic",
        "Axis-aligned Gaussian x/y standard deviations and explicit edge policy",
        FieldKind.Group(V.estimateCases(u.planar)(2).parts, GroupRule.Independent)
      )
    )(v =>
      for
        x <- sigmaX[U].construct(v._1)
        y <- sigmaY[U].construct(v._2)
      yield StudyEstimate.Anisotropic(x, y, v._3)
    )

  val syncMark
      : ParameterDescriptor[(String, Instant, Instant), SyncMark, RecipeParameterError] =
    descriptor(V.syncMark("syncMark", "Named common source and target instants"))(x =>
      SyncMark.of(x._1, x._2, x._3).left.map(RecipeParameterError.Synchronization.apply)
    )
  val residualLimit: ParameterDescriptor[Span, SyncResidualLimit, RecipeParameterError] =
    numeric(forms.residualLimit)(residualOf)
  val area
      : ParameterDescriptor[(String, String, Bounds[Px]), RecordingArea, RecipeParameterError] =
    descriptor(V.area("area", "AOI ID, display label and checked pixel bounds"))(x =>
      RecordingArea.of(x._1, x._2, x._3).left.map(RecipeParameterError.Recording.apply)
    )

  val perspective
      : ParameterDescriptor[(Double, Double, Double), Perspective, RecipeParameterError] =
    descriptor(V.perspective)(x =>
      Perspective.millimetres(x._1, x._2, x._3).left.map(RecipeParameterError.Geometry.apply)
    )

  val ivtThreshold: ParameterDescriptor[Velocity[Deg], IvtThreshold, RecipeParameterError] =
    numeric(forms.ivtThreshold)(ivtOf)
  val minimumDuration: ParameterDescriptor[Span, MinimumEventDuration, RecipeParameterError] =
    numeric(forms.minimumDuration)(minimumOf)
  val idtWidth: ParameterDescriptor[Double, Double, RecipeParameterError] =
    numeric(forms.idtWidth)(extentOf)
  val idtHeight: ParameterDescriptor[Double, Double, RecipeParameterError] =
    numeric(forms.idtHeight)(extentOf)
  val ekEtaX: ParameterDescriptor[Double, Double, RecipeParameterError] =
    numeric(forms.ekEtaX)(etaOf)
  val ekEtaY: ParameterDescriptor[Double, Double, RecipeParameterError] =
    numeric(forms.ekEtaY)(etaOf)
  val ekMinimumSamples: ParameterDescriptor[Int, EkMinimumSamples, RecipeParameterError] =
    numeric(forms.ekMinimumSamples)(samplesOf)
  val interpolationGap: ParameterDescriptor[Span, InterpolationGap, RecipeParameterError] =
    numeric(forms.interpolationGap)(gapOf)
  val relativeWindow: ParameterDescriptor[(Span, Span), Window, RecipeParameterError] =
    descriptor(V.relativeWindow)(x =>
      Window.of(x._1, x._2).left.map(RecipeParameterError.Time.apply)
    )
  val studyWindow: ParameterDescriptor[(String, Window), StudyWindow, RecipeParameterError] =
    descriptor(
      V.window("studyWindow", "Named half-open window with positive representable duration")
    )(x => StudyWindow.of(x._1, x._2).left.map(RecipeParameterError.Temporal.apply))
  val repetition: ParameterDescriptor[
    (String, String, String),
    RepetitionContrast,
    RecipeParameterError
  ] =
    descriptor(
      V.phases(
        "repetition",
        "Named focal/reference phase comparison within participant",
        Vector(V.text("name", "The repetition's name"))
      )
    )(x =>
      RepetitionContrast
        .withinParticipant(x._1, x._2, x._3)
        .left
        .map(RecipeParameterError.Temporal.apply)
    )
  val failurePolicy: ParameterDescriptor[Option[Int], FailurePolicy, RecipeParameterError] =
    descriptor(V.failurePolicy) {
      case None    => Right(FailurePolicy.RequireAll)
      case Some(n) =>
        FailurePolicy.successfulOnly(n).left.map(RecipeParameterError.Reduction.apply)
    }

  private def choice[A](view: FieldView): ParameterDescriptor[A, A, RecipeParameterError] =
    descriptor(view)(x => Right(x))
  val weight: ParameterDescriptor[Weight, Weight, RecipeParameterError] = choice(V.weight)
  val edges: ParameterDescriptor[EdgePolicy, EdgePolicy, RecipeParameterError] =
    choice(V.edges)
  val boundary: ParameterDescriptor[FixationBoundary, FixationBoundary, RecipeParameterError] =
    choice(
      V.choice(
        "temporal.boundary",
        "Clip fixation duration to window and observed coverage, or require full containment",
        FixationBoundary.values.toVector
      )
    )
  val syncModel: ParameterDescriptor[SyncFitMode, SyncFitMode, RecipeParameterError] =
    choice(V.syncModel)

/** Canonical description values, paired with stable, versioned scientific meaning. */
final class InspectedParameter private[plan] (
    val info: ParameterInfo,
    val values: Vector[Provenance.Param],
    val children: Vector[InspectedParameter] = Vector.empty
)
final class RecipeInspection private[plan] (
    val fields: Vector[InspectedParameter],
    val conventions: Vector[String],
    val execution: ExecutionCapability
):
  def description: Vector[(String, Vector[Provenance.Param])] =
    fields.map(f => f.info.id -> f.values)

  /** Every field as a host sees it, in description order. */
  def views: Vector[FieldView] = fields.map(_.info.view)

/** Metadata tables cover the existing scientific description exactly.
  * Adding a description field without metadata fails inspection visibly.
  */
object RecipeDescriptors:
  import RecipeViews as V

  private def info(view: FieldView): ParameterInfo = ParameterInfo.literal(view)
  private def reference(id: String, meaning: String): ParameterInfo =
    info(V.reference(id, meaning))

  private def studyFields(unit: PlanarUnit) = Vector(
    reference("input", "Semantic identity of ordered scientific input"),
    reference("layout", "Versioned typed participant/stimulus/phase projections"),
    reference("method", "Versioned comparison method"),
    info(
      V.phases(
        "phases",
        "Focal and reference phases; exhaustive controls stay within participant"
      )
    ),
    RecipeParameters.weight.info,
    RecipeParameters.failurePolicy.info,
    info(
      V.frameBox(
        "frame",
        "Nominal frame ID, spatial unit, xmin/ymin/xmax/ymax and y-axis direction",
        unit,
        withUnit = true
      )
    ),
    RecipeParameters.grid[Deg].info,
    info(
      V.frameBox(
        "admission",
        "Admission frame of a windowed study: ID, unit, bounds and y-axis; the grid lies on the window",
        unit,
        withUnit = true
      )
    ),
    info(
      V.box(
        "window",
        "Analysis window: its frame ID and half-open region [xmin, xmax) x [ymin, ymax) of the admission frame",
        unit,
        Vector(V.reference("frameId", "The window's frame identity"))
      )
    ),
    info(
      V.choice(
        "offWindow",
        "Fixations on the screen but outside the window: excluded from the map, or failing the trial",
        OffWindowPolicy.values.toVector
      )
    ),
    info(V.angularScale(unit)),
    info(V.pairing),
    info(V.initialFixations(unit))
  )

  private def inspect(
      description: Vector[(String, Vector[Provenance.Param])],
      metadata: Vector[ParameterInfo],
      conventions: Vector[String],
      children: Map[String, Vector[InspectedParameter]] = Map.empty,
      execution: ExecutionCapability = ExecutionCapability.SynchronousWholeOperation
  ): Either[DescriptorError, RecipeInspection] =
    val byId    = metadata.map(m => m.id -> m).toMap
    val missing = description.map(_._1).filterNot(byId.contains)
    if metadata.map(_.id).distinct.size != metadata.size then
      Left(DescriptorError.DuplicateFields(metadata.map(_.id)))
    else if missing.nonEmpty then Left(DescriptorError.UnexplainedFields(missing))
    else
      description
        .traverse { case (id, values) =>
          byId
            .get(id)
            .toRight(DescriptorError.UnexplainedFields(Vector(id)))
            .map(new InspectedParameter(_, values, children.getOrElse(id, Vector.empty)))
        }
        .map(new RecipeInspection(_, conventions, execution))

  private def prefixed(prefix: String, p: ParameterInfo): ParameterInfo =
    p.prefixed(prefix)

  def study[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: StudyPlan[K, U, P, S, D]
  ): Either[DescriptorError, RecipeInspection] =
    for
      method <- plan.method.descriptor.toRight(DescriptorError.MissingMethod(plan.method.id))
      _      <- Either.cond(
        method.id == plan.method.id,
        (),
        DescriptorError.MethodIdentity(plan.method.id, method.id)
      )
      _ <- Either.cond(
        method.execution == plan.method.capability,
        (),
        DescriptorError.ExecutionMismatch(method.execution, plan.method.capability)
      )
      _ <- method.verify(
        plan.parameters,
        plan.method.parameters(plan.parameters),
        plan.method.difference.components
      )
      result <- inspect(
        plan.description,
        studyFields(UnitLabel[U].planar) ++
          plan.scales.indices.map(i =>
            info(
              V.view(
                s"scale.$i",
                "The scale as declared in degrees; estimate.i is its resolved frame-unit equivalent",
                FieldKind.Variant(V.estimateCases(PlanarUnit.Deg))
              )
            )
          ) ++
          method.parameters.fields.map(f => prefixed("method.", f.descriptor.info)) ++
          plan.estimates.zipWithIndex.map { case (estimate, i) =>
            val meaning = estimate match
              case StudyEstimate.Binned() =>
                "Binned occupancy normalized to unit mass; no smoothing"
              case StudyEstimate.Gaussian(_, _) =>
                "Gaussian standard deviation and edge policy; normalized to unit mass"
              case StudyEstimate.Anisotropic(_, _, _) =>
                "Axis-aligned Gaussian x/y standard deviations and edge policy; normalized to unit mass"
            info(
              V.view(
                s"estimate.$i",
                meaning,
                FieldKind.Variant(V.estimateCases(UnitLabel[U].planar))
              )
            )
          },
        Vector(
          "Exhaustive focal-major pairing within participant",
          "Matched stimulus minus different-stimulus control; explicit failure denominators",
          "Grid rows follow the declared frame y-axis; scales are never pooled"
        ),
        plan.estimates.zipWithIndex.map { case (estimate, i) =>
          val fields = estimate match
            case StudyEstimate.Anisotropic(x, y, edges) =>
              Vector(
                new InspectedParameter(
                  RecipeParameters.sigmaX[U].info,
                  Vector(Provenance.Param.Num(x.value))
                ),
                new InspectedParameter(
                  RecipeParameters.sigmaY[U].info,
                  Vector(Provenance.Param.Num(y.value))
                ),
                new InspectedParameter(
                  RecipeParameters.edges.info,
                  Vector(Provenance.Param.Text(edges.toString))
                )
              )
            case StudyEstimate.Binned()               => Vector.empty
            case StudyEstimate.Gaussian(sigma, edges) =>
              Vector(
                new InspectedParameter(
                  RecipeParameters.sigma[U].info,
                  Vector(Provenance.Param.Num(sigma.value))
                ),
                new InspectedParameter(
                  RecipeParameters.edges.info,
                  Vector(Provenance.Param.Text(edges.toString))
                )
              )
          s"estimate.$i" -> fields
        }.toMap,
        plan.method.capability
      )
    yield result

  def recording[P](plan: RecordingPlan[P]): Either[DescriptorError, RecipeInspection] =
    for
      method <- plan.method.descriptor.toRight(DescriptorError.MissingMethod(plan.method.id))
      _      <- Either.cond(
        method.id == plan.method.id,
        (),
        DescriptorError.MethodIdentity(plan.method.id, method.id)
      )
      _ <- method.parameters.verify(plan.parameters, plan.method.parameters(plan.parameters))
      result <- inspect(
        plan.description,
        Vector(
          reference("input", "Semantic recording input identity"),
          reference("source", "Source recording identity"),
          info(
            V.frameBox(
              "frame",
              "Pixel display frame ID, bounds and y-axis direction",
              PlanarUnit.Px,
              withUnit = false
            )
          ),
          info(
            V.view(
              "clocks",
              "Distinct tracker and analysis clock identities",
              FieldKind.Group(
                Vector(
                  V.reference("tracker", "The tracker clock"),
                  V.reference("analysis", "The analysis clock")
                ),
                GroupRule.Distinct(
                  Vector(FieldId.literal("tracker"), FieldId.literal("analysis"))
                )
              )
            )
          ),
          reference("angularFrame", "Output frame identity in degrees of visual angle"),
          info(
            V.view(
              "viewing",
              RecipeParameters.perspective.info.meaning,
              FieldKind.Optional(
                RecipeParameters.perspective.info.view,
                "No physical viewing geometry is declared"
              )
            )
          ),
          RecipeParameters.syncModel.info,
          info(
            V.view(
              "residualLimitMicros",
              "Optional nonnegative synchronization residual rejection threshold",
              FieldKind.Optional(
                RecipeParameters.residualLimit.info.view,
                "Every synchronization residual is accepted"
              )
            )
          ),
          RecipeParameters.interpolationGap.info,
          reference("detector", "Versioned detector factory")
        ) ++ method.parameters.fields.map(f => prefixed("detector.", f.descriptor.info)) ++
          plan.marks.indices.map(i =>
            info(
              V.syncMark(s"sync.$i", "Named common mark with exact source/target microseconds")
            )
          ) ++
          plan.areas.indices.map(i =>
            info(V.area(s"area.$i", "AOI ID, display label and ordered pixel bounds"))
          ),
        Vector(
          "Tracker-to-analysis synchronization precedes visual-angle conversion",
          "Interpolation retains source support; detector card specifies assumptions and deviations"
        )
      )
    yield result

  def temporal[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D]
  ): Either[DescriptorError, RecipeInspection] =
    study(plan.base).flatMap { base =>
      inspect(
        plan.description,
        base.fields.map(_.info) ++ Vector(
          reference(
            "temporal.input",
            "Semantic identity of study, measured anchors and observed coverage"
          ),
          RecipeParameters.boundary.info,
          reference("temporal.scope", "Repetitions compare phases within participant")
        ) ++ plan.windows.indices.map(i =>
          info(V.window(s"window.$i", RecipeParameters.studyWindow.info.meaning))
        ) ++
          plan.repetitions.indices.map(i =>
            info(
              V.phases(
                s"repetition.$i",
                RecipeParameters.repetition.info.meaning,
                Vector(V.text("name", "The repetition's name"))
              )
            )
          ),
        base.conventions ++ Vector(
          "Half-open windows resolve against measured anchors",
          "Duration weighting uses observed coverage; gaps are missing time, not zero gaze",
          "Each repetition supplies its focal/reference phases; base phases do not select temporal cells"
        ),
        base.fields.map(f => f.info.id -> f.children).toMap,
        // Every cell runs the base study's cursor, so the temporal plan executes
        // exactly as its base method does.
        base.execution
      )
    }
