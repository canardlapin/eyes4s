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

/** Concrete typed construction routes. No universal scientific defaults are imposed. */
object RecipeParameters:
  private def descriptor[R, A](
      id: String,
      meaning: String,
      units: ParameterUnits,
      domain: ParameterDomain
  )(
      construct: R => Either[RecipeParameterError, A]
  ): ParameterDescriptor[R, A, RecipeParameterError] =
    new ParameterDescriptor(
      ParameterInfo.literal(id, meaning, units, domain),
      construct,
      _.message
    )

  def sigma[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[Double, Sigma[U], RecipeParameterError] =
    descriptor(
      "sigma",
      "Gaussian standard deviation in the input frame's units",
      ParameterUnits.Spatial(u.symbol),
      ParameterDomain.PositiveFinite
    )(x => Sigma.of[U](x).left.map(RecipeParameterError.Geometry.apply))

  def bounds[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(Double, Double, Double, Double), Bounds[U], RecipeParameterError] =
    descriptor(
      "bounds",
      "Finite increasing x/y bounds",
      ParameterUnits.Spatial(u.symbol),
      ParameterDomain.OrderedFiniteBounds
    )(x => Bounds.of[U](x._1, x._2, x._3, x._4).left.map(RecipeParameterError.Geometry.apply))

  def grid[U <: Unit2D]
      : ParameterDescriptor[(GridId, Frame[U], Int, Int), Grid[U], RecipeParameterError] =
    descriptor(
      "grid",
      "Nominal grid and frame; columns nx and rows ny, row-major cells",
      ParameterUnits.Cells,
      ParameterDomain.PositiveGridDimensions
    )(x => Grid.of(x._1, x._2, x._3, x._4).left.map(RecipeParameterError.Geometry.apply))

  def frame[U <: Unit2D](using
      u: UnitLabel[U]
  ): ParameterDescriptor[(FrameId, Bounds[U], YAxis), Frame[U], RecipeParameterError] =
    descriptor(
      "frame",
      "Nominal identity, checked bounds and y-axis direction",
      ParameterUnits.Spatial(u.symbol),
      ParameterDomain.DomainValue("Frame.of")
    )(x => Right(Frame.of(x._1, x._2, x._3)))

  def gaussian[U <: Unit2D: UnitLabel]
      : ParameterDescriptor[(Double, EdgePolicy), StudyEstimate[U], RecipeParameterError] =
    descriptor(
      "gaussian",
      "Gaussian standard deviation and explicit edge policy",
      ParameterUnits.Mixed,
      ParameterDomain.DomainValue("Sigma.of and StudyEstimate.Gaussian")
    )(x => sigma[U].construct(x._1).map(s => StudyEstimate.Gaussian(s, x._2)))

  val syncMark
      : ParameterDescriptor[(String, Instant, Instant), SyncMark, RecipeParameterError] =
    descriptor(
      "syncMark",
      "Named common source and target instants",
      ParameterUnits.Microseconds,
      ParameterDomain.SignedMicroseconds
    )(x => SyncMark.of(x._1, x._2, x._3).left.map(RecipeParameterError.Synchronization.apply))
  val residualLimit: ParameterDescriptor[Span, SyncResidualLimit, RecipeParameterError] =
    descriptor(
      "residualLimitMicros",
      "Nonnegative synchronization residual rejection threshold",
      ParameterUnits.Microseconds,
      ParameterDomain.NonNegativeMicroseconds
    )(x => SyncResidualLimit.of(x).left.map(RecipeParameterError.Synchronization.apply))
  val area
      : ParameterDescriptor[(String, String, Bounds[Px]), RecordingArea, RecipeParameterError] =
    descriptor(
      "area",
      "AOI ID, display label and checked pixel bounds",
      ParameterUnits.Spatial("px"),
      ParameterDomain.DomainValue("RecordingArea.of")
    )(x => RecordingArea.of(x._1, x._2, x._3).left.map(RecipeParameterError.Recording.apply))

  val perspective
      : ParameterDescriptor[(Double, Double, Double), Perspective, RecipeParameterError] =
    descriptor(
      "viewing",
      "Viewing distance, physical width and height in millimetres",
      ParameterUnits.Millimetres,
      ParameterDomain.PositivePhysicalDimensions
    )(x =>
      Perspective.millimetres(x._1, x._2, x._3).left.map(RecipeParameterError.Geometry.apply)
    )

  val ivtThreshold: ParameterDescriptor[Velocity[Deg], IvtThreshold, RecipeParameterError] =
    descriptor(
      "thresholdDegPerSecond",
      "Strictly positive physical I-VT velocity threshold",
      ParameterUnits.PerSecond("deg"),
      ParameterDomain.PositiveFinite
    )(x => IvtThreshold.of(x).left.map(RecipeParameterError.Configuration.apply))
  val minimumDuration: ParameterDescriptor[Span, MinimumEventDuration, RecipeParameterError] =
    descriptor(
      "minimumDurationMicros",
      "Minimum event duration; shorter candidates stay unclassified",
      ParameterUnits.Microseconds,
      ParameterDomain.PositiveMicroseconds
    )(x => MinimumEventDuration.of(x).left.map(RecipeParameterError.Configuration.apply))
  val interpolationGap: ParameterDescriptor[Span, InterpolationGap, RecipeParameterError] =
    descriptor(
      "interpolationGapMicros",
      "Largest missing interval eligible for interpolation; zero disables it",
      ParameterUnits.Microseconds,
      ParameterDomain.NonNegativeMicroseconds
    )(x => InterpolationGap.of(x).left.map(RecipeParameterError.Configuration.apply))
  val relativeWindow: ParameterDescriptor[(Span, Span), Window, RecipeParameterError] =
    descriptor(
      "window",
      "Half-open signed offsets relative to a measured trial anchor",
      ParameterUnits.Microseconds,
      ParameterDomain.DomainValue("Window.of")
    )(x => Window.of(x._1, x._2).left.map(RecipeParameterError.Time.apply))
  val studyWindow: ParameterDescriptor[(String, Window), StudyWindow, RecipeParameterError] =
    descriptor(
      "studyWindow",
      "Named half-open window with positive representable duration",
      ParameterUnits.Microseconds,
      ParameterDomain.PositiveHalfOpenWindow
    )(x => StudyWindow.of(x._1, x._2).left.map(RecipeParameterError.Temporal.apply))
  val repetition: ParameterDescriptor[
    (String, String, String),
    RepetitionContrast,
    RecipeParameterError
  ] =
    descriptor(
      "repetition",
      "Named focal/reference phase comparison within participant",
      ParameterUnits.NominalIdentity,
      ParameterDomain.DistinctNonEmptyPhases
    )(x =>
      RepetitionContrast
        .withinParticipant(x._1, x._2, x._3)
        .left
        .map(RecipeParameterError.Temporal.apply)
    )
  val failurePolicy: ParameterDescriptor[Option[Int], FailurePolicy, RecipeParameterError] =
    descriptor(
      "failurePolicy",
      "Require every selected pair, or explicitly require a minimum successful count",
      ParameterUnits.Dimensionless,
      ParameterDomain.DomainValue("FailurePolicy.successfulOnly")
    ) {
      case None    => Right(FailurePolicy.RequireAll)
      case Some(n) =>
        FailurePolicy.successfulOnly(n).left.map(RecipeParameterError.Reduction.apply)
    }

  private def choice[A](
      id: String,
      meaning: String,
      values: Vector[A]
  ): ParameterDescriptor[A, A, RecipeParameterError] =
    descriptor(
      id,
      meaning,
      ParameterUnits.Dimensionless,
      ParameterDomain.Alternatives(values.map(_.toString))
    )(x => Right(x))
  val weight: ParameterDescriptor[Weight, Weight, RecipeParameterError] =
    choice(
      "weight",
      "Per-fixation mass: uniform count or measured duration",
      Weight.values.toVector
    )
  val edges: ParameterDescriptor[EdgePolicy, EdgePolicy, RecipeParameterError] =
    choice(
      "edges",
      "Truncate loses kernel mass outside the grid; Renormalise preserves each point's mass",
      EdgePolicy.values.toVector
    )
  val boundary: ParameterDescriptor[FixationBoundary, FixationBoundary, RecipeParameterError] =
    choice(
      "temporal.boundary",
      "Clip fixation duration to window and observed coverage, or require full containment",
      FixationBoundary.values.toVector
    )
  val syncModel: ParameterDescriptor[SyncFitMode, SyncFitMode, RecipeParameterError] =
    choice(
      "syncModel",
      "Fit the declared clock mapping from named common marks",
      SyncFitMode.values.toVector
    )

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

/** Metadata tables cover the existing scientific description exactly.
  * Adding a description field without metadata fails inspection visibly.
  */
object RecipeDescriptors:
  private def info(
      id: String,
      meaning: String,
      units: ParameterUnits = ParameterUnits.Mixed,
      allowed: ParameterDomain = ParameterDomain.NominalReference
  ): ParameterInfo =
    ParameterInfo.literal(id, meaning, units, allowed)
  private val studyFields = Vector(
    info("input", "Semantic identity of ordered scientific input"),
    info("layout", "Versioned typed participant/stimulus/phase projections"),
    info("method", "Versioned comparison method"),
    info(
      "phases",
      "Focal and reference phases; exhaustive controls stay within participant",
      allowed = ParameterDomain.DistinctNonEmptyPhases
    ),
    RecipeParameters.weight.info,
    RecipeParameters.failurePolicy.info,
    info(
      "frame",
      "Nominal frame ID, spatial unit, xmin/ymin/xmax/ymax and y-axis direction",
      allowed = ParameterDomain.OrderedFiniteBounds
    ),
    RecipeParameters.grid[Deg].info
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
        studyFields ++
          method.parameters.fields.map(f => prefixed("method.", f.descriptor.info)) ++
          plan.estimates.zipWithIndex.map { case (estimate, i) =>
            val meaning = estimate match
              case StudyEstimate.Binned() =>
                "Binned occupancy normalized to unit mass; no smoothing"
              case StudyEstimate.Gaussian(_, _) =>
                "Gaussian standard deviation and edge policy; normalized to unit mass"
            info(
              s"estimate.$i",
              meaning,
              allowed = ParameterDomain.DomainValue("StudyEstimate")
            )
          },
        Vector(
          "Exhaustive focal-major pairing within participant",
          "Matched stimulus minus different-stimulus control; explicit failure denominators",
          "Grid rows follow the declared frame y-axis; scales are never pooled"
        ),
        plan.estimates.zipWithIndex.map { case (estimate, i) =>
          val fields = estimate match
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
          info("input", "Semantic recording input identity"),
          info("source", "Source recording identity"),
          info("frame", "Pixel display frame ID, bounds and y-axis direction"),
          info("clocks", "Distinct tracker and analysis clock identities"),
          info("angularFrame", "Output frame identity in degrees of visual angle"),
          RecipeParameters.perspective.info,
          RecipeParameters.syncModel.info,
          info(
            "residualLimitMicros",
            "Optional nonnegative synchronization residual rejection threshold",
            ParameterUnits.Microseconds,
            ParameterDomain.NonNegativeMicroseconds
          ),
          RecipeParameters.interpolationGap.info,
          info("detector", "Versioned detector factory")
        ) ++ method.parameters.fields.map(f => prefixed("detector.", f.descriptor.info)) ++
          plan.marks.indices.map(i =>
            info(
              s"sync.$i",
              "Named common mark with exact source/target microseconds",
              ParameterUnits.Microseconds,
              ParameterDomain.SignedMicroseconds
            )
          ) ++
          plan.areas.indices.map(i =>
            info(
              s"area.$i",
              "AOI ID, display label and ordered pixel bounds",
              ParameterUnits.Spatial("px"),
              ParameterDomain.OrderedFiniteBounds
            )
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
          info(
            "temporal.input",
            "Semantic identity of study, measured anchors and observed coverage"
          ),
          RecipeParameters.boundary.info,
          info("temporal.scope", "Repetitions compare phases within participant")
        ) ++ plan.windows.indices.map(i =>
          info(
            s"window.$i",
            RecipeParameters.studyWindow.info.meaning,
            ParameterUnits.Microseconds,
            ParameterDomain.PositiveHalfOpenWindow
          )
        ) ++
          plan.repetitions.indices.map(i =>
            info(
              s"repetition.$i",
              RecipeParameters.repetition.info.meaning,
              ParameterUnits.NominalIdentity,
              ParameterDomain.DistinctNonEmptyPhases
            )
          ),
        base.conventions ++ Vector(
          "Half-open windows resolve against measured anchors",
          "Duration weighting uses observed coverage; gaps are missing time, not zero gaze",
          "Each repetition supplies its focal/reference phases; base phases do not select temporal cells"
        ),
        base.fields.map(f => f.info.id -> f.children).toMap
      )
    }
