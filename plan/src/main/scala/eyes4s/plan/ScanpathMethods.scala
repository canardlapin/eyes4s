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
import eyes4s.core.*
import eyes4s.kernel.*

object ScanpathMethodDefinitions:
  val multiMatch: DefinitionId = DefinitionId.builtIn("eyes4s.scanpath-multimatch", 1)
  val overlap: DefinitionId    = DefinitionId.builtIn("eyes4s.fixation-overlap", 1)
  val transport: DefinitionId  = DefinitionId.builtIn("eyes4s.fixation-transport", 1)

enum ScanpathMethodKind derives CanEqual:
  case MultiMatch, Overlap, Transport
  def id: DefinitionId = this match
    case MultiMatch => ScanpathMethodDefinitions.multiMatch
    case Overlap    => ScanpathMethodDefinitions.overlap
    case Transport  => ScanpathMethodDefinitions.transport

enum MultiMatchCardinality derives CanEqual:
  case NativeTwoFixations, BaselineThreeFixations
  def minimum: Int = this match
    case NativeTwoFixations     => 2
    case BaselineThreeFixations => BaselineScanpath.MinimumFixations

enum ScanpathComparisonError:
  case Parameter(name: String, value: Long)
  case AlignmentSize(leftFixations: Int, rightFixations: Int, cells: Long, maximum: Long)
  case Alignment(underlying: CompareError)
  case Fixations(underlying: FixationComparisonError)
  def message: String = this match
    case Parameter(name, value) =>
      s"Scanpath comparison parameter $name=$value must be positive."
    case AlignmentSize(left, right, cells, maximum) =>
      s"MultiMatch leftFixations=$left, rightFixations=$right requires alignmentCells=$cells, exceeding maximum=$maximum."
    case Alignment(error) => error.message
    case Fixations(error) => error.message

object ScanpathComparisonError:
  given diagnose: Diagnose[ScanpathComparisonError, Nothing] =
    Diagnose.derived(ScanpathMethodDiagnostics.comparison)(_.message)

/** Bounds the two transition-lattice arrays before MultiMatch allocates them.
  * This is an allocation/work admission bound, not an intra-pair time quantum.
  */
final class AlignmentCellLimit private (val value: Long)
object AlignmentCellLimit:
  val default: AlignmentCellLimit = new AlignmentCellLimit(1000000L)
  def of(value: Long): Either[ScanpathComparisonError, AlignmentCellLimit] =
    Either.cond(
      value > 0,
      new AlignmentCellLimit(value),
      ScanpathComparisonError.Parameter("maximumAlignmentCells", value)
    )

/** Exact caller-ordered queries retain duplicates and use a stated clock.
  * Threshold is strict (<); missing support keeps its declared denominator.
  */
final class FixationOverlapSpec[U <: Unit2D] private (
    val clock: ClockId,
    val queries: Vector[Instant],
    val threshold: OverlapThreshold[U],
    val distance: FixationGroundDistance,
    val endpoint: TrajectoryEndpoint,
    val missing: MissingOverlapPolicy
)
object FixationOverlapSpec:
  def of[U <: Unit2D](
      clock: ClockId,
      queries: Vector[Instant],
      threshold: OverlapThreshold[U],
      distance: FixationGroundDistance,
      endpoint: TrajectoryEndpoint,
      missing: MissingOverlapPolicy
  ): Either[FixationComparisonError, FixationOverlapSpec[U]] =
    Either.cond(
      queries.nonEmpty,
      new FixationOverlapSpec(clock, queries, threshold, distance, endpoint, missing),
      FixationComparisonError.EmptyQueries
    )

/** Full method outputs retain query failures and transport diagnostics.
  * A strict missing-overlap policy can refuse its scalar summary while the
  * original requested query rows remain available in the output.
  */
enum ScanpathMethodOutput:
  case MultiMatch(value: MultiMatchScore)
  case Overlap(value: FixationOverlapResult)
  case Transport(value: FixationTransportResult)
  def scores: Either[FixationComparisonError, Vector[(String, Double)]] = this match
    case MultiMatch(value) =>
      Right(
        Vector(
          "shape"     -> value.shape,
          "direction" -> value.direction,
          "length"    -> value.length,
          "position"  -> value.position,
          "duration"  -> value.duration
        )
      )
    case Overlap(value)   => value.similarity.map(score => Vector("overlap" -> score.value))
    case Transport(value) => value.similarity.map(score => Vector("transport" -> score.value))

/** Registered comparisons ship as whole synchronous pair operations. Their
  * scientific conventions and checked limits are explicit recipe parameters.
  */
enum ScanpathMethod[U <: Unit2D]:
  case MultiMatch(cardinality: MultiMatchCardinality, maximumAlignmentCells: AlignmentCellLimit)
  case Overlap(specification: FixationOverlapSpec[U])
  case Transport(configuration: FixationTransportConfig[U])

  def kind: ScanpathMethodKind = this match
    case MultiMatch(_, _) => ScanpathMethodKind.MultiMatch
    case Overlap(_)       => ScanpathMethodKind.Overlap
    case Transport(_)     => ScanpathMethodKind.Transport
  def id: DefinitionId               = kind.id
  def execution: ExecutionCapability = ExecutionCapability.SynchronousWholeOperation
  def componentNames: Vector[String] = this match
    case MultiMatch(_, _) => Vector("shape", "direction", "length", "position", "duration")
    case Overlap(_)       => Vector("overlap")
    case Transport(_)     => Vector("transport")
  def properties: Set[ComparisonProperty] =
    val symmetric = this match
      case MultiMatch(_, _) | Overlap(_) => Set(ComparisonProperty.Symmetric)
      case Transport(_)                  => Set.empty[ComparisonProperty]
    symmetric ++ Set(ComparisonProperty.Bounded, ComparisonProperty.NonNegative)
  def parameters: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    this match
      case MultiMatch(cardinality, limit) =>
        Vector(
          "kind"                  -> Vector(Text("multiMatch")),
          "cardinality"           -> Vector(Text(cardinality.toString)),
          "maximumAlignmentCells" -> Vector(Text(limit.value.toString)),
          "alignmentConvention"   -> Vector(
            Text(eyes4s.compare.MultiMatch.alignmentConvention.toString)
          )
        )
      case Overlap(spec) =>
        Vector(
          "kind"           -> Vector(Text("overlap")),
          "clock"          -> Vector(Text(spec.clock.name)),
          "queries"        -> spec.queries.map(at => Text(at.toMicros.toString)),
          "threshold"      -> Vector(Num(spec.threshold.value)),
          "distance"       -> Vector(Text(spec.distance.toString)),
          "normConvention" -> Vector(Text(spec.distance.normConvention.toString)),
          "endpoint"       -> Vector(Text(spec.endpoint.toString)),
          "missing"        -> Vector(Text(spec.missing.toString))
        )
      case Transport(config) =>
        Vector(
          "kind"              -> Vector(Text("transport")),
          "xScale"            -> Vector(Num(config.xScale)),
          "yScale"            -> Vector(Num(config.yScale)),
          "timeScaleMicros"   -> Vector(Text(config.timeScale.toMicros.toString)),
          "timeWeight"        -> Vector(Num(config.timeWeight)),
          "lambda"            -> Vector(Num(config.lambda)),
          "maximumIterations" -> Vector(Text(config.maximumIterations.toString)),
          "marginalTolerance" -> Vector(Num(config.marginalTolerance)),
          "maximumCosts"      -> Vector(Text(config.maximumCosts.toString)),
          "maximumWork"       -> Vector(Text(config.maximumWork.toString))
        )

  def info: MeasureInfo = this match
    case MultiMatch(_, _) => eyes4s.compare.MultiMatch[U].info
    case Overlap(_)       =>
      MeasureInfo(
        "Fixation overlap",
        "Fraction of requested latest-onset queries within a strict spatial threshold; explicit missing-support denominator",
        MeasureScale.Bounded(0, 1),
        None
      )
    case Transport(_) =>
      MeasureInfo(
        "Fixation transport similarity",
        "1 / (1 + rootCost) from duration-weighted entropic fixation transport; finite iteration is not a symmetry or exact EMD claim",
        MeasureScale.Bounded(0, 1),
        None
      )
  def description(using UnitLabel[U]): Vector[(String, Vector[Provenance.Param])] =
    Vector(
      "method"     -> Vector(Provenance.Param.Text(s"${id.name}@${id.version}")),
      "unit"       -> Vector(Provenance.Param.Text(summon[UnitLabel[U]].symbol)),
      "components" -> componentNames.map(Provenance.Param.Text.apply)
    ) ++ parameters
  def inspect(using UnitLabel[U]): Either[DescriptorError, RecipeInspection] =
    ScanpathMethodDescriptions.inspect(this)

  def parameterHash: ContentHash = Provenance
    .Step(
      "scanpath-method",
      parameters.flatMap { (name, values) =>
        values.zipWithIndex.map((value, index) => s"$name.$index" -> value)
      }
    )
    .digest

  def compare(
      left: Scanpath[U],
      right: Scanpath[U]
  ): Either[ScanpathComparisonError, ScanpathMethodOutput] = this match
    case MultiMatch(cardinality, limit) =>
      val cells = (left.n.toLong - 1) * (right.n.toLong - 1)
      for
        _ <- Either.cond(
          left.n >= cardinality.minimum,
          (),
          ScanpathComparisonError.Alignment(
            CompareError.TooShort("left scanpath", left.n, cardinality.minimum)
          )
        )
        _ <- Either.cond(
          right.n >= cardinality.minimum,
          (),
          ScanpathComparisonError.Alignment(
            CompareError.TooShort("right scanpath", right.n, cardinality.minimum)
          )
        )
        _ <- Either.cond(
          cells <= limit.value,
          (),
          ScanpathComparisonError.AlignmentSize(left.n, right.n, cells, limit.value)
        )
        result <- eyes4s.compare
          .MultiMatch[U]
          .compare(left, right)
          .left
          .map(ScanpathComparisonError.Alignment.apply)
      yield ScanpathMethodOutput.MultiMatch(result)
    case Overlap(spec) =>
      FixationOverlap
        .compare(
          FixationTrajectory.fromScanpath(left),
          FixationTrajectory.fromScanpath(right),
          spec.clock,
          spec.queries,
          spec.threshold,
          spec.distance,
          spec.endpoint,
          spec.missing
        )
        .left
        .map(ScanpathComparisonError.Fixations.apply)
        .map(ScanpathMethodOutput.Overlap.apply)
    case Transport(config) =>
      FixationTransport
        .compare(left, right, config)
        .left
        .map(ScanpathComparisonError.Fixations.apply)
        .map(ScanpathMethodOutput.Transport.apply)

object ScanpathMethods:
  val all: Vector[ScanpathMethodKind]                       = ScanpathMethodKind.values.toVector
  def resolve(id: DefinitionId): Option[ScanpathMethodKind] = all.find(_.id == id)
  def multiMatch[U <: Unit2D]: ScanpathMethod[U]            =
    ScanpathMethod.MultiMatch(
      MultiMatchCardinality.NativeTwoFixations,
      AlignmentCellLimit.default
    )
  def baselineMultiMatch[U <: Unit2D]: ScanpathMethod[U] =
    ScanpathMethod.MultiMatch(
      MultiMatchCardinality.BaselineThreeFixations,
      AlignmentCellLimit.default
    )

object ScanpathMethodDescriptions:
  def inspect[U <: Unit2D: UnitLabel](
      method: ScanpathMethod[U]
  ): Either[DescriptorError, RecipeInspection] =
    import cats.syntax.all.*
    val meanings = Map(
      "method"     -> "Registered method identity including version",
      "unit"       -> "Static spatial unit of the scanpaths, thresholds and spatial scales",
      "components" -> "Ordered dimensionless similarity components; larger is closer",
      "kind" -> "MultiMatch, latest-onset fixation overlap or entropic fixation transport",
      "cardinality" -> "Two fixations per operand for native MultiMatch, or the three-fixation baseline admission",
      "maximumAlignmentCells" -> "Maximum product of operand transition counts before alignment matrix allocation",
      "alignmentConvention" -> "Canonical operands and exact-cost traceback policy, including exact and near ties",
      "clock" -> "Nominal timeline shared by overlap queries and both trajectories",
      "queries" -> "Ordered exact signed64 microsecond instants; repetitions contribute repeatedly",
      "threshold" -> s"Strict distance threshold in ${summon[UnitLabel[U]].symbol}; equality is not overlap",
      "distance"       -> "Euclidean or Manhattan distance between latest-onset positions",
      "normConvention" -> "Named binary64 operation order for the selected distance",
      "endpoint"       -> "Support ends at the final onset or holds its fixation indefinitely",
      "missing" -> "Count missing queries as non-overlap or refuse the scalar summary; retain all query rows",
      "xScale" -> s"Positive horizontal ground-cost scale in ${summon[UnitLabel[U]].symbol}",
      "yScale" -> s"Positive vertical ground-cost scale in ${summon[UnitLabel[U]].symbol}",
      "timeScaleMicros" -> "Positive exact microsecond scale for differences between fixation onsets",
      "timeWeight"        -> "Nonnegative multiplier of scaled temporal displacement",
      "lambda"            -> "Positive entropy regularisation, not its inverse",
      "maximumIterations" -> "Maximum alternating row/column scaling iterations",
      "marginalTolerance" -> "Maximum admitted absolute marginal residual",
      "maximumCosts"      -> "Maximum product of fixation counts before cost-matrix allocation",
      "maximumWork" -> "Exact signed64 cost-cell iteration budget, checked before another solver iteration"
    )
    method.description
      .traverse { (id, values) =>
        meanings.get(id).toRight(DescriptorError.UnexplainedFields(Vector(id))).map { meaning =>
          new InspectedParameter(
            ParameterInfo.literal(RecipeViews.reference(id, meaning)),
            values
          )
        }
      }
      .map(fields =>
        new RecipeInspection(
          fields,
          Vector(
            "Each method is one whole pair comparison; a pair quantum cannot interrupt alignment, a query batch or transport iterations",
            "MultiMatch compares geometry and durations and needs no shared absolute timeline",
            "Overlap and transport require matching nominal clocks; no synchronization is inferred",
            "Overlap uses latest onsets, including intervals beyond fixation offsets; missing query rows are retained",
            "Transport retains squared/root cost, iterations and residual; its finite solver is neither an exact EMD backend nor a symmetric comparison"
          ),
          ExecutionCapability.SynchronousWholeOperation
        )
      )
