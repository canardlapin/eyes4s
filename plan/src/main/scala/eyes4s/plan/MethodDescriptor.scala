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
import eyes4s.detect.*
import eyes4s.design.SignedDifference
import eyes4s.kernel.*

enum ParameterUnits derives CanEqual:
  case Dimensionless, Microseconds, Millimetres, Cells, NominalIdentity, Mixed
  case Spatial(symbol: String)
  case PerSecond(symbol: String)

/** Informational constructor contracts, never a second validation implementation. */
enum ParameterDomain derives CanEqual:
  case PositiveFinite, NonNegativeFinite, PositiveMicroseconds, NonNegativeMicroseconds
  case SignedMicroseconds, PositiveGridDimensions, OrderedFiniteBounds,
    PositivePhysicalDimensions
  case PositiveHalfOpenWindow, DistinctNonEmptyPhases, NominalReference
  case Alternatives(values: Vector[String])
  case DomainValue(constructor: String)

final class ParameterInfo private (
    val id: String,
    val version: Int,
    val meaning: String,
    val units: ParameterUnits,
    val allowed: ParameterDomain
):
  private[plan] def prefixed(prefix: String): ParameterInfo =
    new ParameterInfo(prefix + id, version, meaning, units, allowed)
object ParameterInfo:
  def of(
      id: String,
      version: Int,
      meaning: String,
      units: ParameterUnits,
      allowed: ParameterDomain
  ): Either[DescriptorError, ParameterInfo] =
    if id.trim.isEmpty || version <= 0 || meaning.trim.isEmpty then
      Left(DescriptorError.InvalidField(id, version, meaning))
    else
      allowed match
        case ParameterDomain.Alternatives(xs)
            if xs.isEmpty || xs.exists(_.trim.isEmpty) || xs.distinct.size != xs.size =>
          Left(DescriptorError.InvalidAlternatives(id, xs))
        case _ => Right(new ParameterInfo(id, version, meaning, units, allowed))
  private[plan] def literal(
      id: String,
      meaning: String,
      units: ParameterUnits,
      allowed: ParameterDomain
  ): ParameterInfo = new ParameterInfo(id, 1, meaning, units, allowed)

final class NamedParameterDefault[A] private (
    val name: String,
    val value: A,
    val reason: String
)
object NamedParameterDefault:
  def of[A](
      name: String,
      value: A,
      reason: String
  ): Either[DescriptorError, NamedParameterDefault[A]] =
    Either.cond(
      name.trim.nonEmpty && reason.trim.nonEmpty,
      new NamedParameterDefault(name, value, reason),
      DescriptorError.InvalidDefault(name, reason)
    )

final class ParameterFailure[R, E] private[plan] (
    val field: ParameterInfo,
    val input: R,
    val underlying: E,
    val message: String
)

/** Raw input, constructed value and domain error all remain statically typed. */
final class ParameterDescriptor[R, A, E](
    val info: ParameterInfo,
    val construct: R => Either[E, A],
    val errorMessage: E => String,
    val default: Option[NamedParameterDefault[A]] = None
):
  def parse(input: R): Either[ParameterFailure[R, E], A] =
    construct(input).left.map(e =>
      new ParameterFailure(info, input, e, s"${info.id}: ${errorMessage(e)}")
    )
  def bind[P](get: P => A)(encode: A => Provenance.Param): ParameterField[P] =
    val outer = this
    new ParameterField[P]:
      type Raw   = R
      type Value = A
      type Error = E
      val descriptor: ParameterDescriptor[R, A, E] = outer
      def value(parameters: P): A                  = get(parameters)
      def encoded(parameters: P): Provenance.Param = encode(get(parameters))

/** An existential field retains its own types; it is never an Any-valued bag. */
trait ParameterField[P]:
  type Raw
  type Value
  type Error
  val descriptor: ParameterDescriptor[Raw, Value, Error]
  def value(parameters: P): Value
  def encoded(parameters: P): Provenance.Param

final class ParameterSet[P] private (val fields: Vector[ParameterField[P]]):
  def values(parameters: P): Vector[(String, Provenance.Param)] =
    fields.map(f => f.descriptor.info.id -> f.encoded(parameters))
  def verify(
      parameters: P,
      declared: Vector[(String, Provenance.Param)]
  ): Either[DescriptorError, Unit] =
    val described = values(parameters)
    Either.cond(
      declared.map(_._1).distinct.size == declared.size &&
        described.toMap == declared.toMap,
      (),
      DescriptorError.ParameterMismatch(described, declared)
    )

object ParameterSet:
  def of[P](fields: Vector[ParameterField[P]]): Either[DescriptorError, ParameterSet[P]] =
    val ids = fields.map(_.descriptor.info.id)
    Either.cond(
      ids.distinct.size == ids.size,
      new ParameterSet(fields),
      DescriptorError.DuplicateFields(ids)
    )
  val empty: ParameterSet[Unit] = new ParameterSet(Vector.empty)
  private[plan] def literal[P](fields: Vector[ParameterField[P]]): ParameterSet[P] =
    new ParameterSet(fields)

enum ScoreDirection derives CanEqual:
  case HigherIsCloser, LowerIsCloser, NoOrder
enum ComparisonProperty derives CanEqual:
  case Symmetric, NonNegative, Bounded
enum ExecutionCapability derives CanEqual:
  /** Whole synchronous operations; no intra-operation cancellation guarantee. */
  case SynchronousWholeOperation

  /** Comparisons resume in declared quanta through a `BoundedCompare` cursor.
    * Estimation stays a whole operation per trial; pairing is paged.
    */
  case BoundedComparison

final class ScoreComponent[S, D] private (
    val id: String,
    val meaning: String,
    val units: ParameterUnits,
    val range: MeasureScale,
    val direction: ScoreDirection,
    val score: S => Double,
    val difference: D => Double
)
object ScoreComponent:
  def of[S, D](
      id: String,
      meaning: String,
      units: ParameterUnits,
      range: MeasureScale,
      direction: ScoreDirection
  )(
      score: S => Double,
      difference: D => Double
  ): Either[DescriptorError, ScoreComponent[S, D]] =
    val validRange = range match
      case MeasureScale.Bounded(lo, hi) => lo.isFinite && hi.isFinite && lo <= hi
      case _                            => true
    Either.cond(
      id.trim.nonEmpty && meaning.trim.nonEmpty && validRange,
      new ScoreComponent(id, meaning, units, range, direction, score, difference),
      DescriptorError.InvalidComponent(id, meaning, range)
    )

  /** A similarity component whose difference is a signed difference, for a
    * registered method whose range comes from its measure's declared scale.
    */
  private[plan] def literal(
      id: String,
      meaning: String,
      units: ParameterUnits,
      range: MeasureScale,
      direction: ScoreDirection
  ): ScoreComponent[Similarity, SignedDifference] =
    new ScoreComponent(id, meaning, units, range, direction, _.value, _.value)

final class MethodDescriptor[P, S, D] private (
    val id: DefinitionId,
    val parameters: ParameterSet[P],
    val info: P => MeasureInfo,
    val components: P => Either[DescriptorError, Vector[ScoreComponent[S, D]]],
    val properties: Set[ComparisonProperty],
    val execution: ExecutionCapability
):
  def verify(
      p: P,
      declared: Vector[(String, Provenance.Param)],
      componentIds: Vector[String]
  ): Either[DescriptorError, Unit] =
    for
      _        <- parameters.verify(p, declared)
      supplied <- components(p)
      ids = supplied.map(_.id)
      _ <- Either.cond(
        ids.nonEmpty && ids.distinct.size == ids.size && ids == componentIds,
        (),
        DescriptorError.ComponentMismatch(ids, componentIds)
      )
    yield ()

object MethodDescriptor:
  /** `execution` is a claim about the method; `StudyPlan.inspect` rejects a
    * descriptor whose claim disagrees with the method's typed evidence.
    */
  def of[P, S, D](
      id: DefinitionId,
      parameters: ParameterSet[P],
      info: P => MeasureInfo,
      components: P => Either[DescriptorError, Vector[ScoreComponent[S, D]]],
      properties: Set[ComparisonProperty] = Set.empty,
      execution: ExecutionCapability = ExecutionCapability.SynchronousWholeOperation
  ): MethodDescriptor[P, S, D] =
    new MethodDescriptor(id, parameters, info, components, properties, execution)

final class RecordingMethodDescriptor[P](
    val id: DefinitionId,
    val parameters: ParameterSet[P],
    val card: AlgorithmCard
):
  val execution: ExecutionCapability = ExecutionCapability.SynchronousWholeOperation

object RecordingMethodDescriptor:
  def idt(id: DefinitionId): RecordingMethodDescriptor[IdtParameters] =
    new RecordingMethodDescriptor(
      id,
      ParameterSet.literal(
        Vector(
          RecipeParameters.idtWidth
            .bind[IdtParameters](_.extent.width)(Provenance.Param.Num.apply),
          RecipeParameters.idtHeight
            .bind[IdtParameters](_.extent.height)(Provenance.Param.Num.apply),
          RecipeParameters.minimumDuration.bind[IdtParameters](_.minimumDuration)(p =>
            Provenance.Param.Text(p.span.toMicros.toString)
          )
        )
      ),
      AlgorithmCards.idt
    )

  def engbertKliegl(id: DefinitionId): RecordingMethodDescriptor[EkParameters] =
    new RecordingMethodDescriptor(
      id,
      ParameterSet.literal(
        Vector(
          RecipeParameters.ekEtaX
            .bind[EkParameters](_.thresholds.etaX)(Provenance.Param.Num.apply),
          RecipeParameters.ekEtaY
            .bind[EkParameters](_.thresholds.etaY)(Provenance.Param.Num.apply),
          RecipeParameters.ekMinimumSamples.bind[EkParameters](_.minimumSamples)(p =>
            Provenance.Param.Num(p.value.toDouble)
          )
        )
      ),
      AlgorithmCards.engbertKliegl
    )

  def ivt(id: DefinitionId): RecordingMethodDescriptor[IvtParameters] =
    new RecordingMethodDescriptor(
      id,
      ParameterSet.literal(
        Vector(
          RecipeParameters.ivtThreshold.bind[IvtParameters](_.threshold)(p =>
            Provenance.Param.Num(p.velocity.value)
          ),
          RecipeParameters.minimumDuration.bind[IvtParameters](_.minimumDuration)(p =>
            Provenance.Param.Text(p.span.toMicros.toString)
          )
        )
      ),
      AlgorithmCards.ivt
    )

enum DescriptorError derives CanEqual:
  case InvalidField(id: String, version: Int, meaning: String)
  case InvalidAlternatives(id: String, values: Vector[String])
  case InvalidDefault(name: String, reason: String)
  case DuplicateFields(ids: Vector[String])
  case InvalidComponent(id: String, meaning: String, range: MeasureScale)
  case ParameterMismatch(
      described: Vector[(String, Provenance.Param)],
      declared: Vector[(String, Provenance.Param)]
  )
  case ComponentMismatch(described: Vector[String], declared: Vector[String])
  case MissingMethod(id: DefinitionId)
  case MethodIdentity(expected: DefinitionId, found: DefinitionId)
  case ExecutionMismatch(declared: ExecutionCapability, actual: ExecutionCapability)
  case UnexplainedFields(fields: Vector[String])
  def message: String = this match
    case InvalidField(id, v, meaning) =>
      s"Invalid descriptor '$id' version $v with meaning '$meaning'."
    case InvalidAlternatives(id, xs) =>
      s"Descriptor '$id' has empty or repeated alternatives: $xs."
    case InvalidDefault(n, r) =>
      s"Default '$n' needs a name and scientific justification, got '$r'."
    case DuplicateFields(ids)           => s"Descriptor field IDs must be unique: $ids."
    case InvalidComponent(id, m, range) => s"Invalid score component '$id', '$m', range $range."
    case ParameterMismatch(a, b)        =>
      s"Described parameters $a disagree with scientific parameters $b."
    case ComponentMismatch(a, b) =>
      s"Described score components $a disagree with contrast components $b."
    case MissingMethod(id) => s"Method ${id.name}@${id.version} has no registered descriptor."
    case MethodIdentity(a, b) => s"Descriptor identity $b disagrees with method $a."
    case ExecutionMismatch(declared, actual) =>
      s"Descriptor declares execution $declared but the method executes as $actual."
    case UnexplainedFields(xs) =>
      s"Scientific description contains fields without metadata: $xs."
