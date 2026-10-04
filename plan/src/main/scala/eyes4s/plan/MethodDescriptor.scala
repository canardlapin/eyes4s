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
import scala.annotation.nowarn

// The two enums below are deprecated. A Scala 3 enum's own cases extend their
// deprecated parent, so without @nowarn each enum definition warns against
// itself; these two annotations are the only ones CR6 adds.

/** Informational units, superseded by [[Quantity]]. */
@deprecated("Use Quantity (ParameterInfo.quantity / FieldView.quantity)", "0.1.0")
@nowarn("cat=deprecation")
enum ParameterUnits derives CanEqual:
  case Dimensionless, Microseconds, Millimetres, Cells, NominalIdentity, Mixed
  case Spatial(symbol: String)
  case PerSecond(symbol: String)

/** Informational constructor contracts, superseded by [[FieldKind]]. */
@deprecated("Use FieldKind (ParameterInfo.kind / FieldView.kind)", "0.1.0")
@nowarn("cat=deprecation")
enum ParameterDomain derives CanEqual:
  case PositiveFinite, NonNegativeFinite, PositiveMicroseconds, NonNegativeMicroseconds
  case SignedMicroseconds, PositiveGridDimensions, OrderedFiniteBounds,
    PositivePhysicalDimensions
  case PositiveHalfOpenWindow, DistinctNonEmptyPhases, NominalReference
  case Alternatives(values: Vector[String])
  case DomainValue(constructor: String)

/** A described field: its stable identity, version, scientific meaning and
  * the [[FieldView]] a host builds a control from.
  */
final class ParameterInfo private (val view: FieldView):
  def id: String         = view.id.value
  def version: Int       = view.version
  def meaning: String    = view.meaning
  def kind: FieldKind    = view.kind
  def quantity: Quantity = view.quantity

  /** The legacy projection of [[quantity]]. */
  @deprecated("Use quantity", "0.1.0")
  def units: ParameterUnits = LegacyDescriptors.units(view.quantity)

  /** The legacy projection of [[kind]]. */
  @deprecated("Use kind", "0.1.0")
  def allowed: ParameterDomain = LegacyDescriptors.domain(view.kind)

  private[plan] def prefixed(prefix: String): ParameterInfo =
    new ParameterInfo(view.prefixed(prefix))

  override def equals(that: Any): Boolean = that match
    case p: ParameterInfo => view == p.view
    case _                => false
  override def hashCode: Int    = view.hashCode
  override def toString: String = s"ParameterInfo($view)"

object ParameterInfo:
  def of(view: FieldView): ParameterInfo = new ParameterInfo(view)

  def of(
      id: String,
      version: Int,
      meaning: String,
      kind: FieldKind
  ): Either[DescriptorError, ParameterInfo] =
    FieldId.of(id).flatMap(FieldView.of(_, version, meaning, kind)).map(new ParameterInfo(_))

  /** The legacy constructor: the units and domain are translated into a
    * [[FieldKind]]; a domain with no translation is refused.
    */
  @deprecated("Use ParameterInfo.of(id, version, meaning, kind)", "0.1.0")
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
        case _ =>
          LegacyDescriptors.kind(id, units, allowed).flatMap(of(id, version, meaning, _))

  private[plan] def literal(id: String, meaning: String, kind: FieldKind): ParameterInfo =
    new ParameterInfo(FieldView.literal(id, meaning, kind))

  private[plan] def literal(view: FieldView): ParameterInfo = new ParameterInfo(view)

/** A default value that states why it is the default. Both `name` and `reason`
  * are non-blank: an unexplained default is refused as `DescriptorError.InvalidDefault`,
  * because a default is a scientific choice the methods section must be able to cite.
  */
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

/** A parameter's typed refusal: the field it concerns, the raw input, the domain
  * error and a message prefixed with the field id. Built only by
  * `ParameterDescriptor.parse`.
  */
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
    val default: Option[NamedParameterDefault[A]] = None,
    val form: Option[FormField[E, A]] = None
):
  /** A raw form value parsed on its own: through [[form]] when the field has
    * one, else through the shape and bounds stages of its view.
    */
  def validate(raw: RawValue): Either[FieldError[E], Unit] =
    form.fold(info.view.check(raw))(_.parse(raw).map(_ => ()))

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

object ParameterDescriptor:
  /** A one-number parameter whose raw input is the field's number: its
    * metadata is the field's view and its constructor the field's domain
    * constructor, so the two cannot disagree.
    */
  def numeric[E, N, A](field: NumericField[E, N, A]): ParameterDescriptor[N, A, E] =
    new ParameterDescriptor(
      ParameterInfo.of(field.view),
      field.domain,
      field.errorMessage,
      None,
      Some(field)
    )

/** An existential field retains its own types; it is never an Any-valued bag. */
trait ParameterField[P]:
  /** The raw input type the field parses. */
  type Raw

  /** The constructed, checked parameter type. */
  type Value

  /** The field's typed domain error. */
  type Error
  val descriptor: ParameterDescriptor[Raw, Value, Error]
  def value(parameters: P): Value
  def encoded(parameters: P): Provenance.Param

  /** The host view of this field: plain data, no type members. */
  final def view: FieldView = descriptor.info.view

  /** A raw form value checked on its own; see [[ParameterDescriptor.validate]]. */
  final def validate(raw: RawValue): Either[FieldError[Any], Unit] = descriptor.validate(raw)

  /** The raw form value of this field in `parameters`, when the field has a
    * form.
    */
  final def raw(parameters: P): Option[RawValue] =
    descriptor.form.map(_.raw(value(parameters)))

/** The ordered fields of a parameter type `P`. Field ids are unique and each
  * field's form presents the same view as its metadata (checked by `ParameterSet.of`,
  * refusing `DuplicateFields` or `FormViewMismatch`), so validation, inspection and
  * provenance describe one set of fields.
  */
final class ParameterSet[P] private (val fields: Vector[ParameterField[P]]):
  /** The host views of the fields, in order. */
  def views: Vector[FieldView] = fields.map(_.view)

  /** The raw form values of the fields that have a form, in `parameters`. */
  def formValues(parameters: P): FormValues =
    FormValues.from(fields.flatMap(f => f.raw(parameters).map(f.view.id -> _)).toMap)

  /** One raw value checked by the field it names, without the other fields. */
  def validate(id: FieldId, raw: RawValue): Either[FieldError[Any], Unit] =
    fields
      .find(_.view.id == id)
      .toRight(FieldError.UnknownField(id))
      .flatMap(_.validate(raw))

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
  /** Field ids are unique, and a field's form presents the same view as its
    * metadata, so validation and inspection describe one field.
    */
  def of[P](fields: Vector[ParameterField[P]]): Either[DescriptorError, ParameterSet[P]] =
    val ids = fields.map(_.descriptor.info.id)
    for
      _ <- Either.cond(ids.distinct.size == ids.size, (), DescriptorError.DuplicateFields(ids))
      _ <- fields
        .collectFirst {
          case f if f.descriptor.form.exists(_.view != f.view) =>
            DescriptorError.FormViewMismatch(f.view.id, f.descriptor.form.get.view.id)
        }
        .toLeft(())
    yield new ParameterSet(fields)
  val empty: ParameterSet[Unit] = new ParameterSet(Vector.empty)
  private[plan] def literal[P](fields: Vector[ParameterField[P]]): ParameterSet[P] =
    new ParameterSet(fields)

/** Which way a score component orders closeness: higher, lower, or not ordered at all. */
enum ScoreDirection derives CanEqual:
  case HigherIsCloser, LowerIsCloser, NoOrder

/** A property a comparison method declares about its scores: symmetric in its
  * operands, never negative, or bounded. A declaration, not checked against computed
  * scores.
  */
enum ComparisonProperty derives CanEqual:
  case Symmetric, NonNegative, Bounded

/** How a method executes, as the bounded-work scheduler may rely on it. */
enum ExecutionCapability derives CanEqual:
  /** Whole synchronous operations; no intra-operation cancellation guarantee. */
  case SynchronousWholeOperation

  /** Comparisons resume in declared quanta through a `BoundedCompare` cursor.
    * Estimation stays a whole operation per trial; pairing is paged.
    */
  case BoundedComparison

/** One named numeric component of a score `S` and of its difference `D`, with its
  * quantity, declared range and direction. `ScoreComponent.of` requires a non-blank
  * id and meaning, a numeric quantity and, for a bounded range, finite endpoints
  * with the lower not above the upper; otherwise `DescriptorError.InvalidComponent`.
  */
final class ScoreComponent[S, D] private (
    val id: String,
    val meaning: String,
    val quantity: Quantity,
    val range: MeasureScale,
    val direction: ScoreDirection,
    val score: S => Double,
    val difference: D => Double
):
  /** The legacy projection of [[quantity]]. */
  @deprecated("Use quantity", "0.1.0")
  def units: ParameterUnits = LegacyDescriptors.units(quantity)

object ScoreComponent:
  def of[S, D](
      id: String,
      meaning: String,
      quantity: Quantity,
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
      id.trim.nonEmpty && meaning.trim.nonEmpty && validRange && quantity.isNumeric,
      new ScoreComponent(id, meaning, quantity, range, direction, score, difference),
      DescriptorError.InvalidComponent(id, meaning, range)
    )

  @deprecated("Use ScoreComponent.of with a Quantity", "0.1.0")
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
    LegacyDescriptors
      .quantity(units)
      .flatMap(of(id, meaning, _, range, direction)(score, difference))

  /** A similarity component whose difference is a signed difference, for a
    * registered method whose range comes from its measure's declared scale.
    */
  private[plan] def literal(
      id: String,
      meaning: String,
      quantity: Quantity,
      range: MeasureScale,
      direction: ScoreDirection
  ): ScoreComponent[Similarity, SignedDifference] =
    new ScoreComponent(id, meaning, quantity, range, direction, _.value, _.value)

/** The inspectable description of a comparison method: its versioned identity,
  * parameter fields, measure information and score components for given
  * parameters, declared properties and execution capability. `verify` checks a
  * saved description's parameter values and component ids against the method's;
  * properties, execution capability and component ranges are not verified.
  */
final class MethodDescriptor[P, S, D] private (
    val id: DefinitionId,
    val parameters: ParameterSet[P],
    val info: P => MeasureInfo,
    val components: P => Either[DescriptorError, Vector[ScoreComponent[S, D]]],
    val properties: Set[ComparisonProperty],
    val execution: ExecutionCapability
):
  /** The method's parameters as a host sees them. */
  def formView: FormView = FormView(id, parameters.views)

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

/** The inspectable description of an event detector: its versioned identity, its
  * parameter fields and its `AlgorithmCard`. Detectors run as whole synchronous
  * operations, so `execution` is fixed.
  */
final class RecordingMethodDescriptor[P](
    val id: DefinitionId,
    val parameters: ParameterSet[P],
    val card: AlgorithmCard
):
  /** The detector's parameters as a host sees them. */
  def formView: FormView = FormView(id, parameters.views)

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

/** Why a descriptor, field, default, form or saved description was refused,
  * naming the field ids, values, bounds or identities that disagree.
  */
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
  case InvalidFieldId(value: String)
  case InvalidBounds(lower: Option[Endpoint], upper: Option[Endpoint])
  case BoundsForShape(field: FieldId, shape: NumberShape, bounds: NumericBounds)
  case UnknownRulePart(field: FieldId, part: FieldId)
  case InvalidRepetition(field: FieldId, minimum: Int, maximum: Option[Int])
  case DefaultRefused(field: FieldId, error: FieldError[Nothing])
  case UntranslatableLegacy(field: String, units: String, domain: String)
  case FormViewMismatch(field: FieldId, form: FieldId)
  case RulePartKind(field: FieldId, part: FieldId)
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
    case InvalidFieldId(v)     => s"Field id '$v' must be non-empty and contain no whitespace."
    case InvalidBounds(lo, hi) =>
      s"Bounds need finite endpoints with the lower below the upper, got $lo and $hi."
    case BoundsForShape(f, shape, bounds) =>
      s"Field '$f' has bounds ${bounds.render} that a $shape number cannot represent exactly."
    case UnknownRulePart(f, part) =>
      s"Field '$f' has a rule naming '$part', which is not one of its parts."
    case InvalidRepetition(f, min, max) =>
      s"Field '$f' repeats between $min and $max items; the minimum must be non-negative and not above the maximum."
    case DefaultRefused(f, e) =>
      s"The default of field '$f' does not pass its own checks: ${e.message}"
    case FormViewMismatch(f, form) =>
      s"Field '$f' carries a form field '$form' whose view differs from its own."
    case RulePartKind(f, part) =>
      s"Field '$f' has a rule over part '$part', which is not of a kind the rule compares."
    case UntranslatableLegacy(f, units, domain) =>
      s"Field '$f' uses legacy units $units and domain $domain, which have no FieldKind translation."
