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

import eyes4s.kernel.*

/** Translations between the deprecated [[ParameterUnits]]/[[ParameterDomain]]
  * vocabulary and [[Quantity]]/[[FieldKind]]. The new vocabulary is primary;
  * the legacy values are projections of it, kept for the deprecated
  * accessors and constructors only.
  */
@deprecated("Legacy descriptor projections; use Quantity and FieldKind", "0.1.0")
private[plan] object LegacyDescriptors:
  def units(q: Quantity): ParameterUnits = q match
    case Quantity.Planar(u)                         => ParameterUnits.Spatial(u.symbol)
    case Quantity.Rate(u)                           => ParameterUnits.PerSecond(u.symbol)
    case Quantity.Duration                          => ParameterUnits.Microseconds
    case Quantity.Length(LengthUnit.Millimetres)    => ParameterUnits.Millimetres
    case Quantity.Count(Counted.Cells)              => ParameterUnits.Cells
    case Quantity.Count(_) | Quantity.Dimensionless => ParameterUnits.Dimensionless
    case Quantity.Nominal                           => ParameterUnits.NominalIdentity
    case Quantity.Length(_) | Quantity.UnitsPerDegree(_) | Quantity.Composite =>
      ParameterUnits.Mixed

  def domain(kind: FieldKind): ParameterDomain = kind match
    case FieldKind.Numeric(q, shape, bounds)           => numeric(q, shape, bounds)
    case FieldKind.Choice(ChoiceSource.Fixed(options)) =>
      ParameterDomain.Alternatives(options.map(_.token))
    case FieldKind.Choice(ChoiceSource.FromInput(_)) => ParameterDomain.NominalReference
    case FieldKind.Toggle => ParameterDomain.Alternatives(Vector("false", "true"))
    case FieldKind.Text | FieldKind.Reference => ParameterDomain.NominalReference
    case FieldKind.Optional(of, _)            => domain(of.kind)
    case FieldKind.Repeated(of, _, _)         => domain(of.kind)
    case FieldKind.Variant(cases) => ParameterDomain.Alternatives(cases.map(_.token))
    case FieldKind.Group(parts, GroupRule.Distinct(names)) =>
      val phases = names.forall(n =>
        parts.exists(p =>
          p.id == n && p.kind == FieldKind.Choice(ChoiceSource.FromInput(InputDomain.Phases))
        )
      )
      if phases then ParameterDomain.DistinctNonEmptyPhases
      else ParameterDomain.NominalReference
    case FieldKind.Group(parts, GroupRule.Ordered(pairs)) =>
      if pairs.size == 1 && numericParts(parts).forall(_ == Quantity.Duration) then
        ParameterDomain.PositiveHalfOpenWindow
      else ParameterDomain.OrderedFiniteBounds
    case FieldKind.Group(parts, GroupRule.Independent) =>
      numericParts(parts).distinct match
        case Vector(Quantity.Count(Counted.Cells)) => ParameterDomain.PositiveGridDimensions
        case Vector(Quantity.Length(_))            => ParameterDomain.PositivePhysicalDimensions
        case Vector(Quantity.Duration)             => ParameterDomain.SignedMicroseconds
        case _                                     =>
          ParameterDomain.DomainValue(parts.map(_.id.value).mkString("parts: ", ", ", ""))

  private def numericParts(parts: Vector[FieldView]): Vector[Quantity] =
    parts.map(_.kind).collect { case FieldKind.Numeric(q, _, _) => q }

  private def numeric(q: Quantity, shape: NumberShape, b: NumericBounds): ParameterDomain =
    val integral = shape != NumberShape.Real
    (b.lower, b.upper, q) match
      case (Some(Endpoint.Open(0)), None, Quantity.Duration) =>
        ParameterDomain.PositiveMicroseconds
      case (Some(Endpoint.Closed(0)), None, Quantity.Duration) =>
        ParameterDomain.NonNegativeMicroseconds
      case (None, None, Quantity.Duration)                 => ParameterDomain.SignedMicroseconds
      case (Some(Endpoint.Open(0)), None, _)               => ParameterDomain.PositiveFinite
      case (Some(Endpoint.Closed(1)), None, _) if integral => ParameterDomain.PositiveFinite
      case (Some(Endpoint.Closed(0)), None, _)             => ParameterDomain.NonNegativeFinite
      case _ => ParameterDomain.DomainValue(s"bounds ${b.render}")

  def quantity(units: ParameterUnits): Either[DescriptorError, Quantity] = units match
    case ParameterUnits.Dimensionless   => Right(Quantity.Dimensionless)
    case ParameterUnits.Microseconds    => Right(Quantity.Duration)
    case ParameterUnits.Millimetres     => Right(Quantity.Length(LengthUnit.Millimetres))
    case ParameterUnits.Cells           => Right(Quantity.Count(Counted.Cells))
    case ParameterUnits.NominalIdentity => Right(Quantity.Nominal)
    case ParameterUnits.Mixed           => Right(Quantity.Composite)
    case ParameterUnits.Spatial(s)      => planar(s, units).map(Quantity.Planar(_))
    case ParameterUnits.PerSecond(s)    => planar(s, units).map(Quantity.Rate(_))

  private def planar(
      symbol: String,
      units: ParameterUnits
  ): Either[DescriptorError, PlanarUnit] =
    PlanarUnit.values
      .find(_.symbol == symbol)
      .toRight(DescriptorError.UntranslatableLegacy("", units.toString, ""))

  def kind(
      id: String,
      units: ParameterUnits,
      allowed: ParameterDomain
  ): Either[DescriptorError, FieldKind] =
    def refuse = DescriptorError.UntranslatableLegacy(id, units.toString, allowed.toString)
    def shapeOf(q: Quantity) = q match
      case Quantity.Duration => NumberShape.Int64
      case Quantity.Count(_) => NumberShape.Int32
      case _                 => NumberShape.Real
    def number(q: Quantity, bounds: NumericBounds) =
      Either.cond(q.isNumeric, FieldKind.Numeric(q, shapeOf(q), bounds), refuse)
    quantity(units).left.map(_ => refuse).flatMap { q =>
      allowed match
        case ParameterDomain.PositiveFinite       => number(q, NumericBounds.positive)
        case ParameterDomain.NonNegativeFinite    => number(q, NumericBounds.nonNegative)
        case ParameterDomain.PositiveMicroseconds =>
          number(Quantity.Duration, NumericBounds.positive)
        case ParameterDomain.NonNegativeMicroseconds =>
          number(Quantity.Duration, NumericBounds.nonNegative)
        case ParameterDomain.SignedMicroseconds =>
          number(Quantity.Duration, NumericBounds.unbounded)
        case ParameterDomain.Alternatives(xs) =>
          Right(FieldKind.Choice(ChoiceSource.Fixed(xs.map(x => ChoiceOption(x, x)))))
        case ParameterDomain.NominalReference => Right(FieldKind.Reference)
        case _                                => Left(refuse)
    }
