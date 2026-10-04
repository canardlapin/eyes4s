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
import eyes4s.kernel.*

/** The stable identity of a descriptor field: `sigma`, `method.sigma`,
  * `estimate.0`. Non-empty and without whitespace.
  */
final case class FieldId private (value: String) derives CanEqual:
  def prefixed(prefix: String): FieldId = new FieldId(prefix + value)
  override def toString: String         = value

object FieldId:
  def of(value: String): Either[DescriptorError, FieldId] =
    Either.cond(
      value.nonEmpty && !value.exists(_.isWhitespace),
      new FieldId(value),
      DescriptorError.InvalidFieldId(value)
    )
  private[plan] def literal(value: String): FieldId = new FieldId(value)

/** What a descriptor value counts. */
enum Counted derives CanEqual:
  case Cells, Samples, Scores, Occurrences

/** What a field's number measures, and in which unit.
  *
  * A numeric field names exactly one quantity; a field whose parts differ is
  * `Composite`, and its parts name their own. Units come from the kernel's
  * [[PlanarUnit]] and [[LengthUnit]], never from a string.
  */
enum Quantity derives CanEqual:
  /** A position, extent or standard deviation in a frame of `unit`. */
  case Planar(unit: PlanarUnit)

  /** A speed: `unit` per second. */
  case Rate(unit: PlanarUnit)

  /** A declared linear scale: `unit` per degree of visual angle. */
  case UnitsPerDegree(unit: PlanarUnit)

  /** A physical length, such as a viewing distance. */
  case Length(unit: LengthUnit)

  /** A time interval in integer microseconds ([[Span]]). */
  case Duration

  /** A whole number of things. */
  case Count(of: Counted)

  /** A pure number: a ratio, a score or a choice. */
  case Dimensionless

  /** A name or identity; not a number. */
  case Nominal

  /** Parts with different quantities; each part names its own. */
  case Composite

  /** Whether a number of this quantity can be entered. */
  def isNumeric: Boolean = this match
    case Nominal | Composite => false
    case _                   => true

  /** The unit's symbol for display: `deg`, `deg/s`, `px/deg`, `mm`, `µs`. */
  def symbol: String = this match
    case Planar(u)                           => u.symbol
    case Rate(u)                             => s"${u.symbol}/s"
    case UnitsPerDegree(u)                   => s"${u.symbol}/deg"
    case Length(u)                           => u.symbol
    case Duration                            => "µs"
    case Count(of)                           => of.toString.toLowerCase
    case Dimensionless | Nominal | Composite => ""

/** How a number is written: a finite real, or a 32- or 64-bit integer. */
enum NumberShape derives CanEqual:
  case Real, Int32, Int64

/** Reading and writing the numbers of one [[NumberShape]].
  *
  * Text is read by one grammar on every platform: an optional sign, digits
  * with an optional fraction, and for reals an optional exponent. Surrounding
  * whitespace is ignored. `NaN`, infinities, hexadecimal and grouping are not
  * numbers, and a real that overflows to infinity or a non-zero one that
  * underflows to zero is refused. [[write]] gives the canonical text, the same
  * on the JVM and in Scala.js, and reading it back gives the same number.
  */
sealed trait Numeral[N]:
  def shape: NumberShape
  def read(text: String): Option[N]
  def write(n: N): String
  def toDouble(n: N): Double

  /** The representable number next to `n` towards `up` or down, if any. */
  def step(n: N, up: Boolean): Option[N]

  /** `x` as this shape's number, when `x` is exactly one. */
  def exactly(x: Double): Option[N]

  /** The largest (or smallest) finite representable number. */
  def extreme(up: Boolean): N

  /** The representable number nearest a finite `x`, clamped to the range. */
  def nearest(x: Double): N

  /** The sign of `n - v`, compared exactly in this shape's own type; `v` is a
    * declared endpoint.
    */
  def compare(n: N, v: Double): Int

object Numeral:
  private val integralText = "[+-]?[0-9]+".r
  private val realText     = "[+-]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?".r

  given real: Numeral[Double] with
    val shape              = NumberShape.Real
    def read(text: String) =
      val t = text.trim
      // A non-zero mantissa that underflows to zero is refused, as overflow is.
      val nonZero = t.takeWhile(c => c != 'e' && c != 'E').exists(c => c >= '1' && c <= '9')
      if realText.matches(t) then
        Some(t.toDouble).filter(d => d.isFinite && (d != 0.0 || !nonZero))
      else None
    def write(n: Double)             = NumberText.real(n)
    def toDouble(n: Double)          = n
    def step(n: Double, up: Boolean) =
      val next = if up then Math.nextUp(n) else Math.nextDown(n)
      Option.when(next.isFinite)(next)
    def exactly(x: Double)   = Option.when(x.isFinite)(x)
    def extreme(up: Boolean) = if up then Double.MaxValue else -Double.MaxValue
    def nearest(x: Double)   = x
    // `<` and `>` rather than Double.compare, so -0 meets a bound at 0 as 0 does.
    def compare(n: Double, v: Double) = if n < v then -1 else if n > v then 1 else 0

  given int32: Numeral[Int] with
    val shape              = NumberShape.Int32
    def read(text: String) =
      val t = text.trim
      if integralText.matches(t) then t.toIntOption else None
    def write(n: Int)             = n.toString
    def toDouble(n: Int)          = n.toDouble
    def step(n: Int, up: Boolean) =
      if up then Option.when(n < Int.MaxValue)(n + 1) else Option.when(n > Int.MinValue)(n - 1)
    def exactly(x: Double) =
      Option
        .when(x.isWhole && x >= Int.MinValue.toDouble && x <= Int.MaxValue.toDouble)(x.toInt)
    def extreme(up: Boolean) = if up then Int.MaxValue else Int.MinValue
    def nearest(x: Double)   =
      math.rint(x).max(Int.MinValue.toDouble).min(Int.MaxValue.toDouble).toInt
    def compare(n: Int, v: Double) = java.lang.Double.compare(n.toDouble, v)

  given int64: Numeral[Long] with
    val shape              = NumberShape.Int64
    def read(text: String) =
      val t = text.trim
      if integralText.matches(t) then t.toLongOption else None
    def write(n: Long)             = n.toString
    def toDouble(n: Long)          = n.toDouble
    def step(n: Long, up: Boolean) =
      if up then Option.when(n < Long.MaxValue)(n + 1)
      else Option.when(n > Long.MinValue)(n - 1)
    def exactly(x: Double) =
      Option.when(x.isWhole && math.abs(x) <= Numeral.exactIntegers)(x.toLong)
    def extreme(up: Boolean)        = if up then Long.MaxValue else Long.MinValue
    def nearest(x: Double)          = math.rint(x).toLong
    def compare(n: Long, v: Double) = exactly(v) match
      case Some(e) => java.lang.Long.compare(n, e)
      case None    => java.lang.Double.compare(n.toDouble, v)

  /** Integers up to this magnitude are exact as doubles. */
  val exactIntegers: Double = 9007199254740992.0

  def forShape(shape: NumberShape): Numeral[?] = shape match
    case NumberShape.Real  => real
    case NumberShape.Int32 => int32
    case NumberShape.Int64 => int64

/** Canonical number text, identical on the JVM and in Scala.js. */
private[plan] object NumberText:
  /** The shortest decimal that reads back as `x`: `2`, `0.5`, `1E-7`.
    *
    * The search rounds the double's exact binary value, built from its bits
    * with integer arithmetic (the same on every platform), to the fewest
    * significant digits that read back as `x`.
    */
  def real(x: Double): String =
    val sign = if x < 0 || (x == 0.0 && 1.0 / x < 0) then "-" else ""
    val m    = math.abs(x)
    val body =
      if m == 0.0 then "0"
      else if m.isWhole && m < 1e15 then m.toLong.toString
      else
        val decimal = exact(m)
        (1 to 17).iterator
          .map(p =>
            decimal
              .round(new java.math.MathContext(p, java.math.RoundingMode.HALF_EVEN))
              .stripTrailingZeros
              .toString
          )
          .find(_.toDouble == m)
          .getOrElse(decimal.stripTrailingZeros.toString)
    sign + body

  /** The exact value of a finite positive double. */
  private def exact(m: Double): java.math.BigDecimal =
    val bits                 = java.lang.Double.doubleToLongBits(m)
    val biased               = ((bits >>> 52) & 0x7ffL).toInt
    val fraction             = bits & 0xfffffffffffffL
    val (mantissa, exponent) =
      if biased == 0 then (fraction, -1074) else (fraction | (1L << 52), biased - 1075)
    val digits = java.math.BigInteger.valueOf(mantissa)
    if exponent >= 0 then new java.math.BigDecimal(digits.shiftLeft(exponent))
    else
      // m = mantissa / 2^k = mantissa * 5^k / 10^k
      new java.math.BigDecimal(digits.multiply(java.math.BigInteger.valueOf(5).pow(-exponent)))
        .scaleByPowerOfTen(exponent)

/** Which side of a range an endpoint bounds. */
enum Side derives CanEqual:
  case Lower, Upper

/** One end of a numeric range: `Open` excludes the value, `Closed` includes it. */
enum Endpoint derives CanEqual:
  case Open(value: Double)
  case Closed(value: Double)

  def value: Double

  def isClosed: Boolean = this match
    case Closed(_) => true
    case Open(_)   => false

  /** `x` satisfies this endpoint on `side`. */
  def admits(x: Double, side: Side): Boolean =
    admitsBy(v => if x < v then -1 else if x > v then 1 else 0, side)

  /** Whether a number satisfies this endpoint on `side`, given the sign of
    * its difference from an endpoint value.
    */
  def admitsBy(compare: Double => Int, side: Side): Boolean = (this, side) match
    case (Closed(v), Side.Lower) => compare(v) >= 0
    case (Open(v), Side.Lower)   => compare(v) > 0
    case (Closed(v), Side.Upper) => compare(v) <= 0
    case (Open(v), Side.Upper)   => compare(v) < 0

  def render(side: Side): String = (this, side) match
    case (Closed(v), Side.Lower) => s"[${NumberText.real(v)}"
    case (Open(v), Side.Lower)   => s"(${NumberText.real(v)}"
    case (Closed(v), Side.Upper) => s"${NumberText.real(v)}]"
    case (Open(v), Side.Upper)   => s"${NumberText.real(v)})"

/** The declared range of a numeric field, in the field's own quantity. A
  * missing endpoint leaves that side bounded only by what the number's shape
  * can represent.
  */
final case class NumericBounds private (lower: Option[Endpoint], upper: Option[Endpoint])
    derives CanEqual:

  /** The endpoint `x` violates, if any, lower first. */
  def violation(x: Double): Option[(Side, Endpoint)] =
    violationBy(v => if x < v then -1 else if x > v then 1 else 0)

  /** The endpoint a number violates, given the sign of its difference from
    * an endpoint value; lower first.
    */
  def violationBy(compare: Double => Int): Option[(Side, Endpoint)] =
    lower
      .filterNot(_.admitsBy(compare, Side.Lower))
      .map(Side.Lower -> _)
      .orElse(upper.filterNot(_.admitsBy(compare, Side.Upper)).map(Side.Upper -> _))

  def contains(x: Double): Boolean = violation(x).isEmpty

  def endpoints: Vector[(Side, Endpoint)] =
    lower.map(Side.Lower -> _).toVector ++ upper.map(Side.Upper -> _).toVector

  def render: String =
    s"${lower.fold("(-∞")(_.render(Side.Lower))}, ${upper.fold("∞)")(_.render(Side.Upper))}"

object NumericBounds:
  val unbounded: NumericBounds = new NumericBounds(None, None)

  /** Strictly positive: `(0, ∞)`. */
  val positive: NumericBounds = new NumericBounds(Some(Endpoint.Open(0)), None)

  /** Zero or more: `[0, ∞)`. */
  val nonNegative: NumericBounds = new NumericBounds(Some(Endpoint.Closed(0)), None)

  /** One or more: `[1, ∞)`. */
  val atLeastOne: NumericBounds = new NumericBounds(Some(Endpoint.Closed(1)), None)

  /** At least `v`: `[v, ∞)`. */
  def atLeast(v: Double): Either[DescriptorError, NumericBounds] =
    of(Some(Endpoint.Closed(v)), None)

  /** Endpoints are finite and the lower lies strictly below the upper. */
  def of(
      lower: Option[Endpoint],
      upper: Option[Endpoint]
  ): Either[DescriptorError, NumericBounds] =
    val finite  = (lower.toVector ++ upper.toVector).forall(_.value.isFinite)
    val ordered = (lower, upper) match
      case (Some(l), Some(u)) => l.value < u.value
      case _                  => true
    Either.cond(
      finite && ordered,
      new NumericBounds(lower, upper),
      DescriptorError.InvalidBounds(lower, upper)
    )

/** What a form field holds before parsing.
  *
  * A number is kept as the text the user typed, so an empty or malformed
  * entry is representable and the value survives a round trip exactly.
  */
enum RawValue derives CanEqual:
  case Number(text: String)
  case Choice(token: String)
  case Flag(value: Boolean)
  case Text(value: String)
  case Absent
  case Group(fields: Vector[(FieldId, RawValue)])
  case Variant(token: String, fields: Vector[(FieldId, RawValue)])
  case Items(values: Vector[RawValue])

/** One alternative of a choice: its stable token and a display label. The
  * shipped labels are the tokens; a host localises by token.
  */
final case class ChoiceOption(token: String, label: String) derives CanEqual

/** Values a choice draws from the study input rather than from a fixed list. */
enum InputDomain derives CanEqual:
  case Phases

/** Where a choice's alternatives come from. */
enum ChoiceSource derives CanEqual:
  case Fixed(options: Vector[ChoiceOption])
  case FromInput(domain: InputDomain)

/** A rule over the parts of a group, checked with the group itself. */
enum GroupRule derives CanEqual:
  /** Each part is checked on its own. */
  case Independent

  /** Each `(lower, upper)` pair of numeric parts is strictly increasing. */
  case Ordered(pairs: Vector[(FieldId, FieldId)])

  /** The named parts hold different values. */
  case Distinct(parts: Vector[FieldId])

/** One case of a variant field: its token and the parts that case carries. */
final case class VariantCase(token: String, label: String, parts: Vector[FieldView])
    derives CanEqual

/** The shape of a field's value, as plain data a host renders a control from. */
enum FieldKind derives CanEqual:
  /** One number of `quantity`, written in `shape`, within `bounds`. */
  case Numeric(quantity: Quantity, shape: NumberShape, bounds: NumericBounds)

  /** One token from `options`. */
  case Choice(options: ChoiceSource)

  /** A yes/no flag. */
  case Toggle

  /** Free non-empty text, such as a name. */
  case Text

  /** An identity bound elsewhere (an input, a layout, a method); not entered. */
  case Reference

  /** Named parts entered together, with a rule across them. */
  case Group(parts: Vector[FieldView], rule: GroupRule)

  /** A value that may be left out; `absentMeans` says what leaving it out does. */
  case Optional(of: FieldView, absentMeans: String)

  /** A list of `of`, with at least `minimum` items and at most `maximum`. */
  case Repeated(of: FieldView, minimum: Int, maximum: Option[Int])

  /** One of several cases, each with its own parts. */
  case Variant(cases: Vector[VariantCase])

  /** The quantity the value measures; `Composite` when the parts differ. */
  def measures: Quantity = this match
    case Numeric(q, _, _)                       => q
    case Choice(ChoiceSource.Fixed(_)) | Toggle => Quantity.Dimensionless
    case Choice(ChoiceSource.FromInput(_))      => Quantity.Nominal
    case Text | Reference                       => Quantity.Nominal
    case Optional(of, _)                        => of.kind.measures
    case Repeated(of, _, _)                     => of.kind.measures
    case Group(parts, _)                        => FieldKind.common(parts)
    case Variant(cases)                         => FieldKind.common(cases.flatMap(_.parts))

object FieldKind:
  /** The one measured quantity of the parts; `Nominal` when every part is a
    * name; otherwise `Composite`.
    */
  private[plan] def common(parts: Vector[FieldView]): Quantity =
    val all = parts.map(_.kind.measures).distinct
    all.filter(q => q.isNumeric && q != Quantity.Dimensionless) match
      case Vector(q)                                   => q
      case Vector() if all == Vector(Quantity.Nominal) => Quantity.Nominal
      case _                                           => Quantity.Composite

/** A default a field starts from, with the reason it is the default. */
final case class DefaultValue private (raw: RawValue, reason: String) derives CanEqual

object DefaultValue:
  def of(raw: RawValue, reason: String): Either[DescriptorError, DefaultValue] =
    Either.cond(
      reason.trim.nonEmpty,
      new DefaultValue(raw, reason),
      DescriptorError.InvalidDefault(raw.toString, reason)
    )
  private[plan] def literal(raw: RawValue, reason: String): DefaultValue =
    new DefaultValue(raw, reason)

/** A descriptor field as a host sees it: plain data, no type members and no
  * functions, so a host (or a codec) can render and store it.
  *
  * [[check]] runs the first two parse stages on a raw value, the shape and
  * the declared bounds and rules, without the domain constructor. A typed
  * [[FormField]] adds the third.
  */
final class FieldView private (
    val id: FieldId,
    val version: Int,
    val meaning: String,
    val kind: FieldKind,
    val default: Option[DefaultValue]
) derives CanEqual:
  def quantity: Quantity = kind.measures

  def check(raw: RawValue): Either[FieldError[Nothing], Unit] = FieldChecks.check(this, raw)

  def prefixed(prefix: String): FieldView =
    new FieldView(id.prefixed(prefix), version, meaning, kind, default)

  private[plan] def renamed(to: FieldId): FieldView =
    new FieldView(to, version, meaning, kind, default)

  def withDefault(value: DefaultValue): Either[DescriptorError, FieldView] =
    FieldView.of(id, version, meaning, kind, Some(value))

  private def parts                       = (id, version, meaning, kind, default)
  override def equals(that: Any): Boolean = that match
    case v: FieldView => parts == v.parts
    case _            => false
  override def hashCode: Int    = parts.hashCode
  override def toString: String = s"FieldView$parts"

object FieldView:
  /** A field whose parts have distinct ids, whose rules name its own parts,
    * whose integral bounds are exact integers, and whose default passes
    * [[FieldView.check]].
    */
  def of(
      id: FieldId,
      version: Int,
      meaning: String,
      kind: FieldKind,
      default: Option[DefaultValue] = None
  ): Either[DescriptorError, FieldView] =
    val view = new FieldView(id, version, meaning, kind, default)
    for
      _ <- Either.cond(
        version > 0 && meaning.trim.nonEmpty,
        (),
        DescriptorError.InvalidField(id.value, version, meaning)
      )
      _ <- FieldChecks.wellFormed(view)
      _ <- default.traverse_(d =>
        view.check(d.raw).left.map(e => DescriptorError.DefaultRefused(id, e))
      )
    yield view

  private[plan] def literal(
      id: String,
      meaning: String,
      kind: FieldKind,
      default: Option[DefaultValue] = None
  ): FieldView = new FieldView(FieldId.literal(id), 1, meaning, kind, default)

/** The fields a method or recipe presents, under its definition. */
final case class FormView(definition: DefinitionId, fields: Vector[FieldView]) derives CanEqual:
  def field(id: FieldId): Option[FieldView] = fields.find(_.id == id)

/** The shape a parse stage expected. */
enum Expected derives CanEqual:
  case Number(shape: NumberShape)
  case Choice, Flag, Text, Group, Variant, Items

/** Why a raw value does not parse, naming the field and the value.
  *
  * The first four cases come from the shape and bounds stages, which need no
  * domain constructor; `Refused` carries the domain constructor's own error.
  */
enum FieldError[+E] derives CanEqual:
  case Missing(field: FieldId)
  case Malformed(field: FieldId, raw: RawValue, expected: Expected)
  case OutOfBounds(
      field: FieldId,
      text: String,
      value: Double,
      side: Side,
      bound: Endpoint,
      quantity: Quantity
  )
  case NotAChoice(field: FieldId, token: String, options: Vector[String])
  case UnknownPart(field: FieldId, part: FieldId)
  case Unordered(
      field: FieldId,
      lower: FieldId,
      upper: FieldId,
      lowerValue: Double,
      upperValue: Double
  )
  case Duplicate(field: FieldId, part: FieldId, token: String)
  case ItemCount(field: FieldId, count: Int, minimum: Int, maximum: Option[Int])
  case Refused(field: FieldId, raw: RawValue, underlying: E, reason: String)

  /** A form names a field its descriptor does not have. */
  case UnknownField(field: FieldId)

  /** A raw group or case gives the same part twice. */
  case RepeatedPart(field: FieldId, part: FieldId)

  def field: FieldId

  def message: String = this match
    case Missing(f)                => s"$f: a value is required."
    case Malformed(f, raw, expect) =>
      s"$f: ${FieldChecks.show(raw)} is not a ${FieldChecks.show(expect)}."
    case OutOfBounds(f, text, _, side, bound, q) =>
      val unit = if q.symbol.isEmpty then "" else s" ${q.symbol}"
      val rule = (side, bound) match
        case (Side.Lower, Endpoint.Open(v))   => s"greater than ${NumberText.real(v)}$unit"
        case (Side.Lower, Endpoint.Closed(v)) => s"at least ${NumberText.real(v)}$unit"
        case (Side.Upper, Endpoint.Open(v))   => s"less than ${NumberText.real(v)}$unit"
        case (Side.Upper, Endpoint.Closed(v)) => s"at most ${NumberText.real(v)}$unit"
      s"$f: $text$unit is out of bounds; it must be $rule."
    case NotAChoice(f, token, options) =>
      s"$f: '$token' is not one of ${options.mkString(", ")}."
    case UnknownPart(f, part)         => s"$f: there is no part '$part'."
    case Unordered(f, lo, hi, lv, hv) =>
      s"$f: $lo (${NumberText.real(lv)}) must be less than $hi (${NumberText.real(hv)})."
    case Duplicate(f, part, token) => s"$f: '$token' in $part repeats another part."
    case ItemCount(f, n, min, max) =>
      s"$f: $n items; at least $min${max.fold("")(m => s" and at most $m")} are needed."
    case Refused(f, raw, _, reason) => s"$f: ${FieldChecks.show(raw)} is refused: $reason"
    case UnknownField(f)            => s"$f: there is no such field."
    case RepeatedPart(f, part)      => s"$f: part '$part' is given more than once."

/** A typed field: a raw value parses to `A` on its own, without a whole
  * recipe, in three stages: its shape, its declared bounds, then the domain
  * constructor, which stays the authority. `raw` gives the canonical raw
  * value of a typed one.
  */
trait FormField[+E, A]:
  def view: FieldView
  def parse(raw: RawValue): Either[FieldError[E], A]
  def raw(value: A): RawValue

  /** The raw value this field writes for what `raw` parses to. */
  final def restore(raw: RawValue): Either[FieldError[E], RawValue] = parse(raw).map(this.raw)

/** A numeric [[FormField]]: `domain` is the domain constructor alone, fed a
  * number of shape `N`; `number` reads a typed value back.
  */
final class NumericField[E, N, A] private (
    val view: FieldView,
    val numeral: Numeral[N],
    val quantity: Quantity,
    val bounds: NumericBounds,
    val domain: N => Either[E, A],
    val number: A => N,
    val errorMessage: E => String
) extends FormField[E, A]:
  def parse(raw: RawValue): Either[FieldError[E], A] =
    for
      n <- FieldChecks.readNumber(view.id, raw, numeral, quantity, bounds)
      a <- domain(n).left.map(e => FieldError.Refused(view.id, raw, e, errorMessage(e)))
    yield a

  def raw(value: A): RawValue = RawValue.Number(numeral.write(number(value)))

object NumericField:
  /** A numeric field of `quantity` within `bounds`. Integral shapes need
    * integral endpoints that their doubles represent exactly.
    */
  def of[E, N, A](
      id: FieldId,
      version: Int,
      meaning: String,
      quantity: Quantity,
      bounds: NumericBounds,
      default: Option[DefaultValue] = None
  )(domain: N => Either[E, A], number: A => N, errorMessage: E => String)(using
      numeral: Numeral[N]
  ): Either[DescriptorError, NumericField[E, N, A]] =
    FieldView
      .of(id, version, meaning, FieldKind.Numeric(quantity, numeral.shape, bounds), default)
      .map(new NumericField(_, numeral, quantity, bounds, domain, number, errorMessage))

  private[plan] def literal[E, N, A](
      id: String,
      meaning: String,
      quantity: Quantity,
      bounds: NumericBounds
  )(domain: N => Either[E, A], number: A => N, errorMessage: E => String)(using
      numeral: Numeral[N]
  ): NumericField[E, N, A] =
    new NumericField(
      FieldView.literal(id, meaning, FieldKind.Numeric(quantity, numeral.shape, bounds)),
      numeral,
      quantity,
      bounds,
      domain,
      number,
      errorMessage
    )

/** The shape and bounds stages over [[FieldView]] data. */
private[plan] object FieldChecks:
  def show(raw: RawValue): String = raw match
    case RawValue.Number(t)     => s"'$t'"
    case RawValue.Choice(t)     => s"choice '$t'"
    case RawValue.Flag(b)       => b.toString
    case RawValue.Text(t)       => s"text '$t'"
    case RawValue.Absent        => "nothing"
    case RawValue.Group(_)      => "a group"
    case RawValue.Variant(t, _) => s"case '$t'"
    case RawValue.Items(xs)     => s"${xs.size} items"

  def show(expected: Expected): String = expected match
    case Expected.Number(NumberShape.Real) => "finite number"
    case Expected.Number(_)                => "whole number"
    case Expected.Choice                   => "choice"
    case Expected.Flag                     => "yes/no flag"
    case Expected.Text                     => "non-empty text"
    case Expected.Group                    => "group"
    case Expected.Variant                  => "case"
    case Expected.Items                    => "list"

  def readNumber[N](
      id: FieldId,
      raw: RawValue,
      numeral: Numeral[N],
      quantity: Quantity,
      bounds: NumericBounds
  ): Either[FieldError[Nothing], N] = raw match
    case RawValue.Absent    => Left(FieldError.Missing(id))
    case RawValue.Number(t) =>
      numeral
        .read(t)
        .toRight(FieldError.Malformed(id, raw, Expected.Number(numeral.shape)))
        .flatMap { n =>
          val x = numeral.toDouble(n)
          bounds.violationBy(numeral.compare(n, _)) match
            case Some((side, bound)) =>
              Left(FieldError.OutOfBounds(id, t.trim, x, side, bound, quantity))
            case None => Right(n)
        }
    case other => Left(FieldError.Malformed(id, other, Expected.Number(numeral.shape)))

  def check(view: FieldView, raw: RawValue): Either[FieldError[Nothing], Unit] =
    val id = view.id
    (view.kind, raw) match
      case (_, RawValue.Absent) =>
        view.kind match
          case FieldKind.Optional(_, _) => Right(())
          case _                        => Left(FieldError.Missing(id))
      case (FieldKind.Numeric(q, shape, bounds), _) =>
        readNumber(id, raw, Numeral.forShape(shape), q, bounds).void
      case (FieldKind.Choice(ChoiceSource.Fixed(options)), RawValue.Choice(t)) =>
        Either.cond(
          options.exists(_.token == t),
          (),
          FieldError.NotAChoice(id, t, options.map(_.token))
        )
      case (FieldKind.Choice(ChoiceSource.FromInput(_)), RawValue.Choice(t)) =>
        Either.cond(t.trim.nonEmpty, (), FieldError.Malformed(id, raw, Expected.Choice))
      case (FieldKind.Toggle, RawValue.Flag(_))                     => Right(())
      case (FieldKind.Text | FieldKind.Reference, RawValue.Text(t)) =>
        Either.cond(t.trim.nonEmpty, (), FieldError.Malformed(id, raw, Expected.Text))
      case (FieldKind.Optional(of, _), _)                         => check(within(id, of), raw)
      case (FieldKind.Repeated(of, min, max), RawValue.Items(xs)) =>
        if xs.size < min || max.exists(xs.size > _) then
          Left(FieldError.ItemCount(id, xs.size, min, max))
        else
          xs.zipWithIndex.traverse_((x, i) =>
            check(of.renamed(FieldId.literal(s"${id.value}.$i")), x)
          )
      case (FieldKind.Group(parts, rule), RawValue.Group(fields)) =>
        checkParts(id, parts, fields, rule)
      case (FieldKind.Variant(cases), RawValue.Variant(token, fields)) =>
        cases.find(_.token == token) match
          case None    => Left(FieldError.NotAChoice(id, token, cases.map(_.token)))
          case Some(c) => checkParts(id, c.parts, fields, GroupRule.Independent)
      case (kind, _) => Left(FieldError.Malformed(id, raw, expected(kind)))

  private def expected(kind: FieldKind): Expected = kind match
    case FieldKind.Numeric(_, shape, _)       => Expected.Number(shape)
    case FieldKind.Choice(_)                  => Expected.Choice
    case FieldKind.Toggle                     => Expected.Flag
    case FieldKind.Text | FieldKind.Reference => Expected.Text
    case FieldKind.Group(_, _)                => Expected.Group
    case FieldKind.Optional(of, _)            => expected(of.kind)
    case FieldKind.Repeated(_, _, _)          => Expected.Items
    case FieldKind.Variant(_)                 => Expected.Variant

  /** A part as its containing field reports it: `window.xMin`. */
  private def within(id: FieldId, part: FieldView): FieldView =
    part.renamed(FieldId.literal(s"${id.value}.${part.id.value}"))

  private def checkParts(
      id: FieldId,
      parts: Vector[FieldView],
      fields: Vector[(FieldId, RawValue)],
      rule: GroupRule
  ): Either[FieldError[Nothing], Unit] =
    val given_ = fields.toMap
    for
      _ <- fields
        .map(_._1)
        .diff(fields.map(_._1).distinct)
        .headOption
        .toLeft(())
        .left
        .map(FieldError.RepeatedPart(id, _))
      _ <- fields
        .map(_._1)
        .find(p => !parts.exists(_.id == p))
        .toLeft(())
        .left
        .map(
          FieldError.UnknownPart(id, _)
        )
      _ <- parts.traverse_(p => check(within(id, p), given_.getOrElse(p.id, RawValue.Absent)))
      _ <- rule match
        case GroupRule.Independent    => Right(())
        case GroupRule.Ordered(pairs) =>
          pairs.traverse_ { (lo, hi) =>
            ordered(parts, given_, lo, hi) match
              case Some((false, l, h)) => Left(FieldError.Unordered(id, lo, hi, l, h))
              case _                   => Right(())
          }
        case GroupRule.Distinct(names) =>
          val tokens = names.flatMap(n => given_.get(n).flatMap(token).map(n -> _))
          tokens
            .groupBy(_._2)
            .collectFirst {
              case (t, ps) if ps.size > 1 => FieldError.Duplicate(id, ps(1)._1, t)
            }
            .toLeft(())
    yield ()

  /** Whether `lo` lies strictly below `hi`, compared in the parts' own
    * number shape (exactly, for 64-bit integers), with both as doubles.
    */
  private def ordered(
      parts: Vector[FieldView],
      given_ : Map[FieldId, RawValue],
      lo: FieldId,
      hi: FieldId
  ): Option[(Boolean, Double, Double)] =
    def shape(id: FieldId) = parts.find(_.id == id).map(_.kind).collect {
      case FieldKind.Numeric(_, s, _) => s
    }
    def text(id: FieldId) = given_.get(id).collect { case RawValue.Number(t) => t }
    def compare[N](numeral: Numeral[N]): Option[(Boolean, Double, Double)] =
      for
        l <- text(lo).flatMap(numeral.read)
        h <- text(hi).flatMap(numeral.read)
      yield
        val (ld, hd) = (numeral.toDouble(l), numeral.toDouble(h))
        numeral.shape match
          case NumberShape.Real  => (ld < hd, ld, hd)
          case NumberShape.Int32 => (ld < hd, ld, hd)
          case NumberShape.Int64 =>
            (
              java.lang.Long.compare(l.asInstanceOf[Long], h.asInstanceOf[Long]) < 0,
              ld,
              hd
            )
    (shape(lo), shape(hi)) match
      case (Some(a), Some(b)) if a == b => compare(Numeral.forShape(a))
      case _                            => compare(Numeral.real)

  private def token(raw: RawValue): Option[String] = raw match
    case RawValue.Choice(t) => Some(t)
    case RawValue.Text(t)   => Some(t)
    case _                  => None

  /** Distinct part ids, rules that name the group's own parts (`Ordered`
    * numeric ones, `Distinct` choice, text or reference ones), and integral
    * bounds that are exact integers.
    */
  def wellFormed(view: FieldView): Either[DescriptorError, Unit] =
    def parts(ps: Vector[FieldView]): Either[DescriptorError, Unit] =
      val ids = ps.map(_.id)
      Either
        .cond(
          ids.distinct.size == ids.size,
          (),
          DescriptorError.DuplicateFields(ids.map(_.value))
        )
        .flatMap(_ => ps.traverse_(wellFormed))
    view.kind match
      case FieldKind.Numeric(quantity, shape, bounds) =>
        val numeral = Numeral.forShape(shape)
        Either.cond(
          quantity.isNumeric,
          (),
          DescriptorError.NonNumericQuantity(view.id, quantity.toString.toLowerCase)
        ) *> Either.cond(
          bounds.endpoints.forall((_, e) => numeral.exactly(e.value).isDefined),
          (),
          DescriptorError.BoundsForShape(view.id, shape, bounds)
        )
      case FieldKind.Choice(ChoiceSource.Fixed(options)) =>
        val tokens = options.map(_.token)
        Either.cond(
          tokens.nonEmpty && tokens.forall(
            _.trim.nonEmpty
          ) && tokens.distinct.size == tokens.size,
          (),
          DescriptorError.InvalidAlternatives(view.id.value, tokens)
        )
      case FieldKind.Group(ps, rule) =>
        val named = rule match
          case GroupRule.Independent    => Vector.empty
          case GroupRule.Ordered(pairs) => pairs.flatMap((a, b) => Vector(a, b))
          case GroupRule.Distinct(ns)   => ns
        def kindOf(n: FieldId) = ps.find(_.id == n).map(_.kind)
        val misfit             = rule match
          case GroupRule.Independent    => None
          case GroupRule.Ordered(pairs) =>
            pairs
              .flatMap((a, b) => Vector(a, b))
              .find(n => !kindOf(n).exists(_.isInstanceOf[FieldKind.Numeric]))
          case GroupRule.Distinct(ns) =>
            ns.find(n =>
              !kindOf(n).exists {
                case FieldKind.Choice(_) | FieldKind.Text | FieldKind.Reference => true
                case _                                                          => false
              }
            )
        parts(ps).flatMap(_ =>
          named
            .find(n => !ps.exists(_.id == n))
            .toLeft(())
            .left
            .map(
              DescriptorError.UnknownRulePart(view.id, _)
            )
            .flatMap(_ => misfit.toLeft(()).left.map(DescriptorError.RulePartKind(view.id, _)))
        )
      case FieldKind.Optional(of, _)        => wellFormed(of)
      case FieldKind.Repeated(of, min, max) =>
        Either
          .cond(
            min >= 0 && max.forall(_ >= min),
            (),
            DescriptorError.InvalidRepetition(view.id, min, max)
          )
          .flatMap(_ => wellFormed(of))
      case FieldKind.Variant(cases) =>
        val tokens = cases.map(_.token)
        Either
          .cond(
            tokens.nonEmpty && tokens.distinct.size == tokens.size,
            (),
            DescriptorError.InvalidAlternatives(view.id.value, tokens)
          )
          .flatMap(_ => cases.traverse_(c => parts(c.parts)))
      case FieldKind.Choice(ChoiceSource.FromInput(_)) | FieldKind.Toggle | FieldKind.Text |
          FieldKind.Reference =>
        Right(())
