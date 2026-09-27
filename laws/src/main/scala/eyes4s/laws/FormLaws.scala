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

package eyes4s.laws

import eyes4s.kernel.Provenance
import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for form-grade descriptor fields.
  *
  * A [[NumericField]] parses a raw value in three stages: its shape, its
  * declared bounds, then the domain constructor. The bounds are a statement
  * about the constructor, so the laws hold them to it:
  *
  *  - at every declared endpoint, one representable step inside it and one
  *    outside it, the bounds admit a number exactly when the constructor
  *    accepts it; on a side with no endpoint, the constructor accepts the
  *    shape's extreme number (a missing bound is caught there);
  *  - a number outside the bounds is refused as `OutOfBounds`, naming the
  *    field, the text, the side and the endpoint it violates;
  *  - the raw form of a typed value parses back to the same value, and any
  *    spelling of an accepted number parses as its canonical text does;
  *  - canonical number text reads back exactly;
  *  - text that is not a number, a missing value and a value of another shape
  *    are refused by the shape stage, naming the field; and
  *  - a declared default parses.
  *
  * The endpoint law has no tolerance: bounds and constructors meet at exact,
  * representable numbers.
  *
  * [[inspection]] checks that every field of a recipe inspection, children
  * included, carries a well-formed view: `FieldView.of` rebuilds it, its
  * defaults pass its own checks, a field whose described values hold a
  * number has a numeric part (a reference to a versioned definition, which
  * is described by its name and version, excepted), and every numeric part
  * names a numeric quantity.
  */
object FormLaws extends Laws:

  /** Numbers at, just inside and just outside each endpoint, and the
    * extreme on each unbounded side.
    */
  def probes[E, N, A](field: NumericField[E, N, A]): Vector[N] =
    val n                              = field.numeral
    def around(e: Endpoint): Vector[N] =
      n.exactly(e.value).toVector.flatMap { at =>
        at +: Vector(n.step(at, up = true), n.step(at, up = false)).flatten
      }
    field.bounds.endpoints.map(_._2).flatMap(around) ++
      Option.when(field.bounds.lower.isEmpty)(n.extreme(up = false)).toVector ++
      Option.when(field.bounds.upper.isEmpty)(n.extreme(up = true)).toVector

  /** Numbers inside and around the bounds, with the probes. */
  def numbers[E, N, A](field: NumericField[E, N, A]): Gen[N] =
    val hi =
      field.bounds.upper.fold(field.bounds.lower.fold(0.0)(_.value).max(0) + 1e6)(_.value)
    val lo = field.bounds.lower.fold(hi - 2e6)(_.value)
    Gen.frequency(
      4 -> Gen.choose(lo, hi).map(field.numeral.nearest),
      1 -> Gen.choose(lo - 10, lo + 10).map(field.numeral.nearest),
      1 -> Gen.oneOf(probes(field) :+ field.numeral.nearest(lo))
    )

  def numeric[E, N, A](field: NumericField[E, N, A], values: Gen[N]): RuleSet =
    val id                                          = field.view.id
    val numeral                                     = field.numeral
    def text(n: N): RawValue                        = RawValue.Number(numeral.write(n))
    def outside(side: Side, e: Endpoint): Vector[N] =
      numeral.exactly(e.value).toVector.flatMap { at =>
        Vector(Some(at), numeral.step(at, up = side == Side.Upper)).flatten
          .filterNot(x => field.bounds.contains(numeral.toDouble(x)))
      }
    new SimpleRuleSet(
      "formField",
      "the declared bounds and the domain constructor agree at the edges" -> {
        val disagreements = probes(field).filter { x =>
          field.bounds.contains(numeral.toDouble(x)) != field.domain(x).isRight
        }
        Prop(disagreements.isEmpty) :|
          s"$id ${field.bounds.render}: bounds and constructor disagree at ${disagreements.map(numeral.write)}"
      },
      "a value outside the bounds is refused naming the bound" -> {
        val wrong = field.bounds.endpoints.flatMap { (side, e) =>
          outside(side, e).flatMap { x =>
            val expected = FieldError.OutOfBounds(
              id,
              numeral.write(x),
              numeral.toDouble(x),
              side,
              e,
              field.quantity
            )
            Option.when(field.parse(text(x)) != Left(expected))(
              s"${numeral.write(x)} gave ${field.parse(text(x))}"
            )
          }
        }
        Prop(wrong.isEmpty) :| s"$id: ${wrong.mkString("; ")}"
      },
      "a typed value's raw form parses back to it" -> forAll(values) { n =>
        field.domain(n) match
          case Right(a) =>
            val back = field.parse(field.raw(a)).map(field.number)
            Prop(back == Right(field.number(a))) :| s"$id ${numeral.write(n)}: $back"
          case Left(_) =>
            Prop(field.parse(text(n)).isLeft) :| s"$id accepted ${numeral.write(n)}"
      },
      "any spelling of a number parses as its canonical text does" -> forAll(values) { n =>
        val canonical = field.parse(text(n)).map(field.number).toOption
        val spellings =
          Vector(s" ${numeral.write(n)} ", n.toString).map(t => field.parse(RawValue.Number(t)))
        Prop(
          spellings.forall(_.map(field.number).toOption == canonical)
        ) :| s"$id ${n.toString}"
      },
      "canonical number text reads back exactly" -> forAll(values) { n =>
        Prop(
          numeral.read(numeral.write(n)) == Some(n)
        ) :| s"${n.toString} -> ${numeral.write(n)}"
      },
      "the shape stage refuses what is not a number, naming the field" -> {
        val expected = Expected.Number(numeral.shape)
        Prop(
          field.parse(RawValue.Number("x")) ==
            Left(FieldError.Malformed(id, RawValue.Number("x"), expected))
        ) :| "text" &&
        Prop(field.parse(RawValue.Number("NaN")).isLeft) :| "NaN" &&
        Prop(field.parse(RawValue.Absent) == Left(FieldError.Missing(id))) :| "absent" &&
        Prop(
          field.parse(RawValue.Choice("1")) ==
            Left(FieldError.Malformed(id, RawValue.Choice("1"), expected))
        ) :| "choice"
      },
      "a declared default parses" -> {
        val refused = field.view.default.map(d => field.parse(d.raw)).filter(_.isLeft)
        Prop(refused.isEmpty) :| s"$id default: $refused"
      }
    )

  /** The views of a field and of every part it contains. */
  def parts(view: FieldView): Vector[FieldView] =
    view +: (view.kind match
      case FieldKind.Group(ps, _)       => ps.flatMap(parts)
      case FieldKind.Variant(cases)     => cases.flatMap(_.parts).flatMap(parts)
      case FieldKind.Optional(of, _)    => parts(of)
      case FieldKind.Repeated(of, _, _) => parts(of)
      case _                            => Vector.empty)

  private def inspected(fields: Vector[InspectedParameter]): Vector[InspectedParameter] =
    fields.flatMap(f => f +: inspected(f.children))

  def inspection(inspections: Gen[Either[DescriptorError, RecipeInspection]]): RuleSet =
    def each(check: InspectedParameter => Option[String]) =
      forAll(inspections) {
        case Left(e)  => Prop(false) :| e.message
        case Right(i) =>
          val problems = inspected(i.fields).flatMap(check)
          Prop(problems.isEmpty) :| problems.mkString("; ")
      }
    new SimpleRuleSet(
      "formInspection",
      "every field's view restates its id and meaning" -> each { f =>
        Option.unless(
          f.info.view.id.value == f.info.id && f.info.view.meaning == f.info.meaning
        )(
          s"${f.info.id} has view ${f.info.view.id}"
        )
      },
      "every view is well formed: FieldView.of rebuilds it" -> each { f =>
        val v       = f.info.view
        val rebuilt = FieldView.of(v.id, v.version, v.meaning, v.kind, v.default)
        Option.unless(rebuilt == Right(v))(s"${f.info.id}: $rebuilt")
      },
      "a field whose values hold numbers has a numeric part" -> each { f =>
        // A reference to a versioned definition is its name and version.
        val definition = f.info.kind == FieldKind.Reference && (f.values match
          case Vector(Provenance.Param.Text(_), Provenance.Param.Num(v)) => v.isWhole && v >= 1
          case _                                                         => false)
        val numbers = !definition && f.values.exists {
          case Provenance.Param.Num(_) => true
          case _                       => false
        }
        val numeric = parts(f.info.view).exists(_.kind.isInstanceOf[FieldKind.Numeric])
        Option.when(numbers && !numeric)(
          s"${f.info.id} describes numbers without a numeric part"
        )
      },
      "every numeric part names a numeric quantity" -> each { f =>
        parts(f.info.view).collectFirst {
          case p if p.kind match
                case FieldKind.Numeric(q, _, _) => !q.isNumeric
                case _                          => false
              =>
            s"${f.info.id}.${p.id} is numeric with quantity ${p.quantity}"
        }
      }
    )
