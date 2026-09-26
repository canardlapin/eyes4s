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

import eyes4s.core.Weight
import eyes4s.design.FailurePolicy
import eyes4s.detect.IvtThreshold
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import scala.compiletime.testing.typeCheckErrors

/** Form-grade descriptors: number text, bounds, the three parse stages, host
  * views as plain data, and fields that validate on their own. Runs on the
  * JVM and Scala.js.
  */
class FormDescriptorSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private def id(s: String): FieldId        = get(FieldId.of(s))
  private val F                             = RecipeParameters.forms

  test("one number grammar on every platform: signs, fractions, exponents, whitespace") {
    val real = summon[Numeral[Double]]
    assertEquals(
      Vector("1", "-2.5", ".5", "5.", "+3", "1e3", "2E-2", " 7 ").map(real.read),
      Vector(1.0, -2.5, 0.5, 5.0, 3.0, 1000.0, 0.02, 7.0).map(Some(_))
    )
    assertEquals(
      Vector(
        "",
        " ",
        "NaN",
        "Infinity",
        "-Infinity",
        "0x10",
        "1,5",
        "1e400",
        "--1",
        "1e",
        "e3",
        "1f"
      )
        .map(real.read),
      Vector.fill(12)(None)
    )
    val int32 = summon[Numeral[Int]]
    assertEquals(Vector("12", "-3", "+0").map(int32.read), Vector(Some(12), Some(-3), Some(0)))
    assertEquals(Vector("1.0", "1e2", "2147483648", "").map(int32.read), Vector.fill(4)(None))
    val int64 = summon[Numeral[Long]]
    assertEquals(int64.read("9223372036854775807"), Some(Long.MaxValue))
    assertEquals(int64.read("9223372036854775808"), None)
  }

  test("canonical number text is the shortest decimal that reads back exactly") {
    val real = summon[Numeral[Double]]
    assertEquals(
      Vector(2.0, 0.5, 0.1, -0.0, 1e-7, 1e20, 123.456, 0.1 + 0.2).map(real.write),
      Vector("2", "0.5", "0.1", "-0", "1E-7", "1E+20", "123.456", "0.30000000000000004")
    )
    Vector(
      Double.MinPositiveValue,
      Double.MaxValue,
      -Double.MaxValue,
      1.0 / 3,
      6.02214076e23,
      -0.0
    )
      .foreach { x =>
        val back = real.read(real.write(x))
        assertEquals(
          back.map(java.lang.Double.doubleToLongBits),
          Some(java.lang.Double.doubleToLongBits(x))
        )
      }
  }

  test("bounds need finite endpoints with the lower strictly below the upper") {
    import Endpoint.*
    assert(NumericBounds.of(Some(Closed(1)), Some(Closed(2))).isRight)
    assertEquals(
      NumericBounds.of(Some(Closed(2)), Some(Open(2))),
      Left(DescriptorError.InvalidBounds(Some(Closed(2)), Some(Open(2))))
    )
    assert(NumericBounds.of(Some(Open(Double.NegativeInfinity)), None).isLeft)
    assert(NumericBounds.of(None, Some(Closed(Double.NaN))).isLeft)
    val b = get(NumericBounds.of(Some(Open(0)), Some(Closed(10))))
    assertEquals(b.violation(0), Some(Side.Lower -> Open(0.0)))
    assertEquals(b.violation(10.5), Some(Side.Upper -> Closed(10.0)))
    assertEquals(Vector(1e-300, 10.0).map(b.contains), Vector(true, true))
    assertEquals(b.render, "(0, 10]")
  }

  test("an out-of-bounds value is refused naming the field, value, bound, side and unit") {
    assertEquals(
      F.sigma[Deg].parse(RawValue.Number(" 0 ")),
      Left(
        FieldError.OutOfBounds(
          id("sigma"),
          "0",
          0.0,
          Side.Lower,
          Endpoint.Open(0),
          Quantity.Planar(PlanarUnit.Deg)
        )
      )
    )
    val refused = F.minimumDuration.parse(RawValue.Number("-5")).swap.toOption.get
    assertEquals(
      refused.message,
      "minimumDurationMicros: -5 µs is out of bounds; it must be greater than 0 µs."
    )
    assertEquals(
      F.interpolationGap.parse(RawValue.Number("-1")).swap.toOption.get.message,
      "interpolationGapMicros: -1 µs is out of bounds; it must be at least 0 µs."
    )
    assertEquals(
      F.ekMinimumSamples.parse(RawValue.Number("0")).swap.toOption.get.message,
      "minimumSamples: 0 samples is out of bounds; it must be at least 1 samples."
    )
  }

  test("the stages run in order: shape, then bounds, then the domain constructor") {
    assertEquals(
      F.ivtThreshold.parse(RawValue.Number("fast")),
      Left(
        FieldError.Malformed(
          id("thresholdDegPerSecond"),
          RawValue.Number("fast"),
          Expected.Number(NumberShape.Real)
        )
      )
    )
    assertEquals(
      F.ivtThreshold.parse(RawValue.Absent),
      Left(FieldError.Missing(id("thresholdDegPerSecond")))
    )
    assertEquals(
      F.minimumDuration.parse(RawValue.Number("1.5")),
      Left(
        FieldError.Malformed(
          id("minimumDurationMicros"),
          RawValue.Number("1.5"),
          Expected.Number(NumberShape.Int64)
        )
      )
    )
    assertEquals(F.ivtThreshold.parse(RawValue.Number("30")).map(_.velocity.value), Right(30.0))
    // A domain refusal the bounds cannot state keeps the constructor's own error.
    val strict = get(
      NumericField.of[String, Double, Double](
        id("even"),
        1,
        "An even number",
        Quantity.Dimensionless,
        NumericBounds.unbounded
      )(
        x => Either.cond(x % 2 == 0, x, "an odd number"),
        identity,
        identity
      )
    )
    assertEquals(
      strict.parse(RawValue.Number("3")),
      Left(
        FieldError.Refused(id("even"), RawValue.Number("3"), "an odd number", "an odd number")
      )
    )
  }

  test("a typed value's raw form is canonical text and parses back") {
    val sigma = get(Sigma.of[Px](2.5))
    assertEquals(F.sigma[Px].raw(sigma), RawValue.Number("2.5"))
    assertEquals(F.sigma[Px].parse(F.sigma[Px].raw(sigma)).map(_.value), Right(2.5))
    val gap = get(RecipeParameters.interpolationGap.construct(Span.micros(75000)))
    assertEquals(F.interpolationGap.raw(gap), RawValue.Number("75000"))
  }

  test("each method field validates on its own, without a whole recipe") {
    val ivt = RecordingMethodDescriptor.ivt(DefinitionId.builtIn("test.ivt", 1)).parameters
    assertEquals(ivt.validate(id("thresholdDegPerSecond"), RawValue.Number("30")), Right(()))
    assert(ivt.validate(id("thresholdDegPerSecond"), RawValue.Number("-1")).left.exists {
      case FieldError.OutOfBounds(f, _, _, Side.Lower, Endpoint.Open(0), _) =>
        f == id("thresholdDegPerSecond")
      case _ => false
    })
    assert(ivt.validate(id("minimumDurationMicros"), RawValue.Number("0")).isLeft)
    assertEquals(
      ivt.validate(id("unknown"), RawValue.Number("1")),
      Left(FieldError.UnknownField(id("unknown")))
    )
    assertEquals(
      ivt.views.map(v => (v.id.value, v.quantity)),
      Vector(
        "thresholdDegPerSecond" -> Quantity.Rate(PlanarUnit.Deg),
        "minimumDurationMicros" -> Quantity.Duration
      )
    )
    // The shipped numeric parameters carry no default: none is scientifically universal.
    assert(ivt.views.forall(_.default.isEmpty))
    // A method with no parameters presents an empty form under its identity.
    assertEquals(
      ComparisonMethods.cosine.descriptor.formView,
      FormView(DefinitionId.cosine, Vector.empty)
    )
  }

  private val cell = FieldView.literal(
    "n",
    "A count",
    FieldKind.Numeric(Quantity.Count(Counted.Cells), NumberShape.Int32, NumericBounds.positive)
  )

  test("a view is checked when it is built: part ids, rule parts, integral bounds, defaults") {
    val x = FieldView.literal(
      "x",
      "x",
      FieldKind.Numeric(
        Quantity.Planar(PlanarUnit.Px),
        NumberShape.Real,
        NumericBounds.unbounded
      )
    )
    assertEquals(
      FieldView.of(id("g"), 1, "g", FieldKind.Group(Vector(x, x), GroupRule.Independent)),
      Left(DescriptorError.DuplicateFields(Vector("x", "x")))
    )
    assertEquals(
      FieldView.of(
        id("g"),
        1,
        "g",
        FieldKind.Group(Vector(x), GroupRule.Ordered(Vector(id("x") -> id("y"))))
      ),
      Left(DescriptorError.UnknownRulePart(id("g"), id("y")))
    )
    val half = get(NumericBounds.of(Some(Endpoint.Closed(0.5)), None))
    assertEquals(
      FieldView.of(
        id("n"),
        1,
        "n",
        FieldKind.Numeric(Quantity.Count(Counted.Cells), NumberShape.Int32, half)
      ),
      Left(DescriptorError.BoundsForShape(id("n"), NumberShape.Int32, half))
    )
    assertEquals(
      FieldView.of(
        id("n"),
        1,
        "n",
        cell.kind,
        Some(get(DefaultValue.of(RawValue.Number("0"), "zero")))
      ),
      Left(
        DescriptorError.DefaultRefused(
          id("n"),
          FieldError.OutOfBounds(
            id("n"),
            "0",
            0,
            Side.Lower,
            Endpoint.Open(0),
            Quantity.Count(Counted.Cells)
          )
        )
      )
    )
    assert(DefaultValue.of(RawValue.Number("1"), " ").isLeft)
    assert(FieldView.of(id("n"), 0, "n", cell.kind).isLeft)
    assert(FieldId.of("has space").isLeft)
    assert(FieldView.of(id("r"), 1, "r", FieldKind.Repeated(cell, 2, Some(1))).isLeft)
  }

  test("a group checks its parts and its rule; a variant, an option and a list check theirs") {
    import RawValue.*
    val window = RecipeViews.box("window", "w", PlanarUnit.Px, Vector.empty)
    def box(a: Double, b: Double, c: Double, d: Double) =
      Group(
        Vector("xMin" -> a, "yMin" -> b, "xMax" -> c, "yMax" -> d).map((k, v) =>
          id(k) -> Number(v.toString)
        )
      )
    assertEquals(window.check(box(0, 0, 10, 10)), Right(()))
    assertEquals(
      window.check(box(10, 0, 10, 10)),
      Left(FieldError.Unordered(id("window"), id("xMin"), id("xMax"), 10, 10))
    )
    assertEquals(
      window.check(Group(Vector(id("xMin") -> Number("0")))),
      Left(FieldError.Missing(id("window.yMin")))
    )
    assertEquals(
      window.check(Group(Vector(id("zMin") -> Number("0")))),
      Left(FieldError.UnknownPart(id("window"), id("zMin")))
    )
    val phases = RecipeViews.phases("phases", "p")
    assertEquals(
      phases.check(
        Group(Vector(id("focal") -> Choice("Encoding"), id("reference") -> Choice("Encoding")))
      ),
      Left(FieldError.Duplicate(id("phases"), id("reference"), "Encoding"))
    )
    val initial = RecipeViews.initialFixations(PlanarUnit.Px)
    assertEquals(initial.check(Variant("keepAll", Vector.empty)), Right(()))
    assertEquals(
      initial.check(Variant("dropAll", Vector.empty)),
      Left(
        FieldError.NotAChoice(
          id("initialFixations"),
          "dropAll",
          Vector("keepAll", "dropFirst", "dropLeadingInClosedDisc")
        )
      )
    )
    assertEquals(
      initial.check(
        Variant(
          "dropLeadingInClosedDisc",
          Vector(
            id("crossX") -> Number("1"),
            id("crossY") -> Number("2"),
            id("radius") -> Number("0")
          )
        )
      ),
      Left(
        FieldError.OutOfBounds(
          id("initialFixations.radius"),
          "0",
          0,
          Side.Lower,
          Endpoint.Open(0),
          Quantity.Planar(PlanarUnit.Deg)
        )
      )
    )
    val failure = RecipeViews.failurePolicy
    assertEquals(failure.check(Absent), Right(()))
    assertEquals(failure.check(Number("3")), Right(()))
    val list = FieldView.literal("scales", "s", FieldKind.Repeated(cell, 1, Some(2)))
    assertEquals(
      list.check(Items(Vector.empty)),
      Left(FieldError.ItemCount(id("scales"), 0, 1, Some(2)))
    )
    assertEquals(list.check(Items(Vector(Number("1"), Number("2")))), Right(()))
    assertEquals(
      RecipeViews.weight.check(Choice("Heavy")),
      Left(FieldError.NotAChoice(id("weight"), "Heavy", Vector("Uniform", "Duration")))
    )
  }

  test("library defaults are declared with their reason and pass their field's checks") {
    val pairing  = RecipeViews.pairing
    val defaults =
      FormLawsSupport.parts(pairing).flatMap(v => v.default.map(v.id.value -> _.raw))
    assertEquals(
      defaults,
      Vector(
        "matched"   -> RawValue.Variant("requireOne", Vector.empty),
        "controls"  -> RawValue.Choice("sameSelection"),
        "unmatched" -> RawValue.Choice("reportNoMatch")
      )
    )
    assertEquals(
      RecipeViews.initialFixations(PlanarUnit.Deg).default.map(_.raw),
      Some(RawValue.Variant("keepAll", Vector.empty))
    )
  }

  test("host views are plain data: no type members, and units are static") {
    assert(typeCheckErrors("(v: FieldView) => { val r: v.Raw = ???; r }").nonEmpty)
    assert(typeCheckErrors("(v: FormView) => { val r: v.Value = ???; r }").nonEmpty)
    assertEquals(typeCheckErrors("(v: FieldView) => v.check(RawValue.Number(\"1\"))"), Nil)
    // A form field takes raw values only: a typed number is not a raw value.
    assert(typeCheckErrors("RecipeParameters.forms.ivtThreshold.parse(30.0)").nonEmpty)
    // A degree field is not a pixel field.
    assert(
      typeCheckErrors(
        "val f: NumericField[RecipeParameterError, Double, Sigma[Unit2D.Px]] = RecipeParameters.forms.sigma[Unit2D.Deg]"
      ).nonEmpty
    )
  }

  test("a part's error names its path: the group, the case, the option or the item") {
    import RawValue.*
    val window = RecipeViews.box("window", "w", PlanarUnit.Px, Vector.empty)
    assertEquals(
      window.check(Group(Vector(id("xMin") -> Number("0"), id("xMin") -> Number("1")))),
      Left(FieldError.RepeatedPart(id("window"), id("xMin")))
    )
    assertEquals(
      RecipeViews.failurePolicy.check(Number("0")).left.map(_.field),
      Left(id("failurePolicy.minimumSuccessful"))
    )
    val list = FieldView.literal("scales", "s", FieldKind.Repeated(cell, 1, None))
    assertEquals(
      list.check(Items(Vector(Number("2"), Number("0")))).left.map(_.field),
      Left(id("scales.1"))
    )
  }

  test("64-bit parts compare exactly, in their own type") {
    import RawValue.*
    val window = RecipeViews.relativeWindow
    val big    = 9007199254740992L
    assertEquals(
      window.check(
        Group(
          Vector(id("start") -> Number(big.toString), id("end") -> Number((big + 1).toString))
        )
      ),
      Right(())
    )
    assert(
      window
        .check(
          Group(
            Vector(id("start") -> Number((big + 1).toString), id("end") -> Number(big.toString))
          )
        )
        .isLeft
    )
    val capped = FieldView.literal(
      "t",
      "t",
      FieldKind.Numeric(
        Quantity.Duration,
        NumberShape.Int64,
        get(NumericBounds.of(None, Some(Endpoint.Closed(big.toDouble))))
      )
    )
    assertEquals(capped.check(Number(big.toString)), Right(()))
    assert(
      capped.check(Number((big + 1).toString)).isLeft,
      "a value one past the endpoint was admitted"
    )
  }

  test("a rule names parts of the kind it compares") {
    val name = FieldView.literal("name", "n", FieldKind.Text)
    val t    = FieldView.literal(
      "t",
      "t",
      FieldKind.Numeric(Quantity.Duration, NumberShape.Int64, NumericBounds.unbounded)
    )
    assertEquals(
      FieldView.of(
        id("g"),
        1,
        "g",
        FieldKind.Group(Vector(name, t), GroupRule.Ordered(Vector(id("name") -> id("t"))))
      ),
      Left(DescriptorError.RulePartKind(id("g"), id("name")))
    )
    assertEquals(
      FieldView.of(
        id("g"),
        1,
        "g",
        FieldKind.Group(Vector(name, t), GroupRule.Distinct(Vector(id("name"), id("t"))))
      ),
      Left(DescriptorError.RulePartKind(id("g"), id("t")))
    )
  }

  test("a parameter's form must present the parameter's own view") {
    val field = RecipeParameters.forms.ivtThreshold
    val other = new ParameterDescriptor[Double, IvtThreshold, RecipeParameterError](
      RecipeParameters.minimumDuration.info,
      field.domain,
      _.message,
      None,
      Some(field)
    )
    assertEquals(
      ParameterSet.of(
        Vector(other.bind[IvtThreshold](identity)(t => Provenance.Param.Num(t.velocity.value)))
      ),
      Left(
        DescriptorError.FormViewMismatch(
          id("minimumDurationMicros"),
          id("thresholdDegPerSecond")
        )
      )
    )
    val matched = ParameterDescriptor.numeric(field)
    assert(
      ParameterSet
        .of(
          Vector(
            matched.bind[IvtThreshold](identity)(t => Provenance.Param.Num(t.velocity.value))
          )
        )
        .isRight
    )
  }

  test("every shipped form field's view is well formed") {
    val views = Vector(
      F.sigma[Px].view,
      F.sigmaX[Deg].view,
      F.sigmaY[Deg].view,
      F.residualLimit.view,
      F.ivtThreshold.view,
      F.minimumDuration.view,
      F.idtWidth.view,
      F.idtHeight.view,
      F.ekEtaX.view,
      F.ekEtaY.view,
      F.ekMinimumSamples.view,
      F.interpolationGap.view
    )
    views.foreach(v =>
      assertEquals(FieldView.of(v.id, v.version, v.meaning, v.kind, v.default), Right(v))
    )
  }

  test("hard doubles have pinned canonical text, and underflow is refused like overflow") {
    val real = summon[Numeral[Double]]
    assertEquals(
      Vector(
        Double.MinPositiveValue,
        2.2250738585072014e-308,
        Double.MaxValue,
        1e23,
        5e-324 * 3
      ).map(real.write),
      Vector(
        "5E-324",
        "2.2250738585072014E-308",
        "1.7976931348623157E+308",
        "1E+23",
        "1.5E-323"
      )
    )
    assertEquals(real.read("1e-400"), None)
    assertEquals(real.read("0e-400"), Some(0.0))
    assertEquals(real.read("0.000"), Some(0.0))
  }

  test("host accessors: views by id, value equality, inspection and detector forms") {
    assert(Endpoint.Closed(0).admits(0, Side.Lower) && !Endpoint.Open(0).admits(0, Side.Lower))
    assert(
      Endpoint.Open(1).admits(0.5, Side.Upper) && !Endpoint.Closed(1).admits(1.5, Side.Upper)
    )
    val v    = F.sigma[Px].view
    val same = get(FieldView.of(v.id, v.version, v.meaning, v.kind))
    assertEquals(same.hashCode, v.hashCode)
    assert(v.toString.contains("sigma"))
    val defaulted =
      get(v.withDefault(get(DefaultValue.of(RawValue.Number("1"), "a test default"))))
    assertEquals(defaulted.default.map(_.raw), Some(RawValue.Number("1")))
    assert(v.withDefault(get(DefaultValue.of(RawValue.Number("-1"), "out of bounds"))).isLeft)
    val info = ParameterInfo.of(v)
    assertEquals(info, RecipeParameters.sigma[Px].info)
    assertEquals(info.hashCode, RecipeParameters.sigma[Px].info.hashCode)
    assertEquals(info.version, 1)
    assert(info.toString.contains("sigma"))
    val ivt = RecordingMethodDescriptor.ivt(DefinitionId.builtIn("test.ivt", 1)).formView
    assertEquals(ivt.field(id("minimumDurationMicros")), Some(F.minimumDuration.view))
    assertEquals(ivt.field(id("absent")), None)
    val frame = get(Frame.screen("display", 800, 600))
    val plan  = get(
      StudyPlan.cosine(
        ArtifactRef.of[StudyInput[StudyKey, Px]](ContentHash.empty),
        get(Grid.of(GridId("g"), frame, 10, 8)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    val inspection = get(plan.inspect)
    assertEquals(inspection.views.map(_.id.value), inspection.description.map(_._1))
  }

/** The parts of a view, for the tests. */
private object FormLawsSupport:
  def parts(view: FieldView): Vector[FieldView] =
    view +: (view.kind match
      case FieldKind.Group(ps, _)       => ps.flatMap(parts)
      case FieldKind.Variant(cases)     => cases.flatMap(_.parts).flatMap(parts)
      case FieldKind.Optional(of, _)    => parts(of)
      case FieldKind.Repeated(of, _, _) => parts(of)
      case _                            => Vector.empty)
