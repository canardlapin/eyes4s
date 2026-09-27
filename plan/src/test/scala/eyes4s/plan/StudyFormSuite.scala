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
import eyes4s.kernel.Unit2D.Px

/** The study form on the Studio fixture's geometry: a 1920x1080 px screen,
  * the 1024x768 image window at (448, 156), a 64x48 grid and a declared
  * 35 px per degree (docs/studio/fixture/FIXTURE.md).
  */
class StudyFormSuite extends munit.FunSuite:
  import RawValue.*
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private def id(s: String): FieldId        = FieldId.literal(s)

  private val screen  = get(Frame.screen("screen", 1920, 1080))
  private val context = StudyFormContext(screen, Some(FrameId("image")), GridId("image-grid"))
  private val form    = new StudyForm(context)
  private val input   = ArtifactRef.of[StudyInput[StudyKey, Px]](ContentHash.empty)
  private val layout  = StudyKey.layout(DefinitionId.studyLayout)
  private val cosine  = ComparisonMethods.cosine.study[Px]

  private def degrees(sigmas: Double*): RawValue =
    Items(
      sigmas.toVector.map(s =>
        Variant(
          "degrees.gaussian",
          Vector(
            id("sigma") -> Number(Numeral.real.write(s)),
            id("edges") -> Choice("Truncate")
          )
        )
      )
    )

  private val fixture: FormValues = FormValues.of(
    id("phases") -> Group(
      Vector(id("focal") -> Choice("Retrieval"), id("reference") -> Choice("Encoding"))
    ),
    id("weight") -> Choice("Duration"),
    id("grid")   -> Group(Vector(id("columns") -> Number("64"), id("rows") -> Number("48"))),
    id("window") -> Group(
      Vector(
        id("xMin") -> Number("448"),
        id("yMin") -> Number("156"),
        id("xMax") -> Number("1472"),
        id("yMax") -> Number("924")
      )
    ),
    id("offWindow")    -> Choice("Exclude"),
    id("angularScale") -> Number("35"),
    id("scales")       -> degrees(0.5, 1, 2, 4),
    id("pairing")      -> Group(
      Vector(
        id("matched")   -> Variant("requireOne", Vector.empty),
        id("controls")  -> Choice("sameSelection"),
        id("unmatched") -> Choice("reportNoMatch")
      )
    ),
    id("initialFixations") -> Variant("keepAll", Vector.empty)
  )

  private def plan(values: FormValues) =
    form
      .parse(values)
      .left
      .map(_.toVector)
      .flatMap(r => r.plan(input, layout, cosine, ()).left.map(e => Vector(e)))

  private val fixturePlan = get(plan(fixture))

  test("the fixture's form values build its plan, and the plan's values restore them") {
    assertEquals(form.values(fixturePlan), fixture)
    assertEquals(fixturePlan.grid.nx, 64)
    assertEquals(fixturePlan.geometry.admission, screen)
  }

  test("derived facts: the cell in pixels and degrees, each sigma in cells and extent") {
    val facts = StudyAdvice.facts(fixturePlan)
    assertEquals(facts.unit, PlanarUnit.Px)
    assertEquals(facts.cell, PerAxis(16, 16))
    assertEquals(facts.cellDegrees, Some(PerAxis(16.0 / 35, 16.0 / 35)))
    assertEquals(facts.scales.map(_.cells.x), Vector(17.5, 35.0, 70.0, 140.0).map(_ / 16))
    assertEquals(facts.scales.map(_.degrees.map(_.x)), Vector(0.5, 1.0, 2.0, 4.0).map(Some(_)))
    assertEquals(facts.scales(3).extent, PerAxis(140.0 / 1024, 140.0 / 768))
  }

  test("advisories fire for 0.5 and 8 degrees on the fixture geometry, and not for 1, 2 or 4") {
    assertEquals(
      StudyAdvice.advisories(fixturePlan),
      Vector(StudyAdvisory.SigmaBelowCells(0, 17.5 / 16, StudyAdvice.MinimumSigmaCells))
    )
    val wide = get(plan(fixture.updated(id("scales"), degrees(1, 8))))
    assertEquals(
      StudyAdvice.advisories(wide),
      Vector(StudyAdvisory.SigmaNearUniform(1, 280.0 / 768, StudyAdvice.NearUniformFraction))
    )
    val advisory = Diagnostic.of(StudyAdvice.advisories(fixturePlan).head)
    assertEquals(advisory.code.render, "study-advisory.sigma-below-cells")
    assertEquals(advisory.severity, DiagnosticSeverity.Warning)
    assertEquals(
      StudyAdvice.advisories(fixturePlan).head.message,
      "Scale 0: sigma spans 1.09 grid cells, fewer than 2; the grid undersamples it."
    )
  }

  test("the recipe sentence carries typed roles (Studio S7.2)") {
    val sentence = StudyText.sentence(fixturePlan)
    assertEquals(
      sentence.text,
      "Compare Retrieval fixations with Encoding fixations of the same item, against the " +
        "other items, chosen alike, by Cosine similarity of duration-weighted maps at " +
        "σ 0.5°, σ 1°, σ 2°, σ 4°."
    )
    assertEquals(
      sentence.tokens.collect { case Token.Value(f, role, _) => f.value -> role },
      Vector(
        "phases.focal"     -> TokenRole.Query,
        "phases.reference" -> TokenRole.Reference,
        "pairing.matched"  -> TokenRole.Match,
        "pairing.controls" -> TokenRole.Control,
        "method"           -> TokenRole.Metric,
        "weight"           -> TokenRole.Policy,
        "scales"           -> TokenRole.Scale
      )
    )
  }

  test("the methods text states every declared recipe field (Studio S9.4)") {
    val methods = StudyText.methods(fixturePlan)
    assertEquals(
      methods.clauses.map(_.text),
      Vector(
        "Each Retrieval trial was compared with the Encoding trial of the same item and with " +
          "the Encoding trials of the other items, chosen alike, within participant.",
        "Queries without a matched reference were reported as no match.",
        "Maps covered the analysis window [448, 1472) × [156, 924) px of the admission frame; " +
          "fixations on the screen outside it were left out of the map.",
        "Degrees were converted linearly at a declared 35 px/°, not a calibration.",
        "Every fixation was kept.",
        "Maps were estimated on a 64 × 48 grid (cell 16 px, 0.46°).",
        "Each map summed duration-weighted fixations at σ 0.5°, σ 1°, σ 2°, σ 4°, truncated " +
          "at the grid edge; each scale was analysed on its own.",
        "Maps were compared by Cosine similarity.",
        "Every selected pair had to be scored."
      )
    )
    val dropping = get(
      plan(
        fixture.updated(
          id("initialFixations"),
          Variant(
            "dropLeadingInClosedDisc",
            Vector(
              id("crossX") -> Number("960"),
              id("crossY") -> Number("540"),
              id("radius") -> Number("1.5")
            )
          )
        )
      )
    )
    assert(
      StudyText
        .methods(dropping)
        .text
        .contains(
          "Leading fixations within 1.5° of the fixation cross at (960, 540) px were left out"
        )
    )
  }

  test("each field parses on its own; refusals name the field's path and accumulate") {
    assertEquals(
      form
        .validate(
          id("window"),
          Group(
            Vector(
              id("xMin") -> Number("0"),
              id("yMin") -> Number("0"),
              id("xMax") -> Number("2000"),
              id("yMax") -> Number("10")
            )
          )
        )
        .left
        .map(_.field),
      Left(id("window"))
    )
    assert(
      form
        .validate(
          id("grid"),
          Group(Vector(id("columns") -> Number("65536"), id("rows") -> Number("65536")))
        )
        .isLeft
    )
    assertEquals(
      form.validate(
        id("phases"),
        Group(Vector(id("focal") -> Choice("Encoding"), id("reference") -> Choice("Encoding")))
      ),
      Left(FieldError.Duplicate(id("phases"), id("reference"), "Encoding"))
    )
    assertEquals(
      form
        .validate(
          id("pairing"),
          Group(
            Vector(
              id("matched") -> Variant(
                "select",
                Vector(id("occurrence") -> Variant("at", Vector(id("n") -> Number("0"))))
              ),
              id("controls")  -> Choice("sameSelection"),
              id("unmatched") -> Choice("reportNoMatch")
            )
          )
        )
        .left
        .map(_.field),
      Left(id("pairing.matched.occurrence.n"))
    )
    assertEquals(
      form.validate(id("scales"), Items(Vector.empty)),
      Left(FieldError.ItemCount(id("scales"), 0, 1, None))
    )
    val broken = form.parse(
      fixture
        .updated(
          id("grid"),
          Group(Vector(id("columns") -> Number("0"), id("rows") -> Number("48")))
        )
        .updated(id("weight"), Choice("Heavy"))
        .updated(id("colour"), Text("red"))
    )
    assertEquals(
      broken.left.map(_.toVector.map(_.field.value)),
      Left(Vector("weight", "grid.columns", "colour"))
    )
  }

  test("whole-recipe checks run on demand and are keyed by the study field") {
    def field(values: FormValues) =
      get(form.parse(values)).plan(input, layout, cosine, ()).left.map(_.field)
    assertEquals(field(fixture.updated(id("offWindow"), Absent)), Left(StudyField.OffWindow))
    assertEquals(field(fixture.updated(id("window"), Absent)), Left(StudyField.OffWindow))
    assertEquals(
      field(fixture.updated(id("angularScale"), Absent)),
      Left(StudyField.AngularScale)
    )
    assertEquals(field(fixture.updated(id("scales"), degrees(1, 1))), Left(StudyField.Scales))
    val sameOccurrence = fixture.updated(
      id("pairing"),
      Group(
        Vector(
          id("matched")   -> Variant("sameOccurrence", Vector.empty),
          id("controls")  -> Choice("sameSelection"),
          id("unmatched") -> Choice("reportNoMatch")
        )
      )
    )
    assertEquals(field(sameOccurrence), Left(StudyField.MatchedReferences))
    val offScreen = fixture.updated(
      id("initialFixations"),
      Variant(
        "dropLeadingInClosedDisc",
        Vector(
          id("crossX") -> Number("5000"),
          id("crossY") -> Number("540"),
          id("radius") -> Number("1.5")
        )
      )
    )
    assertEquals(field(offScreen), Left(StudyField.InitialFixations))
    val refusal = get(form.parse(fixture.updated(id("offWindow"), Absent)))
      .plan(input, layout, cosine, ())
      .swap
      .toOption
      .get
    assertEquals(Diagnostic.of(refusal).code.render, "study-recipe.window-without-policy")
  }

  test("every closed choice of the shipped forms shows a label, not its token") {
    val views   = form.views ++ new TemporalForm().views ++ new RecordingForm().views
    val options = views
      .flatMap(FormViewParts.all)
      .collect {
        case v if v.kind.isInstanceOf[FieldKind.Choice] =>
          v.kind match
            case FieldKind.Choice(ChoiceSource.Fixed(options)) => options
            case _                                             => Vector.empty
      }
      .flatten
    assert(options.nonEmpty)
    options.foreach(o => assert(o.label.trim.nonEmpty && o.label != o.token, o))
  }

  test("a host's erased form-field error projects to a catalogued diagnostic") {
    val ivt     = RecordingMethodDescriptor.ivt(DefinitionId.builtIn("test.ivt", 1)).parameters
    val refused = ivt
      .validate(id("minimumDurationMicros"), Number("0"))
      .swap
      .toOption
      .get
    val out = Diagnose.reportedFormField(refused)
    assertEquals(out.code.render, "form-field.out-of-bounds")
    val domain: FieldError[Any] = FieldError.Refused(
      id("thresholdDegPerSecond"),
      Number("0"),
      RecipeParameterError.Configuration(
        eyes4s.detect.ConfigurationError.NonPositiveIvtThreshold(0)
      ),
      "threshold must be positive"
    )
    assertEquals(
      Diagnose.reportedFormField(domain).causes.map(_.code.render),
      Vector("recipe-parameter.configuration")
    )
    val extension: FieldError[Any] =
      FieldError.Refused(
        id("multiplier"),
        Number("-1"),
        "negative multiplier",
        "negative multiplier"
      )
    val projected = Diagnose.reportedFormField(extension)
    assertEquals(projected.code.render, "form-field.refused")
    assertEquals(projected.causes, Vector.empty)
  }

class StudyFormKeysSuite extends munit.FunSuite:
  import RawValue.*
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private def id(s: String): FieldId        = FieldId.literal(s)
  private val screen                        = get(Frame.screen("screen", 1920, 1080))

  test("a whole-frame context declares no window frame, so the form refuses a window") {
    val form   = new StudyForm(StudyFormContext(screen, None, GridId("g")))
    val region = Group(
      Vector(
        id("xMin") -> Number("0"),
        id("yMin") -> Number("0"),
        id("xMax") -> Number("10"),
        id("yMax") -> Number("10")
      )
    )
    assertEquals(
      form.window.parse(region),
      Left(
        FieldError.Refused(
          id("window"),
          region,
          RecipeParameterError.UndeclaredWindowFrame(FrameId("screen")),
          RecipeParameterError.UndeclaredWindowFrame(FrameId("screen")).message
        )
      )
    )
    assertEquals(form.window.parse(Absent), Right(None))
  }

  test("every study field maps to the form field that edits it, or to the context") {
    val form = new StudyForm(StudyFormContext(screen, None, GridId("g")))
    assertEquals(
      StudyField.values.toVector.filter(StudyForm.formField(_).isEmpty),
      Vector(StudyField.Input, StudyField.Layout, StudyField.Method)
    )
    assertEquals(
      Vector(
        StudyField.MatchedReferences,
        StudyField.ControlReferences,
        StudyField.UnmatchedFocal
      )
        .map(StudyForm.formField),
      Vector.fill(3)(Some(StudyForm.ids.pairing))
    )
    StudyField.values
      .flatMap(StudyForm.formField)
      .foreach(f => assert(form.views.exists(_.id == f), f))
    assertEquals(
      StudyRecipeError.fieldOf(PlanError.MissingAngularScale(0)),
      StudyField.AngularScale
    )
    assertEquals(
      StudyRecipeError.fieldOf(PlanError.UnmatchedFocalRefused(Vector.empty)),
      StudyField.UnmatchedFocal
    )
  }

  test("a whole-recipe refusal is located at its form field and keeps its cause") {
    val refusal = StudyRecipeError.Grid(GeometryError.DegenerateGrid(0, 4))
    val d       = Diagnostic.of(refusal)
    assertEquals(d.subject.head, Locus.Field("grid"))
    assertEquals(d.causes.map(_.code.render), Vector("geometry.degenerate-grid"))
    assertEquals(
      Diagnostic.of(StudyRecipeError.WindowWithoutPolicy).subject,
      Vector(Locus.Field("offWindow"))
    )
  }

private object FormViewParts:
  def all(view: FieldView): Vector[FieldView] =
    view +: (view.kind match
      case FieldKind.Group(ps, _)       => ps.flatMap(all)
      case FieldKind.Variant(cases)     => cases.flatMap(_.parts).flatMap(all)
      case FieldKind.Optional(of, _)    => all(of)
      case FieldKind.Repeated(of, _, _) => all(of)
      case _                            => Vector.empty)
