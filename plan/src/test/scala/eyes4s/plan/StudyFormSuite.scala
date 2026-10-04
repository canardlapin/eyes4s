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
        "For each query, M was its matched score and B the mean of its control scores, and " +
          "D = M − B. D measures spatial correspondence, not sequential replay.",
        "Every selected pair had to be scored, so a query's contrast needed all its pairs."
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

  test("the methods text follows the pairing, failure and edge policies") {
    val averaged = get(
      plan(
        fixture
          .updated(
            id("pairing"),
            Group(
              Vector(
                id("matched")   -> Variant("meanOfAll", Vector.empty),
                id("controls")  -> Choice("sameSelection"),
                id("unmatched") -> Choice("reportNoMatch")
              )
            )
          )
          .updated(id("failurePolicy"), Number("3"))
          .updated(
            id("scales"),
            Items(
              Vector(
                Variant(
                  "degrees.gaussian",
                  Vector(id("sigma") -> Number("1"), id("edges") -> Choice("Truncate"))
                ),
                Variant(
                  "degrees.gaussian",
                  Vector(id("sigma") -> Number("2"), id("edges") -> Choice("Renormalise"))
                )
              )
            )
          )
      )
    )
    val text = StudyText.methods(averaged).text
    assert(
      text.contains(
        "compared with the Encoding trials of every presentation of the same item, averaged"
      ),
      text
    )
    assert(text.contains("requiring at least 3 successful scores."), text)
    assert(
      text.contains(
        "at σ 1° (truncated at the grid edge), σ 2° (renormalised at the grid edge);"
      ),
      text
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

  // --- CR6d: methods-text fact slots ----------------------------------------------

  private def fact(slot: FactSlot, source: FactSource, value: FactValue): Fact =
    get(Fact.of(slot, source, value))
  private def count(n: Long) = FactValue.Count(n)
  private val ledger         = FactSource.Ledger.apply
  private val design         = FactSource.Design.apply
  private val cells          = Vector(
    CellKey(Vector("response" -> "Remembered"), "contrast", "D"),
    CellKey(Vector("response" -> "Forgotten"), "contrast", "D")
  )
  private val contrastKey =
    ContrastKey(Vector.empty, "response", "Remembered", "Forgotten")

  /** The facts of the Studio fixture (FIXTURE.md), as a host fills them. */
  private val fixtureFacts = get(
    MethodsFacts.of(
      Vector(
        fact(FactSlot.DatasetRevision, FactSource.Host("dataset"), FactValue.Label("r3")),
        fact(FactSlot.FixationRecords, ledger(LedgerCount.FixationRecords), count(11520)),
        fact(FactSlot.InventoryTrials, ledger(LedgerCount.InventoryTrials), count(960)),
        fact(FactSlot.Admitted, ledger(LedgerCount.Admitted), count(937)),
        fact(FactSlot.Quarantined, ledger(LedgerCount.Quarantined), count(17)),
        fact(
          FactSlot.QuarantineCause(AdmissionCause.Overlap),
          ledger(LedgerCount.Cause(AdmissionCause.Overlap)),
          count(6)
        ),
        fact(
          FactSlot.QuarantineCause(AdmissionCause.NoFixations),
          ledger(LedgerCount.Cause(AdmissionCause.NoFixations)),
          count(5)
        ),
        fact(
          FactSlot.QuarantineCause(AdmissionCause.DuplicateOrdinals),
          ledger(LedgerCount.Cause(AdmissionCause.DuplicateOrdinals)),
          count(4)
        ),
        fact(
          FactSlot.QuarantineCause(AdmissionCause.RejectedRecords),
          ledger(LedgerCount.Cause(AdmissionCause.RejectedRecords)),
          count(2)
        ),
        fact(FactSlot.Absent, ledger(LedgerCount.Cause(AdmissionCause.Absent)), count(6)),
        fact(
          FactSlot.RecordsOutsideWindow,
          ledger(LedgerCount.RecordsOutsideWindow),
          count(543)
        ),
        fact(FactSlot.TrialsOutsideWindow, ledger(LedgerCount.TrialsOutsideWindow), count(409)),
        fact(FactSlot.RequestedQueries, design(DesignCount.RequestedQueries), count(480)),
        fact(FactSlot.EligibleQueries, design(DesignCount.EligibleQueries), count(457)),
        fact(FactSlot.UnmatchedQueries, design(DesignCount.UnmatchedQueries), count(9)),
        fact(FactSlot.NotAdmittedQueries, design(DesignCount.NotAdmittedQueries), count(14)),
        fact(
          FactSlot.ControlsPerQuery,
          design(DesignCount.ControlsPerQuery),
          FactValue.Controls(
            FactValue.Range(18, 19),
            Vector(FewerControls(171, 18, AdmissionCause.Overlap))
          )
        ),
        fact(
          FactSlot.ReportingSpec,
          FactSource.ReportSpec("by-response"),
          FactValue.Label("retrieval response")
        ),
        fact(FactSlot.GroupSizeRange, FactSource.ReportCells(cells), FactValue.Range(2, 17)),
        fact(FactSlot.PairedN, FactSource.ReportContrast(contrastKey), count(24)),
        fact(
          FactSlot.BelowMinimumQueries,
          FactSource.ReportCells(cells),
          FactValue.Breakdown(Vector(BelowMinimum("P05", "Forgotten", 1)))
        )
      )
    )
  )

  test("CR6d: the methods text states the facts it is given, each with its slot and source") {
    val text    = StudyText.methods(fixturePlan, fixtureFacts)
    val byTopic = text.clauses.groupBy(_.topic).view.mapValues(_.map(_.text)).toMap
    assertEquals(
      byTopic(ClauseTopic.Admission),
      Vector(
        "Fixations (11,520 records) from dataset r3 were admitted by trial: 937 of 960 " +
          "inventory trials were admitted, 17 quarantined (overlap 6, no-fixations 5, " +
          "duplicate-ordinals 4, rejected-records 2) and 6 absent (no fixation records)."
      )
    )
    assertEquals(
      byTopic(ClauseTopic.Design),
      Vector(
        "Of 480 requested queries, 457 were eligible; 9 had no matched reference and 14 were " +
          "not admitted.",
        "Each eligible query had 18–19 controls (171 queries had 18: overlap)."
      )
    )
    assertEquals(
      byTopic(ClauseTopic.Reporting),
      Vector(
        "Results were reported by retrieval response; groups held 2–17 participants and the " +
          "paired contrast 24.",
        "1 participant-group cells had fewer queries than the minimum (P05 · Forgotten · 1)."
      )
    )
    assert(
      text.text.contains(
        "fixations on the screen outside it were left out of the map. 543 records in 409 " +
          "trials fell outside it."
      ),
      text.text
    )
    // Every number is a fact or a recipe value, with its source.
    val sources = text.tokens.collect { case Token.Fact(slot, source, shown) =>
      (slot, source, shown)
    }
    assert(
      sources.contains((FactSlot.EligibleQueries, design(DesignCount.EligibleQueries), "457"))
    )
    assert(
      sources.contains(
        (FactSlot.PairedN, FactSource.ReportContrast(contrastKey), "24")
      )
    )
    assert(
      sources.contains(
        (
          FactSlot.QuarantineCause(AdmissionCause.Overlap),
          ledger(LedgerCount.Cause(AdmissionCause.Overlap)),
          "overlap 6"
        )
      )
    )
    val digits = text.tokens.collect { case Token.Words(w) if w.exists(_.isDigit) => w }
    assertEquals(digits, Vector.empty, "fixed wording states no number")
  }

  test("CR6d: facts not given are not stated; no facts states the plan alone") {
    val plain = StudyText.methods(fixturePlan)
    assertEquals(StudyText.methods(fixturePlan, MethodsFacts.empty), plain)
    assert(plain.tokens.forall {
      case Token.Fact(_, _, _) => false
      case _                   => true
    })
    val eligibleOnly = get(
      MethodsFacts.of(
        Vector(fact(FactSlot.EligibleQueries, design(DesignCount.EligibleQueries), count(457)))
      )
    )
    val clauses = StudyText.methods(fixturePlan, eligibleOnly).clauses
    assertEquals(
      clauses.filter(_.topic == ClauseTopic.Design).map(_.text),
      Vector("457 queries were eligible.")
    )
    assertEquals(clauses.count(_.topic == ClauseTopic.Admission), 0)
    assertEquals(clauses.count(_.topic == ClauseTopic.Reporting), 0)
  }

  test("CR6d: a fact must have the value its slot takes; a slot is given once") {
    val src = FactSource.Host("x")
    assertEquals(
      Fact.of(FactSlot.EligibleQueries, src, FactValue.Label("457")),
      Left(FactError.WrongValue("eligibleQueries", "Label(457)", "a count"))
    )
    assertEquals(
      Fact.of(FactSlot.FixationRecords, src, count(-1)),
      Left(FactError.NegativeCount("fixationRecords", -1))
    )
    assertEquals(
      Fact.of(FactSlot.GroupSizeRange, src, FactValue.Range(17, 2)),
      Left(FactError.EmptyRange("groupSizeRange", 17, 2))
    )
    assertEquals(
      Fact.of(FactSlot.ReportingSpec, src, FactValue.Label(" ")),
      Left(FactError.BlankLabel("reportingSpec"))
    )
    assertEquals(
      Fact.of(
        FactSlot.ControlsPerQuery,
        src,
        FactValue.Controls(
          FactValue.Range(18, 19),
          Vector(FewerControls(3, 19, AdmissionCause.Absent))
        )
      ),
      Left(FactError.NotFewer("controlsPerQuery", 19, 19))
    )
    val one = fact(FactSlot.PairedN, src, count(24))
    assertEquals(MethodsFacts.of(Vector(one, one)), Left(FactError.Duplicate("pairedN")))
    assertEquals(
      FactSlot.QuarantineCause(AdmissionCause.DuplicateOrdinals).name,
      "quarantineCause.duplicate-ordinals"
    )
    assertEquals(
      Diagnostic.of(FactError.Duplicate("pairedN")).code.render,
      "methods-fact.duplicate"
    )
  }

  test("CR6d: admission causes carry the boards' labels") {
    assertEquals(
      Vector(
        QuarantineCause.Overlap(2, "a", "b"),
        QuarantineCause.NoFixations,
        QuarantineCause.DuplicateOrdinals,
        QuarantineCause.RejectedRecords,
        QuarantineCause.WrongClock(1, "c", "d")
      ).map(c => AdmissionCause.of(c).label),
      Vector("overlap", "no-fixations", "duplicate-ordinals", "rejected-records", "wrong-clock")
    )
    assertEquals(
      Vector(
        TrialDisposition.Admitted,
        TrialDisposition.NoFixations,
        TrialDisposition.Absent
      ).map(AdmissionCause.of(_).map(_.label)),
      Vector(None, Some("no-fixations"), Some("absent (no fixation records)"))
    )
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

class RecipeFormAccessorsSuite extends munit.FunSuite:
  import RawValue.*
  private def id(s: String): FieldId = FieldId.literal(s)

  test("temporal and recording fields validate on their own; labels and empty forms") {
    val temporal = new TemporalForm
    assertEquals(temporal.validate(id("temporal.boundary"), Choice("ClipDuration")), Right(()))
    assert(temporal.validate(id("windows"), Items(Vector.empty)).isLeft)
    assertEquals(
      temporal.validate(id("nope"), Absent),
      Left(FieldError.UnknownField(id("nope")))
    )
    val recording = new RecordingForm
    assertEquals(recording.validate(id("syncModel"), Choice("Affine")), Right(()))
    assert(recording.validate(id("interpolationGapMicros"), Number("-1")).isLeft)
    assertEquals(FormValues.empty.get(id("anything")), Absent)
    assertEquals(FormValues.empty.updated(id("x"), Absent), FormValues.empty)
    assertEquals(
      Labelled[eyes4s.core.Weight].label(eyes4s.core.Weight.Uniform),
      "Every fixation counts once"
    )
    assertEquals(StudyAdvisory.SigmaBelowCells(0, 1, 2).field, StudyField.Scales)
  }

private object FormViewParts:
  def all(view: FieldView): Vector[FieldView] =
    view +: (view.kind match
      case FieldKind.Group(ps, _)       => ps.flatMap(all)
      case FieldKind.Variant(cases)     => cases.flatMap(_.parts).flatMap(all)
      case FieldKind.Optional(of, _)    => all(of)
      case FieldKind.Repeated(of, _, _) => all(of)
      case _                            => Vector.empty)
