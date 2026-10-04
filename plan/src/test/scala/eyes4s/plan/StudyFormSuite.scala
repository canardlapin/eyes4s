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
    // Each sentence names the recipe field it states, so a host can link it to that field.
    // The contrast sentence is about the study's contrast, not one field.
    assertEquals(
      methods.clauses.map(_.field.map(_.value)),
      Vector(
        "phases",
        "pairing",
        "window",
        "angularScale",
        "initialFixations",
        "grid",
        "scales",
        "method"
      ).map(Some(_)) ++ Vector(None, Some("failurePolicy"))
    )
    assertEquals(
      methods.clauses.filter(_.field.isEmpty).map(_.topic),
      Vector(ClauseTopic.Contrast)
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
    // Under MeanOfAll a query has several matched scores, and M is their mean.
    assertEquals(
      StudyText.methods(averaged).clauses.filter(_.topic == ClauseTopic.Contrast).map(_.text),
      Vector(
        "For each query, M was the mean of its matched scores and B the mean of its control " +
          "scores, and D = M − B. D measures spatial correspondence, not sequential replay."
      )
    )
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
  private def count(n: Long)  = FactValue.Count(n)
  private def code(s: String) = get(FactCode.of(s))
  private val ledger          = FactSource.Ledger.apply
  private val run             = FactSource.Run.apply
  private def cellKey(group: String, participant: Option[String] = None) =
    CellKey(Vector("response" -> group), "contrast", "D", 0, participant)
  private val cells       = Vector(cellKey("Remembered"), cellKey("Forgotten"))
  private val contrastKey = ContrastKey(Vector.empty, "response", "Remembered", "Forgotten", 0)
  private val overlap     = code("overlap")
  private val offWindow   = code("study-contrast.off-window")
  private def groupCell(participant: String, queries: Int) =
    GroupCell(
      participant,
      "Forgotten",
      queries,
      FactSource.ReportCells(Vector(cellKey("Forgotten", Some(participant))))
    )

  /** The facts of the Studio fixture (FIXTURE.md), as a host fills them; the
    * totals agree with their parts.
    */
  private val fixtureFacts = get(
    MethodsFacts.of(
      Vector(
        fact(FactSlot.DatasetRevision, FactSource.Host("dataset"), FactValue.Label("r3")),
        fact(FactSlot.FixationRecords, ledger(LedgerCount.FixationRecords), count(11520)),
        fact(FactSlot.TalliedRecords, ledger(LedgerCount.TalliedRecords), count(11311)),
        fact(FactSlot.InventoryTrials, ledger(LedgerCount.InventoryTrials), count(960)),
        fact(FactSlot.Admitted, ledger(LedgerCount.Admitted), count(937)),
        fact(FactSlot.Quarantined, ledger(LedgerCount.Quarantined), count(12)),
        fact(FactSlot.QuarantineCause(overlap), ledger(LedgerCount.Cause(overlap)), count(6)),
        fact(
          FactSlot.QuarantineCause(code("duplicate-ordinals")),
          ledger(LedgerCount.Cause(code("duplicate-ordinals"))),
          count(4)
        ),
        fact(
          FactSlot.QuarantineCause(code("rejected-records")),
          ledger(LedgerCount.Cause(code("rejected-records"))),
          count(2)
        ),
        fact(FactSlot.NoFixations, ledger(LedgerCount.NoFixations), count(5)),
        fact(FactSlot.Absent, ledger(LedgerCount.Absent), count(6)),
        fact(
          FactSlot.RecordsOutsideWindow,
          ledger(LedgerCount.RecordsOutsideWindow),
          count(543)
        ),
        fact(FactSlot.TrialsOutsideWindow, ledger(LedgerCount.TrialsOutsideWindow), count(409)),
        fact(
          FactSlot.OutsideWindowShare,
          ledger(LedgerCount.OutsideWindowShare),
          FactValue.Share(0.048, ShareBasis.FixationDuration)
        ),
        fact(
          FactSlot.RecordsOutsideScreen,
          ledger(LedgerCount.RecordsOutsideScreen),
          count(120)
        ),
        fact(FactSlot.TrialsOutsideScreen, ledger(LedgerCount.TrialsOutsideScreen), count(40)),
        fact(
          FactSlot.OffScreenPolicy,
          ledger(LedgerCount.OffScreenPolicy),
          FactValue.OffScreen(OffScreenPolicy.ExcludeRecord)
        ),
        fact(FactSlot.RequestedQueries, run(QueryTotal.Requested), count(480)),
        fact(FactSlot.EligibleQueries, run(QueryTotal.Eligible), count(457)),
        fact(FactSlot.ContributingQueries, run(QueryTotal.Contributing), count(454)),
        fact(FactSlot.FailedQueries, run(QueryTotal.Failed), count(3)),
        fact(FactSlot.FailureCause(offWindow), run(QueryTotal.Failure(offWindow)), count(3)),
        fact(FactSlot.UnmatchedQueries, run(QueryTotal.Unmatched), count(9)),
        fact(FactSlot.NotAdmittedQueries, run(QueryTotal.NotAdmitted), count(14)),
        fact(
          FactSlot.ControlsPerQuery,
          run(QueryTotal.ControlsPerQuery),
          FactValue.Controls(
            Vector(
              ControlCount(18, 171, Some(TrialLoss.Quarantined(overlap))),
              ControlCount(19, 286, None)
            )
          )
        ),
        fact(
          FactSlot.ReportingSpec,
          FactSource.ReportSpec("by-response"),
          FactValue.Label("retrieval response")
        ),
        fact(FactSlot.MinimumQueries, FactSource.ReportSpec("by-response"), count(3)),
        fact(FactSlot.GroupSizeRange, FactSource.ReportCells(cells), FactValue.Range(2, 17)),
        fact(
          FactSlot.GroupN("Remembered"),
          FactSource.ReportCells(Vector(cells(0))),
          count(24)
        ),
        fact(FactSlot.GroupN("Forgotten"), FactSource.ReportCells(Vector(cells(1))), count(23)),
        fact(FactSlot.PairedN, FactSource.ReportContrast(contrastKey), count(24)),
        fact(
          FactSlot.BelowMinimumQueries,
          FactSource.ReportCells(cells),
          FactValue.Breakdown(Vector(groupCell("P05", 1)))
        ),
        fact(
          FactSlot.SmallestGroups,
          FactSource.ReportCells(cells),
          FactValue.Breakdown(Vector(groupCell("P17", 2), groupCell("P21", 2)))
        )
      )
    )
  )

  private val unwindowedPlan =
    get(plan(fixture.updated(id("window"), Absent).updated(id("offWindow"), Absent)))

  private def factTokens(text: Phrase): Vector[Token.Fact] =
    text.tokens.collect { case t: Token.Fact => t }

  test("CR6d: the methods text states the facts it is given") {
    val text    = StudyText.methods(fixturePlan, fixtureFacts)
    val byTopic = text.clauses.groupBy(_.topic).view.mapValues(_.map(_.text)).toMap
    assertEquals(
      byTopic(ClauseTopic.Admission),
      Vector(
        "Fixations (11,520 records) from dataset r3 were admitted by trial: 937 of 960 " +
          "inventory trials were admitted, 12 quarantined (overlap 6, duplicate-ordinals 4, " +
          "rejected-records 2), 5 with no admitted fixations and 6 absent (no fixation records).",
        "120 records, in 40 trials, fell outside the screen; they were admitted but left out " +
          "of every map."
      )
    )
    assertEquals(
      byTopic(ClauseTopic.Design),
      Vector(
        "Of 480 requested queries, 457 were eligible; 454 contributed, 3 failed (off-window 3), " +
          "9 had no admitted matched trial and 14 were not admitted.",
        "Of the compared queries, 286 had 19 controls and 171 had 18 controls (one lost to " +
          "overlap)."
      )
    )
    assertEquals(
      byTopic(ClauseTopic.Reporting),
      Vector(
        "Results were reported by retrieval response; n = 24 Remembered and 23 Forgotten; " +
          "the paired contrast held 24.",
        "Per participant, groups held 2–17 queries; participant-group cells with fewer than 3 " +
          "queries were left out.",
        "1 participant-group cell had fewer queries than the minimum (P05 · Forgotten · 1 query).",
        "2 participant-group cells had the fewest queries (P17 · Forgotten · 2 queries, " +
          "P21 · Forgotten · 2 queries)."
      )
    )
    assert(
      text.text.contains(
        "fixations on the screen outside it were left out of the map. 543 of the 11,311 " +
          "fixation records of admitted trials, in 409 trials, fell outside it (4.8% of their " +
          "fixation duration)."
      ),
      text.text
    )
    val tokens = factTokens(text)
    // Each number and name is its own token, with its slot, part, source and typed value.
    val cause = FactSlot.QuarantineCause(overlap)
    assert(
      tokens.contains(
        Token.Fact(
          cause,
          FactPart.Name,
          ledger(LedgerCount.Cause(overlap)),
          FactValue.Label("overlap"),
          "overlap"
        )
      )
    )
    assert(
      tokens.contains(
        Token.Fact(cause, FactPart.Value, ledger(LedgerCount.Cause(overlap)), count(6), "6")
      )
    )
    val controls = tokens.filter(_.slot == FactSlot.ControlsPerQuery)
    assertEquals(
      controls.map(t => (t.part, t.value)),
      Vector(
        FactPart.Queries  -> count(286),
        FactPart.Controls -> count(19),
        FactPart.Queries  -> count(171),
        FactPart.Controls -> count(18),
        FactPart.Loss     -> FactValue.Label("overlap")
      )
    )
    assert(controls.forall(_.source == run(QueryTotal.ControlsPerQuery)))
    val p05 =
      tokens.filter(t => t.slot == FactSlot.BelowMinimumQueries && t.part != FactPart.Value)
    assertEquals(
      p05.map(t => (t.part, t.value, t.source)),
      Vector(
        (FactPart.Participant, FactValue.Label("P05"), groupCell("P05", 1).source),
        (FactPart.Group, FactValue.Label("Forgotten"), groupCell("P05", 1).source),
        (FactPart.Queries, count(1), groupCell("P05", 1).source)
      )
    )
    assert(
      tokens.contains(
        Token.Fact(
          FactSlot.OutsideWindowShare,
          FactPart.Value,
          ledger(LedgerCount.OutsideWindowShare),
          FactValue.Share(0.048, ShareBasis.FixationDuration),
          "4.8%"
        )
      )
    )
    val digits = text.tokens.collect { case Token.Words(w) if w.exists(_.isDigit) => w }
    assertEquals(digits, Vector.empty, "fixed wording states no number")
    // Each fact's value is stated exactly once; a controls fact states its
    // counts instead of its range.
    fixtureFacts.facts.foreach { f =>
      val values = tokens.count(t => t.slot == f.slot && t.part == FactPart.Value)
      assertEquals(values, if f.slot == FactSlot.ControlsPerQuery then 0 else 1, f.slot.slotId)
    }
  }

  test("CR6d: facts not given are not stated; no facts states the plan alone") {
    val plain = StudyText.methods(fixturePlan)
    assertEquals(StudyText.methods(fixturePlan, MethodsFacts.empty), plain)
    assertEquals(factTokens(plain), Vector.empty)
    val eligibleOnly = get(
      MethodsFacts.of(
        Vector(fact(FactSlot.EligibleQueries, run(QueryTotal.Eligible), count(457)))
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

  /** The slot and source of every number and name a fact states: its own, and
    * a breakdown cell's.
    */
  private def expectedSources(facts: Vector[Fact]): Set[(FactSlot, FactSource)] =
    facts.flatMap { f =>
      (f.slot, f.source) +: (f.value match
        case FactValue.Breakdown(cs) => cs.map(c => (f.slot, c.source))
        case _                       => Vector.empty)
    }.toSet

  test("CR6d: every fact given is stated, alone or with any other facts, on either geometry") {
    val all     = fixtureFacts.facts
    val subsets =
      Vector(all) ++ all.map(Vector(_)) ++ all.indices.map(i => all.patch(i, Nil, 1))
    var stated = 0
    for
      (geometry, p) <- Vector("windowed" -> fixturePlan, "unwindowed" -> unwindowedPlan)
      facts         <- subsets
      accepted      <- MethodsFacts.of(facts).toOption
    do
      stated += 1
      val text   = StudyText.methods(p, accepted)
      val ids    = facts.map(_.slot.slotId)
      val tokens = factTokens(text)
      assertEquals(
        tokens.map(t => (t.slot, t.source)).toSet,
        expectedSources(facts),
        s"$geometry $ids"
      )
      assert(tokens.forall(_.shown.nonEmpty), s"$geometry $ids")
      assert(
        text.clauses.forall(c => c.text.headOption.exists(ch => ch.isUpper || ch.isDigit)),
        s"$geometry $ids: ${text.clauses.map(_.text)}"
      )
    // Leaving out one cause makes the quarantined count disagree, so a few
    // subsets are refused; every singleton and the whole set are stated.
    assert(stated >= 2 * (1 + all.size), stated)
  }

  test("CR6d: facts whose companions are absent still read as sentences") {
    def only(slots: FactSlot*) =
      get(MethodsFacts.of(fixtureFacts.facts.filter(f => slots.contains(f.slot))))
    def topic(t: ClauseTopic, p: StudyPlan[?, Px, ?, ?, ?], facts: MethodsFacts) =
      StudyText.methods(p, facts).clauses.filter(_.topic == t).map(_.text)
    assertEquals(
      topic(
        ClauseTopic.Admission,
        fixturePlan,
        only(
          FactSlot.InventoryTrials,
          FactSlot.QuarantineCause(overlap),
          FactSlot.QuarantineCause(code("rejected-records"))
        )
      ),
      Vector("Of 960 inventory trials, some were quarantined (overlap 6, rejected-records 2).")
    )
    assertEquals(
      topic(ClauseTopic.Admission, fixturePlan, only(FactSlot.InventoryTrials)),
      Vector("The inventory held 960 trials.")
    )
    assertEquals(
      topic(
        ClauseTopic.Admission,
        fixturePlan,
        only(FactSlot.FixationRecords, FactSlot.InventoryTrials, FactSlot.TalliedRecords)
      ),
      Vector(
        "Fixations (11,520 records) were admitted by trial from 960 inventory trials.",
        "The admitted trials held 11,311 fixation records."
      )
    )
    assertEquals(
      topic(ClauseTopic.Admission, fixturePlan, only(FactSlot.OffScreenPolicy)),
      Vector("Records outside the screen were admitted but left out of every map.")
    )
    assertEquals(
      topic(
        ClauseTopic.Design,
        fixturePlan,
        only(
          FactSlot.UnmatchedQueries,
          FactSlot.NotAdmittedQueries,
          FactSlot.FailureCause(offWindow)
        )
      ),
      Vector(
        "Of the requested queries, some failed (off-window 3), 9 had no admitted matched " +
          "trial and 14 were not admitted."
      )
    )
    val trials = StudyText.methods(fixturePlan, only(FactSlot.TrialsOutsideWindow)).text
    assert(trials.contains("were left out of the map. Records in 409 trials fell outside it."))
    val share = StudyText.methods(fixturePlan, only(FactSlot.OutsideWindowShare)).text
    assert(share.contains(" Fixations outside it took 4.8% of the fixation duration."), share)
    val whole = StudyText.methods(unwindowedPlan, only(FactSlot.RecordsOutsideWindow)).text
    assert(whole.contains("Maps covered the whole admission frame"), whole)
    assert(whole.contains(". 543 records fell outside it."), whole)
    assertEquals(
      topic(ClauseTopic.Reporting, fixturePlan, only(FactSlot.PairedN)),
      Vector("The paired contrast held 24.")
    )
    assertEquals(
      topic(ClauseTopic.Reporting, fixturePlan, only(FactSlot.MinimumQueries)),
      Vector("Participant-group cells with fewer than 3 queries were left out.")
    )
  }

  test("CR6d: one of a thing is singular") {
    val one = get(
      MethodsFacts.of(
        Vector(
          fact(FactSlot.RequestedQueries, run(QueryTotal.Requested), count(1)),
          fact(FactSlot.EligibleQueries, run(QueryTotal.Eligible), count(1)),
          fact(
            FactSlot.ControlsPerQuery,
            run(QueryTotal.ControlsPerQuery),
            FactValue.Controls(Vector(ControlCount(1, 1, None)))
          ),
          fact(FactSlot.GroupSizeRange, FactSource.ReportCells(cells), FactValue.Range(1, 1))
        )
      )
    )
    val text = StudyText.methods(fixturePlan, one).clauses.map(_.text)
    assert(text.contains("Of 1 requested query, 1 was eligible."), text)
    assert(text.contains("Of the compared queries, 1 had 1 control."), text)
    assert(text.contains("Per participant, groups held 1 query."), text)
  }

  test("CR6d: a fact must have the value its slot takes") {
    val src                                       = FactSource.Host("x")
    def refused(slot: FactSlot, value: FactValue) = Fact.of(slot, src, value).left.toOption
    def controls(cs: ControlCount*)               = FactValue.Controls(cs.toVector)
    assertEquals(
      refused(FactSlot.EligibleQueries, FactValue.Label("457")),
      Some(FactError.WrongValue("eligibleQueries", "Label(457)", "a count"))
    )
    assertEquals(
      refused(FactSlot.FixationRecords, count(-1)),
      Some(FactError.NegativeCount("fixationRecords", -1))
    )
    assertEquals(
      refused(FactSlot.GroupSizeRange, FactValue.Range(17, 2)),
      Some(FactError.EmptyRange("groupSizeRange", 17, 2))
    )
    assertEquals(
      refused(FactSlot.GroupSizeRange, FactValue.Range(-1, 5)),
      Some(FactError.NegativeCount("groupSizeRange", -1))
    )
    assertEquals(
      refused(FactSlot.ReportingSpec, FactValue.Label(" ")),
      Some(FactError.BlankLabel("reportingSpec"))
    )
    assertEquals(
      refused(FactSlot.GroupN(" "), count(3)),
      Some(FactError.BlankLabel("groupN. "))
    )
    Vector(1.5, -0.1, Double.NaN).foreach { f =>
      refused(
        FactSlot.OutsideWindowShare,
        FactValue.Share(f, ShareBasis.FixationDuration)
      ) match
        case Some(FactError.InvalidShare("outsideWindowShare", g)) =>
          assert(g == f || (g.isNaN && f.isNaN), g)
        case other => fail(s"$f: $other")
    }
    assertEquals(
      refused(FactSlot.ControlsPerQuery, controls()),
      Some(FactError.NoCounts("controlsPerQuery"))
    )
    assertEquals(
      refused(
        FactSlot.ControlsPerQuery,
        controls(ControlCount(19, 3, None), ControlCount(19, 4, None))
      ),
      Some(FactError.RepeatedControls("controlsPerQuery", 19))
    )
    assertEquals(
      refused(
        FactSlot.ControlsPerQuery,
        controls(ControlCount(19, 3, Some(TrialLoss.Absent)), ControlCount(18, 4, None))
      ),
      Some(FactError.NotFewer("controlsPerQuery", 19))
    )
    assertEquals(
      refused(FactSlot.ControlsPerQuery, controls(ControlCount(19, -3, None))),
      Some(FactError.NegativeCount("controlsPerQuery", -3))
    )
    assertEquals(
      refused(FactSlot.ControlsPerQuery, controls(ControlCount(-1, 3, None))),
      Some(FactError.NegativeCount("controlsPerQuery", -1))
    )
    assertEquals(
      refused(FactSlot.BelowMinimumQueries, FactValue.Breakdown(Vector(groupCell("P05", -1)))),
      Some(FactError.NegativeCount("belowMinimumQueries", -1))
    )
    assertEquals(
      refused(FactSlot.SmallestGroups, FactValue.Breakdown(Vector(groupCell(" ", 2)))),
      Some(FactError.BlankLabel("smallestGroups"))
    )
    Vector("", "Overlap", "wrong clock", "a..b", ".a", "a-").foreach { s =>
      assertEquals(FactCode.of(s), Left(FactError.InvalidCode(s)))
    }
    assertEquals(
      Diagnostic.of(FactError.Duplicate("pairedN")).code.render,
      "methods-fact.duplicate"
    )
  }

  test("CR6d: a slot is given once, and totals agree with their parts") {
    val src = FactSource.Host("x")
    val a   = fact(FactSlot.PairedN, src, count(24))
    val b   = fact(FactSlot.EligibleQueries, src, count(4))
    // The first slot repeated in input order is the one named.
    assertEquals(
      MethodsFacts.of(Vector(a, b, b, a)),
      Left(FactError.Duplicate("eligibleQueries"))
    )
    assertEquals(MethodsFacts.of(Vector(b, a, a, b)), Left(FactError.Duplicate("pairedN")))
    def without(slot: FactSlot)           = fixtureFacts.facts.filterNot(_.slot == slot)
    def replaced(slot: FactSlot, n: Long) =
      fixtureFacts.facts.map(f => if f.slot == slot then fact(slot, f.source, count(n)) else f)
    assertEquals(
      MethodsFacts.of(replaced(FactSlot.InventoryTrials, 961)),
      Left(
        FactError.Inconsistent(
          "inventoryTrials",
          961,
          Vector("admitted", "quarantined", "noFixations", "absent"),
          960
        )
      )
    )
    // Without one disposition the inventory is not checked against the rest.
    assert(MethodsFacts.of(without(FactSlot.Absent)).isRight)
    assertEquals(
      MethodsFacts.of(without(FactSlot.QuarantineCause(overlap))),
      Left(
        FactError.Inconsistent(
          "quarantined",
          12,
          Vector("quarantineCause.duplicate-ordinals", "quarantineCause.rejected-records"),
          6
        )
      )
    )
    assertEquals(
      MethodsFacts.of(replaced(FactSlot.EligibleQueries, 458)),
      Left(
        FactError.Inconsistent(
          "eligibleQueries",
          458,
          Vector("contributingQueries", "failedQueries"),
          457
        )
      )
    )
    assertEquals(
      MethodsFacts.of(replaced(FactSlot.RequestedQueries, 479)),
      Left(
        FactError.Inconsistent(
          "requestedQueries",
          479,
          Vector("eligibleQueries", "unmatchedQueries", "notAdmittedQueries"),
          480
        )
      )
    )
    // Without the unmatched count the requested queries are not checked.
    assert(
      MethodsFacts
        .of(
          replaced(FactSlot.RequestedQueries, 479)
            .filterNot(_.slot == FactSlot.UnmatchedQueries)
        )
        .isRight
    )
    assertEquals(
      MethodsFacts.of(
        replaced(FactSlot.FailedQueries, 4).filterNot(_.slot == FactSlot.EligibleQueries)
      ),
      Left(
        FactError.Inconsistent(
          "failedQueries",
          4,
          Vector("failureCause.study-contrast.off-window"),
          3
        )
      )
    )
  }

  test("CR6d: slot ids are stable, distinct and read back") {
    val slots = FactSlot.fixed ++ Vector(
      FactSlot.QuarantineCause(overlap),
      FactSlot.FailureCause(offWindow),
      FactSlot.GroupN("Not remembered")
    )
    assertEquals(
      slots.map(_.slotId),
      Vector(
        "datasetRevision",
        "fixationRecords",
        "talliedRecords",
        "inventoryTrials",
        "admitted",
        "quarantined",
        "noFixations",
        "absent",
        "recordsOutsideWindow",
        "trialsOutsideWindow",
        "outsideWindowShare",
        "recordsOutsideScreen",
        "trialsOutsideScreen",
        "offScreenPolicy",
        "requestedQueries",
        "eligibleQueries",
        "notAdmittedQueries",
        "unmatchedQueries",
        "contributingQueries",
        "failedQueries",
        "controlsPerQuery",
        "reportingSpec",
        "minimumQueries",
        "groupSizeRange",
        "pairedN",
        "belowMinimumQueries",
        "smallestGroups",
        "quarantineCause.overlap",
        "failureCause.study-contrast.off-window",
        "groupN.Not remembered"
      )
    )
    assertEquals(slots.map(s => FactSlot.fromSlotId(s.slotId)), slots.map(Some(_)))
    Vector("quarantineCause.", "quarantineCause.Overlap", "groupN. ", "eligible").foreach {
      id =>
        assertEquals(FactSlot.fromSlotId(id), None, id)
    }
  }

  test("CR6d: causes are the ledger's codes; no fixations and absence are not causes") {
    assertEquals(
      Vector(
        QuarantineCause.Overlap(2, "a", "b"),
        QuarantineCause.NoFixations,
        QuarantineCause.DuplicateOrdinals,
        QuarantineCause.RejectedRecords,
        QuarantineCause.WrongClock(1, "c", "d")
      ).map(c => FactCode.of(c).label),
      Vector("overlap", "no-fixations", "duplicate-ordinals", "rejected-records", "wrong-clock")
    )
    assertEquals(
      DiagnosticCatalog.quarantine.codes.map(_.name).toSet,
      Set(
        "rejected-records",
        "duplicate-ordinals",
        "no-fixations",
        "overlap",
        "wrong-clock",
        "invalid-transition",
        "invalid-extent",
        "unmappable-fixation",
        "correction-conflict",
        "item-conflict",
        "occurrence-conflict",
        "not-in-inventory",
        "inventory-item-conflict"
      )
    )
    assertEquals(
      Vector(
        TrialDisposition.Admitted,
        TrialDisposition.Quarantined(QuarantineCause.DuplicateOrdinals),
        TrialDisposition.NoFixations,
        TrialDisposition.Absent
      ).map(TrialLoss.of(_).map(_.label)),
      Vector(None, Some("duplicate-ordinals"), Some("no admitted fixations"), Some("absent"))
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
