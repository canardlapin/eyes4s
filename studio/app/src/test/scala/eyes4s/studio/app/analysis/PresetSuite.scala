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

package eyes4s.studio.app.analysis

import eyes4s.core.{Event, Scanpath, Weight}
import eyes4s.design.{FailurePolicy, PairCursor, PairPage, PairQuantum, Trial, Trials}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.{
  DefinitionId,
  RecipeFamily,
  StudyEstimate,
  StudyFinding,
  StudyGeometry,
  StudyInput,
  StudyMethod,
  StudyPairing,
  StudyPlan,
  StudyScale,
  TrialKeyDefinitions,
  TrialOccurrence,
  TrialKey as CoreKey,
  UnmatchedFocalPolicy
}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticOrigin,
  Eligibility,
  Phase,
  StudioDiagnostic,
  TrialDisposition,
  TrialKey
}
import eyes4s.studio.core.command.{Command, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.preset.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.annotation.tailrec

/** Recipe presets (ticket S7.1; Analysis.dc.html, recipe): three presets fill
  * the one eyes4s fixation-study recipe; choosing one changes only the fields
  * it declares, as the draft's plan.diff shows; Save & run records the preset
  * the recipe holds; and a lure or novel probe never receives a match: eyes4s
  * leaves it unmatched under the Recognition preset, and the preset reads it
  * as by design, refusing any match the inventory does not declare.
  */
class PresetSuite extends munit.ScalaCheckSuite:
  import StoryModels.ok

  private val presets = RecipePresets.all

  /** Recipes over every phase pair a preset declares, and others. */
  private val recipes: Gen[Recipe] =
    val phase = Gen.oneOf(
      presets.flatMap(p => Vector(p.phases.focal, p.phases.reference)) :+ Phase("Cue")
    )
    for
      r <- DocumentGen.recipe
      f <- phase
      g <- phase
    yield r.copy(phases = PhasePair(f, g))

  // --- The catalogue ------------------------------------------------------------------

  test("three presets fill the eyes4s fixation-study recipe with phases it accepts") {
    assertEquals(
      presets.map(_.preset),
      Vector(Preset.EncodingRetrieval, Preset.PerceptionImagery, Preset.Recognition)
    )
    presets.foreach { p =>
      assertEquals(p.family, RecipeFamily.FixationStudy)
      val core = ok(p.corePhases)
      assertEquals(
        (core.focal, core.reference),
        (p.phases.focal.label, p.phases.reference.label)
      )
    }
    assertEquals(
      presets.map(p => (p.phases.focal.label, p.phases.reference.label)),
      Vector(("Retrieval", "Encoding"), ("Imagery", "Perception"), ("Recognition", "Study"))
    )
    assertEquals(RecipePresets.of(Preset.Custom), None)
    assertEquals(
      presets.map(p => (p.queryDisplay, p.referenceDisplay)),
      Vector(
        (DisplayKind.BlankWithFixationCross, DisplayKind.Image),
        (DisplayKind.Blank, DisplayKind.Image),
        (DisplayKind.Image, DisplayKind.Image)
      )
    )
    // Lures are unmatched by design: the Recognition preset must report them.
    assertEquals(RecipePresets.recognition.unmatched, Some(UnmatchedFocalPolicy.ReportNoMatch))
    assertEquals(
      presets.map(_.declared),
      Vector(
        Vector(RecipeField.Phases),
        Vector(RecipeField.Phases),
        Vector(RecipeField.Phases, RecipeField.UnmatchedFocal)
      )
    )
  }

  test("the picker offers every preset in the board's words") {
    assertEquals(PresetPicker.offered.map(_._1.preset), presets.map(_.preset))
    val vm = PresetPicker.vm(StoryModels.t2)
    assertEquals(vm.heading, "Preset · sets defaults and wording; one engine underneath")
    assertEquals(
      vm.options.map(o => (o.title, o.detail)),
      Vector(
        "Encoding → Retrieval" -> "Query: Retrieval · Reference: Encoding · match on item",
        "Perception → Imagery" -> "Query: Imagery on blank · Reference: Perception",
        "Study → Recognition"  ->
          "Old probes only. Lures and novel: No corresponding study trial (by design)"
      )
    )
    assertEquals(vm.options.map(_.selected), Vector(true, false, false))
    assertEquals(vm.note, None)
    assertEquals(
      vm.options.map(_.changes),
      Vector(
        "no change",
        "changes phases Retrieval vs Encoding → Imagery vs Perception",
        "changes phases Retrieval vs Encoding → Recognition vs Study"
      )
    )
    assertEquals(
      vm.options.map(_.choose),
      Vector(
        None,
        Some(Intent.ChoosePreset(Preset.PerceptionImagery)),
        Some(Intent.ChoosePreset(Preset.Recognition))
      )
    )
    val empty = PresetPicker.vm(StoryModels.empty)
    assertEquals(empty.note, Some("No analysis revision to apply a preset to yet."))
    assert(empty.options.forall(o => o.choose.isEmpty && !o.selected))
  }

  // --- Switching changes only declared fields -----------------------------------------

  property("a preset's plan.diff names only its declared fields, and applying it is stable") {
    forAll(recipes, Gen.oneOf(presets)) { (recipe, p) =>
      val changes = p.changes(recipe)
      assert(changes.forall(c => p.declared.contains(c.field)), changes.map(_.render))
      assertEquals(changes.foldLeft(recipe)((r, c) => c.applyTo(r)), p.applyTo(recipe))
      assert(p.holds(p.applyTo(recipe)))
      assertEquals(p.changes(p.applyTo(recipe)), Vector.empty)
      assertEquals(RecipePresets.resolve(Preset.Custom, p.applyTo(recipe)), p.preset)
    }
  }

  property("switching presets keeps every field the target does not declare") {
    forAll(recipes, Gen.oneOf(presets), Gen.oneOf(presets)) { (recipe, from, to) =>
      val before = from.applyTo(recipe)
      val after  = to.applyTo(before)
      RecipeChange.between(before, after).foreach(c => assert(to.declared.contains(c.field)))
      assertEquals(RecipePresets.resolve(from.preset, after), to.preset)
    }
  }

  property("a recipe whose phases no preset declares resolves to Custom") {
    forAll(DocumentGen.recipe, Gen.oneOf(Preset.values.toSeq)) { (recipe, current) =>
      val odd = recipe.copy(phases = PhasePair(Phase("Cue"), Phase("Cue")))
      assertEquals(RecipePresets.resolve(current, odd), Preset.Custom)
    }
  }

  test("choosing a preset on a draft adds its declared fields to the draft's plan.diff") {
    val t2     = AppModel.open(StoryModels.t2, Some(StoryModels.project))
    val draft  = ok(t2.document.draft.toRight("no draft"))
    val base   = ok(t2.document.analysis(draft.base).toRight("no base"))
    val (m, _) = AppModel.update(t2, Intent.ChoosePreset(Preset.PerceptionImagery))
    assertEquals(m.notice, None)
    val after = ok(m.document.draft.toRight("draft gone"))
    assertEquals(after.id, draft.id)
    assertEquals(after.base, draft.base)
    val diff = RecipeChange.between(base.recipe, ok(m.document.draftRecipe.toRight("none")))
    assertEquals(diff.map(_.field), Vector(RecipeField.Phases, RecipeField.Scales))
    assertEquals(
      diff.map(_.render),
      Vector("phases Retrieval vs Encoding → Imagery vs Perception", "scales +σ 8°")
    )
    assertEquals(PresetPicker.selected(m.document), Some(Preset.PerceptionImagery))
    // Choosing the held preset again changes nothing.
    assertEquals(AppModel.update(m, Intent.ChoosePreset(Preset.PerceptionImagery))._1, m)
    // Custom is not a choice: a recipe becomes custom by its fields.
    assertEquals(AppModel.update(m, Intent.ChoosePreset(Preset.Custom))._1, m)
    // Choosing back returns the phases and leaves the scales edit alone.
    val (back, _) = AppModel.update(m, Intent.ChoosePreset(Preset.EncodingRetrieval))
    assertEquals(back.document.draft, t2.document.draft)
  }

  test("without a draft, choosing a preset starts one of the latest revision") {
    val t2        = AppModel.open(StoryModels.t2, Some(StoryModels.project))
    val (none, _) = AppModel.update(t2, Intent.Dispatch(Command.DiscardDraft))
    assertEquals(none.document.draft, None)
    val latest = ok(none.document.latestAnalysis.toRight("no analysis"))
    // An analyst who refused unmatched queries: Recognition reports them.
    val refusing = Command.StartDraft(
      latest.id,
      None,
      Vector(RecipeChange.Unmatched(latest.recipe.unmatched, UnmatchedChoice.Refuse))
    )
    val (r, _)   = AppModel.update(none, Intent.Dispatch(refusing))
    val (m, _)   = AppModel.update(none, Intent.ChoosePreset(Preset.Recognition))
    val recipe   = ok(m.document.draftRecipe.toRight("no draft"))
    val declared = RecipeChange.between(latest.recipe, recipe)
    assertEquals(declared.map(_.field), Vector(RecipeField.Phases))
    assertEquals(recipe.phases, PhasePair(Phase("Recognition"), Phase("Study")))
    val (r2, _) = AppModel.update(r, Intent.ChoosePreset(Preset.Recognition))
    val held    = ok(r2.document.draftRecipe.toRight("no draft"))
    assertEquals(held.unmatched, UnmatchedChoice.ReportNoMatch)
    assertEquals(
      RecipeChange.between(latest.recipe, held).map(_.field),
      Vector(RecipeField.Phases)
    )
    // One undo step per declared field changed on the draft.
    def done(model: AppModel) = model.history.stack(HistoryStack.Science).done.size
    assertEquals(done(r2) - done(r), 2)
  }

  test("Save & run records the preset the saved recipe holds, keeping name and description") {
    val t2     = AppModel.open(StoryModels.t2, Some(StoryModels.project))
    val (m, _) = AppModel.update(t2, Intent.ChoosePreset(Preset.Recognition))
    val (s, _) = AppModel.update(m, Intent.Dispatch(Command.SaveAndRun(None)))
    val saved  = ok(s.document.latestAnalysis.toRight("nothing saved"))
    val base   = ok(t2.document.latestAnalysis.toRight("no base"))
    assertEquals(saved.studio, base.studio.copy(preset = Preset.Recognition))
    // Unchanged phases keep the base's preset.
    val (k, _) = AppModel.update(t2, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(ok(k.document.latestAnalysis.toRight("none")).studio, base.studio)
  }

  // --- Lures and novel probes never receive invented matches ----------------------------

  private def get[E, A](e: Either[E, A]): A = e.fold(err => fail(s"$err"), identity)

  private val frame = get(Frame.screen("recognition", 4, 4))
  private val grid  = get(Grid.over(frame, 2, 2))

  private def key(phase: String, trial: String, item: String): CoreKey =
    get(CoreKey.of("P01", phase, trial, TrialOccurrence.first, item))

  private def path(k: CoreKey, x: Double): Scanpath[Px] =
    val clock = ClockId(s"${k.phase}/${k.trial}")
    get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.withoutDispersion(
              get(Interval.of(clock, Instant.micros(0), Instant.micros(100))),
              Pt[Px](x, 1.5),
              1
            )
          )
        )
      )
    )

  test("under the Recognition preset eyes4s matches old probes only; lures stay unmatched") {
    val p      = RecipePresets.recognition
    val phases = get(p.corePhases)
    val study  = phases.reference
    val probe  = phases.focal
    // Two studied scenes; an old probe of each, a lure resembling a studied
    // scene (its own item) and a novel scene.
    val studyA = key(study, "s1", "beach-042")
    val studyB = key(study, "s2", "bridge-019")
    val oldA   = key(probe, "r1", "beach-042")
    val oldB   = key(probe, "r2", "bridge-019")
    val lure   = key(probe, "r3", "beach-042-lure")
    val novel  = key(probe, "r4", "market-066")
    val keys   = Vector(studyA, studyB, oldA, oldB, lure, novel)
    val input  = StudyInput(
      Trials(keys.zipWithIndex.map((k, i) => Trial(k, (), path(k, 0.5 + (i % 4)))))
    )
    val pairing = StudyPairing.default.copy(
      unmatched = p.unmatched.getOrElse(StudyPairing.default.unmatched)
    )
    val plan = get(
      StudyPlan.configure(
        input.reference,
        CoreKey.layout(TrialKeyDefinitions.trialLayout),
        StudyGeometry.WholeFrame(grid),
        probe,
        study,
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned())),
        None,
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        pairing
      )
    )
    val report = plan.preflight(Some(input))
    assertEquals(report.blockers, Vector.empty)
    val unmatched = report.warnings.collect { case StudyFinding.UnmatchedFocal(k) => k }
    assertEquals(unmatched.toSet, Set(lure, novel))
    val work = get(plan.prepare(input))
    @tailrec
    def pairs(
        cursor: PairCursor[CoreKey, CoreKey],
        acc: Vector[(CoreKey, CoreKey)]
    ): Vector[(CoreKey, CoreKey)] =
      get(cursor.advance(PairQuantum.default)) match
        case PairPage.More(ps, _, next) =>
          pairs(next, acc ++ ps.map(q => q.left -> q.right))
        case PairPage.Done(ps, _, _) => acc ++ ps.map(q => q.left -> q.right)
    assertEquals(
      pairs(work.matched.start, Vector.empty).toSet,
      Set(oldA -> studyA, oldB -> studyB)
    )
  }

  private val diagnostic = StudioDiagnostic(
    "study-finding.unmatched-focal",
    DiagnosticLevel.Warning,
    DiagnosticOrigin.EyesCore,
    Vector.empty,
    ""
  )
  private val probe      = TrialKey("P01", Phase("Recognition"), "r3", 1)
  private val studyTrial = TrialKey("P01", Phase("Study"), "s1", 1)

  test("Recognition reads an unmatched probe without a declared study trial as by design") {
    val r = RecipePresets.recognition
    def at(e: Eligibility, m: Option[TrialKey], ref: InventoryReference) =
      r.categorize(probe, e, m, ref)
    assertEquals(
      at(Eligibility.NoMatch(diagnostic), None, InventoryReference.Undeclared),
      Right(QueryCategory.ByDesign)
    )
    // A declared study trial that admission dropped is a finding, not design.
    assertEquals(
      at(Eligibility.NoMatch(diagnostic), None, InventoryReference.Declared),
      Right(QueryCategory.NoMatch(diagnostic))
    )
    val notAdmitted = Eligibility.QueryNotAdmitted(TrialDisposition.Absent)
    assertEquals(
      at(notAdmitted, None, InventoryReference.Undeclared),
      Right(QueryCategory.NotAdmitted(TrialDisposition.Absent))
    )
    assertEquals(
      at(Eligibility.Eligible, Some(studyTrial), InventoryReference.Declared),
      Right(QueryCategory.Eligible(studyTrial))
    )
    // Encoding → retrieval has no by-design category: every one is a finding.
    assertEquals(
      RecipePresets.encodingRetrieval.categorize(
        probe,
        Eligibility.NoMatch(diagnostic),
        None,
        InventoryReference.Undeclared
      ),
      Right(QueryCategory.NoMatch(diagnostic))
    )
  }

  property("no preset ever shows a match the inventory does not declare") {
    val eligibility = Gen.oneOf(
      Eligibility.Eligible,
      Eligibility.NoMatch(diagnostic),
      Eligibility.QueryNotAdmitted(TrialDisposition.NoFixations)
    )
    forAll(Gen.oneOf(presets), eligibility, Gen.option(Gen.const(studyTrial))) { (p, e, m) =>
      p.categorize(probe, e, m, InventoryReference.Undeclared) match
        case Right(QueryCategory.Eligible(_)) =>
          fail(s"${p.preset} matched an undeclared probe")
        case Left(error) =>
          assertEquals(e, Eligibility.Eligible)
          assert(error.message.contains(probe.label), error.message)
        case Right(_) => ()
    }
  }

  test("an undeclared match is refused, naming the query and the reference") {
    assertEquals(
      RecipePresets.recognition.categorize(
        probe,
        Eligibility.Eligible,
        Some(studyTrial),
        InventoryReference.Undeclared
      ),
      Left(PresetError.UndeclaredMatch(Preset.Recognition, probe, studyTrial))
    )
    assertEquals(
      PresetError.UndeclaredMatch(Preset.Recognition, probe, studyTrial).message,
      "Query P01 · r3 (Recognition) is matched to P01 · s1 (Study) under the Recognition " +
        "preset, but the trial inventory declares no Study trial of its item for that participant."
    )
  }
