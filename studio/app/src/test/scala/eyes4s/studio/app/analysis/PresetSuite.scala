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
  TrialDisposition as CoreDisposition,
  TrialIdentity,
  TrialOccurrence,
  TrialKey as CoreKey,
  UnmatchedFocalPolicy,
  UnmatchedKind,
  DeclaredReference,
  QuarantineCause
}
import eyes4s.studio.app.text.UnmatchedText
import eyes4s.studio.app.{AppModel, Intent, Notice, StoryModels}
import eyes4s.studio.core.backend.Phase
import eyes4s.studio.core.command.{Command, CommandError, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.preset.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

import scala.annotation.tailrec

/** Recipe presets (ticket S7.1; Analysis.dc.html, recipe): three presets fill
  * the one eyes4s fixation-study recipe (layout, phases and, for Recognition,
  * the unmatched policy); choosing one changes only those fields, as one
  * undoable edit whose plan.diff shows them; Save & run records the preset the
  * recipe holds; and a lure or novel probe never receives a match: eyes4s
  * leaves it unmatched, decides why against the inventory, and the studio only
  * words eyes4s's kind.
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
    // The match layout is eyes4s's trial-keyed layout: participant and item.
    presets.foreach(p =>
      assertEquals(p.layout, DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout))
    )
    // Lures are unmatched by design: the Recognition preset must report them.
    assertEquals(RecipePresets.recognition.unmatched, Some(UnmatchedFocalPolicy.ReportNoMatch))
    assertEquals(
      presets.map(_.declared),
      Vector(
        Vector(RecipeField.Layout, RecipeField.Phases),
        Vector(RecipeField.Layout, RecipeField.Phases),
        Vector(RecipeField.Layout, RecipeField.Phases, RecipeField.UnmatchedFocal)
      )
    )
    // The story's recipe is held by its preset, layout included.
    assert(
      RecipePresets.encodingRetrieval.holds(
        ok(StoryModels.t2.latestAnalysis.toRight("none")).recipe
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
    // One preset choice is one undoable edit, however many fields it sets.
    def done(model: AppModel) = model.history.stack(HistoryStack.Science).done.size
    assertEquals(done(r2) - done(r), 1)
    val (undone, _) = AppModel.update(r2, Intent.Undo(HistoryStack.Science))
    assertEquals(undone.document.draft, r.document.draft)
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

  test("a preset edit that is refused changes nothing, and says why") {
    val t2     = AppModel.open(StoryModels.t2, Some(StoryModels.project))
    val recipe = ok(t2.document.draftRecipe.toRight("no draft"))
    // The second change starts from a value the draft does not hold.
    val stale = Command.ChangeRecipes(
      Vector(
        RecipeChange.Phases(recipe.phases, RecipePresets.recognition.phases),
        RecipeChange.Unmatched(UnmatchedChoice.Refuse, UnmatchedChoice.ReportNoMatch)
      )
    )
    val (m, effects) = AppModel.update(t2, Intent.Dispatch(stale))
    assertEquals(m.document, t2.document)
    assertEquals(effects, Vector.empty)
    assert(
      m.notice.exists {
        case Notice.Refused(_, CommandError.StaleChange(RecipeField.UnmatchedFocal, _, _)) =>
          true
        case _ => false
      },
      m.notice
    )
  }

  test("Save & run refuses a preset the recipe does not hold, and keeps an unedited one") {
    val t2     = AppModel.open(StoryModels.t2, Some(StoryModels.project))
    val base   = ok(t2.document.latestAnalysis.toRight("no base"))
    val held   = base.studio.copy(preset = Preset.Recognition)
    val (r, _) = AppModel.update(t2, Intent.Dispatch(Command.SaveAndRun(Some(held))))
    assertEquals(r.document, t2.document)
    assert(
      r.notice.exists {
        case Notice.Refused(_, CommandError.PresetNotHeld(Preset.Recognition, _)) => true
        case _                                                                    => false
      },
      r.notice
    )
    // A revision saved as Custom whose next draft only adds a scale stays Custom:
    // the preset is resolved again only when a draft changes a declared field.
    val custom = base.studio.copy(preset = Preset.Custom)
    val (c, _) = AppModel.update(t2, Intent.Dispatch(Command.SaveAndRun(Some(custom))))
    val saved  = ok(c.document.latestAnalysis.toRight("nothing saved"))
    assertEquals(saved.studio.preset, Preset.Custom)
    val grid = Command.StartDraft(
      saved.id,
      None,
      Vector(RecipeChange.Grid(saved.recipe.grid, ok(GridSize.of(32, 24))))
    )
    val (g, _) = AppModel.update(c, Intent.Dispatch(grid))
    val (s, _) = AppModel.update(g, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(ok(s.document.latestAnalysis.toRight("none")).studio.preset, Preset.Custom)
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
    val unmatched = report.warnings.collect { case StudyFinding.UnmatchedFocal(k, _) => k }
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

  test("the studio words eyes4s's unmatched kinds and decides none of them") {
    def trial(phase: String, label: String) =
      get(TrialIdentity.of("P03", phase, label, TrialOccurrence.first))
    // The reference phase is the recipe's, never a fixed word.
    assertEquals(
      UnmatchedText(UnmatchedKind.NoReferenceInDesign, "Study"),
      "No corresponding study trial (by design)"
    )
    assertEquals(
      UnmatchedText(UnmatchedKind.NoReferenceInDesign, "Perception"),
      "No corresponding perception trial (by design)"
    )
    assertEquals(
      UnmatchedText(
        UnmatchedKind.ReferenceNotAdmitted(
          Vector(
            DeclaredReference(
              trial("Encoding", "enc_11"),
              CoreDisposition.Quarantined(QuarantineCause.Overlap(3, "[0,10)", "[5,15)"))
            )
          )
        ),
        "Encoding"
      ),
      "No match · enc_11 quarantined (Overlap)"
    )
    assertEquals(
      UnmatchedText(
        UnmatchedKind.ReferenceNotPairable(Vector(trial("Encoding", "enc_02"))),
        "Encoding"
      ),
      "No match · enc_02 cannot be paired"
    )
    assertEquals(UnmatchedText(UnmatchedKind.Undetermined, "Encoding"), "No match")
  }
