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

package eyes4s.studio.app.geometry

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.assets.AssetFile
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{FakePlacement, GoldenAssets, MockStudy, StoryMoments}
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.core.selection.{RecordNumber, StudioRef, TallyRegion}

import java.nio.charset.StandardCharsets.UTF_8

/** The geometry panel's behaviour, headless (ticket S5.5): every change is
  * one "Dataset · re-admit" edit that edits a pending revision or creates a
  * new pending one from an admitted revision; the off-screen policy is such
  * a change; a marked orientation is recorded as a ledger rule and never
  * touches the source; the outside-window and outside-screen counts are
  * shown apart from eyes4s's window totals.
  */
class GeometryPanelSuite extends munit.FunSuite:
  import StoryModels.{ok, play}
  import StoryMoments.{r2, r3}

  private val r4 = DatasetRevision(4)

  private def opened(document: StudioDocument): AppModel =
    AppModel.open(document, Some(StoryModels.project))

  /** t1: r3 is a pending re-import of r2. t2: r3 is admitted. */
  private def t1: AppModel = opened(StoryModels.t1)
  private def t2: AppModel = opened(StoryModels.t2)

  private def synced(model: AppModel): (GeometryPanel, Vector[GeometryEffect]) =
    GeometryPanel.sync(GeometryPanel.empty, model)

  /** Apply the panel's app intents to the model, as the host does. */
  private def perform(model: AppModel, effects: Vector[GeometryEffect]): AppModel =
    effects.foldLeft(model) {
      case (m, GeometryEffect.App(i)) => AppModel.update(m, i)._1
      case (m, _)                     => m
    }

  private def commandsOf(effects: Vector[GeometryEffect]): Vector[Command] =
    effects.collect { case GeometryEffect.App(Intent.Dispatch(c)) => c }

  /** eyes4s's window totals as the fake backend serves them for r3. */
  private lazy val summary: AdmissionSummary =
    val s = ok(MockStudy.load).summary
    AdmissionSummary(
      r3,
      DatasetState.Draft,
      // The geometry panel reads only the window totals.
      InventoryJoin.Undeclared,
      s.admitted,
      Vector.empty,
      0,
      s.fixationRecords,
      WindowTotals(
        outsideWindow = s.outsideWindowRecords,
        outsideScreen = 0,
        total = 11311,
        trialsOutsideWindow = s.outsideWindowTrials,
        trialsOutsideScreen = 0,
        trials = s.admitted,
        untallied = 0,
        sourceRecords = Some(s.fixationRecords),
        outsideWindowMicros = 0L,
        outsideScreenMicros = 0L,
        totalMicros = 1L
      ),
      s.itemsInPool,
      s.imagesFound,
      Vector.empty,
      "",
      0,
      None
    )

  test("the panel follows the Data selection and asks for its records and counts") {
    val (panel, effects) = synced(t1)
    assertEquals(panel.shown.map(_.id), Some(r3))
    assertEquals(panel.fields.field(GeometryField.ImageLeft), "448")
    assertEquals(
      effects.map(_.productPrefix),
      Vector("RequestPlacement", "RequestCounts", "RequestDisplays")
    )
    assertEquals(effects(1), GeometryEffect.RequestCounts(r3))
    assertEquals(effects.last, GeometryEffect.RequestDisplays(panel.shown.get))
    // r2 shares r3's source file but not its mapping: its records are read again.
    val onR2 =
      play(t1, _ => Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(r2)))))
    val (moved, again) = GeometryPanel.sync(panel, onR2)
    assertEquals(moved.shown.map(_.id), Some(r2))
    assertEquals(
      again.map(_.productPrefix),
      Vector("RequestPlacement", "RequestCounts", "RequestDisplays")
    )
  }

  test(
    "outside window and outside screen are counted apart: 543 of 11,520 records · 409 trials"
  ) {
    val (panel, _) = synced(t1)
    val loaded     =
      GeometryPanel.update(panel, t1, GeometryIntent.CountsRead(r3, Right(summary)))._1
    val vm = GeometryPanelVM.of(loaded, t1, None)
    assertEquals(vm.outsideWindow.title, "Outside image frame")
    assertEquals(vm.outsideWindow.value, Some("543 of 11,520 records · 409 trials"))
    assertEquals(vm.outsideScreen.title, "Outside screen")
    assertEquals(vm.outsideScreen.value, Some("0 of 11,520 records · 0 trials"))
    assert(vm.outsideScreen.note.contains("not a quarantine"), vm.outsideScreen.note)
    assertEquals(vm.countsSource, "Counts: eyes4s admission of r3.")
    assertEquals(vm.kind, "Dataset · re-admit")
    // Each count traces to the eyes4s tally it shows.
    assertEquals(
      (vm.outsideWindow.ref, vm.outsideScreen.ref),
      (
        Some(StudioRef.WindowTally(r3, TallyRegion.OutsideWindow)),
        Some(StudioRef.WindowTally(r3, TallyRegion.OutsideScreen))
      )
    )
    assertEquals(GeometryPanelVM.of(panel, t1, None).outsideWindow.ref, None)
  }

  test("a count opens the eyes4s tally it shows, as the admission ledger's counts do") {
    val (panel, _)      = synced(t1)
    val ref             = StudioRef.WindowTally(r3, TallyRegion.OutsideWindow)
    val (same, effects) = GeometryPanel.update(panel, t1, GeometryIntent.OpenCount(ref))
    assertEquals(same, panel)
    assertEquals(effects, Vector(GeometryEffect.App(Intent.Explain(Place.At(ref)))))
    val opened = perform(t1, effects)
    assertEquals(opened.location.trail.lastOption, Some(Place.At(ref)))
  }

  test("switching the off-screen policy edits the pending draft r3 in one undoable step") {
    val (panel, _)       = synced(t1)
    val (after, effects) =
      GeometryPanel.update(
        panel,
        t1,
        GeometryIntent.ChooseOffScreen(OffScreenChoice.QuarantineTrial)
      )
    assertEquals(
      commandsOf(effects),
      Vector(Command.SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial))
    )
    assertEquals(after.problem, None)
    val model = perform(t1, effects)
    val spec  = model.document.dataset(r3).get
    assertEquals(spec.admission.offScreen, OffScreenChoice.QuarantineTrial)
    assertEquals(spec.decision, AdmissionDecision.Pending)
    assertEquals(model.document.datasets.map(_.id), Vector(r2, r3))
    // The policy note and the outside-screen note follow the policy.
    val (shown, _) = GeometryPanel.sync(after, model)
    val vm         = GeometryPanelVM.of(shown, model, None)
    assertEquals(
      vm.policies.filter(_.selected).map(_.value),
      Vector(OffScreenChoice.QuarantineTrial)
    )
    assert(vm.outsideScreen.note.contains("quarantined"), vm.outsideScreen.note)
    val undone = AppModel.update(model, Intent.Undo(HistoryStack.Science))._1
    assertEquals(undone.document, t1.document)
    // Choosing the policy the revision has is not a change.
    assertEquals(
      GeometryPanel
        .update(panel, t1, GeometryIntent.ChooseOffScreen(OffScreenChoice.ExcludeRecord))
        ._2,
      Vector.empty
    )
  }

  test("switching the policy of admitted r3 creates draft r4, and the selection follows it") {
    val (panel, _)   = synced(t2)
    val (_, effects) =
      GeometryPanel.update(
        panel,
        t2,
        GeometryIntent.ChooseOffScreen(OffScreenChoice.QuarantineTrial)
      )
    val r3spec = t2.document.dataset(r3).get
    assertEquals(
      commandsOf(effects),
      Vector(
        Command.ImportSources(
          Some(r3),
          r3spec.sources,
          r3spec.mapping,
          r3spec.units,
          r3spec.geometry,
          r3spec.attributes,
          Some(r3spec.admission.copy(offScreen = OffScreenChoice.QuarantineTrial)),
          r3spec.inventory
        )
      )
    )
    val model = perform(t2, effects)
    // One command, so one undo removes the draft.
    assertEquals(
      AppModel.update(model, Intent.Undo(HistoryStack.Science))._1.document,
      t2.document
    )
    val draft = model.document.dataset(r4).get
    assertEquals(draft.decision, AdmissionDecision.Pending)
    assertEquals(draft.parent, Some(r3))
    assertEquals(draft.admission.offScreen, OffScreenChoice.QuarantineTrial)
    assertEquals(model.document.dataset(r3), Some(r3spec))
    val follow = GeometryPanel.follow(t2.document, model.document)
    assertEquals(
      follow,
      Some(Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(r4)))))
    )
    val followed = AppModel.update(model, follow.get)._1
    assertEquals(GeometryPanel.selected(followed).map(_.id), Some(r4))
    // The new draft shares r3's records; only its counts and its own
    // displays are asked for.
    val (onR4, asks) = GeometryPanel.sync(panel, followed)
    assertEquals(
      asks,
      Vector(
        GeometryEffect.RequestCounts(r4),
        GeometryEffect.RequestDisplays(followed.document.dataset(r4).get)
      )
    )
    // The fake backend has no r4: the panel falls back to its parent's counts, labelled.
    val refused =
      GeometryPanel.update(onR4, followed, GeometryIntent.CountsRead(r4, Left("No dataset r4")))
    assertEquals(refused._2, Vector(GeometryEffect.RequestCounts(r3)))
    val parent = GeometryPanel
      .update(refused._1, followed, GeometryIntent.CountsRead(r3, Right(summary)))
      ._1
    val vm = GeometryPanelVM.of(parent, followed, None)
    assertEquals(vm.outsideWindow.value, Some("543 of 11,520 records · 409 trials"))
    assertEquals(
      vm.countsSource,
      "Counts: eyes4s admission of r3. r4 is pending; its own counts follow its verification."
    )
    // The outside-screen note describes r3's counts, admitted under ExcludeRecord,
    // not the draft's QuarantineTrial.
    assert(vm.outsideScreen.note.contains("not a quarantine"), vm.outsideScreen.note)
    assert(vm.policyNote.contains("quarantin"), vm.policyNote)
    // The parent's counts trace to the parent, not to the draft.
    assertEquals(
      vm.outsideWindow.ref,
      Some(StudioRef.WindowTally(r3, TallyRegion.OutsideWindow))
    )
  }

  test("a geometry edit of pending r3 is S5.2's one ReviseDataset; one undo restores it") {
    val (panel, _) = synced(t1)
    val edited     = GeometryPanel
      .update(panel, t1, GeometryIntent.EditField(GeometryField.ImageTop, "150"))
      ._1
    val (_, effects) = GeometryPanel.update(edited, t1, GeometryIntent.CommitFields)
    val r3spec       = t1.document.dataset(r3).get
    val moved        = ok(ImagePlacement.of(448, 150, 1024, 768))
    val geometry     =
      ok(Geometry.of(r3spec.geometry.screen, moved, r3spec.geometry.pixelsPerDegree))
    assertEquals(
      commandsOf(effects),
      Vector(
        Command.ReviseDataset(
          r3,
          r3spec.mapping,
          r3spec.units,
          geometry,
          r3spec.attributes,
          r3spec.inventory
        )
      )
    )
    val model = perform(t1, effects)
    assertEquals(model.document.dataset(r3).map(_.geometry), Some(geometry))
    assertEquals(
      AppModel.update(model, Intent.Undo(HistoryStack.Science))._1.document,
      t1.document
    )
  }

  test("a geometry edit of admitted r3 re-imports it as r4 with the new geometry") {
    val (panel, _) = synced(t2)
    val edited     = GeometryPanel
      .update(panel, t2, GeometryIntent.EditField(GeometryField.PixelsPerDegree, "36"))
      ._1
    val (_, effects) = GeometryPanel.update(edited, t2, GeometryIntent.CommitFields)
    val model        = perform(t2, effects)
    assertEquals(commandsOf(effects).map(_.name), Vector("ImportSources"))
    assertEquals(model.document.dataset(r4).map(_.geometry.pixelsPerDegree.value), Some(36.0))
    assertEquals(model.document.dataset(r4).map(_.decision), Some(AdmissionDecision.Pending))
  }

  test("a geometry edit and a policy re-admit both keep the trials.csv inventory mapping") {
    // Pending r3: the geometry edit revises r3 in place.
    val pending = t1.document.dataset(r3).get
    assert(pending.inventory.isDefined, "the story's r3 maps no inventory")
    val (p1, _) = synced(t1)
    val typed   = GeometryPanel
      .update(p1, t1, GeometryIntent.EditField(GeometryField.ImageTop, "150"))
      ._1
    val edited = perform(t1, GeometryPanel.update(typed, t1, GeometryIntent.CommitFields)._2)
    assertEquals(edited.document.dataset(r3).map(_.geometry.image.top), Some(150))
    assertEquals(edited.document.dataset(r3).flatMap(_.inventory), pending.inventory)
    // Admitted r3: the policy change re-admits as r4, which keeps r3's mapping.
    val admitted = t2.document.dataset(r3).get
    val (p2, _)  = synced(t2)
    val readmit  = GeometryPanel
      .update(
        p2,
        t2,
        GeometryIntent.ChooseOffScreen(OffScreenChoice.QuarantineTrial)
      )
      ._2
    val readmitted = perform(t2, readmit)
    val r4spec     = readmitted.document.dataset(r4).get
    assertEquals(r4spec.admission.offScreen, OffScreenChoice.QuarantineTrial)
    assert(admitted.inventory.isDefined, "the story's admitted r3 maps no inventory")
    assertEquals(r4spec.inventory, admitted.inventory)
  }

  test("a mistyped field names itself and changes nothing") {
    val (panel, _) = synced(t1)
    val edited     = GeometryPanel
      .update(panel, t1, GeometryIntent.EditField(GeometryField.ScreenWidth, "wide"))
      ._1
    val (after, effects) = GeometryPanel.update(edited, t1, GeometryIntent.CommitFields)
    assertEquals(effects, Vector.empty)
    assertEquals(
      after.problem,
      Some("The screen width 'wide' is not a whole number of pixels.")
    )
    // Unchanged fields commit nothing.
    assertEquals(GeometryPanel.update(panel, t1, GeometryIntent.CommitFields)._2, Vector.empty)
  }

  private val p05ret04 = TrialKey("P05", Phase.Retrieval, "ret_04", 1)

  test("marking a trial as wrong orientation records a ledger rule; the sources never change") {
    val (panel, _) = synced(t1)
    val marked     = GeometryPanel.update(panel, t1, GeometryIntent.MarkTrial(p05ret04))._1
    assert(GeometryPanelVM.of(marked, t1, None).canMark)
    val opened = GeometryPanel.update(marked, t1, GeometryIntent.OpenOrientation)._1
    assertEquals(opened.orientation.map(_.trial), Some(p05ret04))
    val vm = GeometryPanelVM.of(opened, t1, None).orientation.get
    assertEquals(vm.title, "Mark P05 · ret_04 as wrong orientation")
    assertEquals(vm.scopes.map(_.label), Vector("This trial", "Every trial of P05"))
    val fixed =
      GeometryPanel.update(opened, t1, GeometryIntent.ChooseFix(OrientationFix.FlipY))._1
    val (done, effects) = GeometryPanel.update(fixed, t1, GeometryIntent.RecordOrientation)
    val rule = CorrectionRule(CorrectionTarget.Trial(p05ret04), CoordinateCorrection.FlipY)
    assertEquals(commandsOf(effects), Vector(Command.AddCorrection(r3, 0, rule)))
    assertEquals(done.orientation, None)
    val model = perform(t1, effects)
    val spec  = model.document.dataset(r3).get
    assertEquals(spec.admission.corrections, Vector(rule))
    assertEquals(spec.sources, t1.document.dataset(r3).get.sources)
    val (shown, _) = GeometryPanel.sync(done, model)
    assertEquals(
      GeometryPanelVM.of(shown, model, None).rules.map(_.text),
      Vector("1 · flip vertically · P05 · ret_04")
    )
    // Removing it is the inverse edit.
    val (_, removal) = GeometryPanel.update(shown, model, GeometryIntent.RemoveRule(0))
    assertEquals(commandsOf(removal), Vector(Command.RemoveCorrection(r3, 0)))
  }

  test("a rule that would overlap a recorded one is refused, naming the trial and the rule") {
    val all   = CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipX)
    val model = perform(
      t1,
      Vector(GeometryEffect.App(Intent.Dispatch(Command.AddCorrection(r3, 0, all))))
    )
    val (panel, _) = synced(model)
    val opened     = model
    val form       = GeometryPanel
      .update(
        GeometryPanel.update(panel, opened, GeometryIntent.MarkTrial(p05ret04))._1,
        opened,
        GeometryIntent.OpenOrientation
      )
      ._1
    val (after, effects) = GeometryPanel.update(form, opened, GeometryIntent.RecordOrientation)
    assertEquals(effects, Vector.empty)
    assertEquals(
      after.problem,
      Some(
        "Not recorded: the new rule and rule 1 would both cover P05 · ret_04; eyes4s refuses " +
          "overlapping rules."
      )
    )
  }

  // --- pictures ----------------------------------------------------------------

  private val csv: String =
    """participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
      |P01,Encoding,enc_01,1,1,700,420,0,200,100
      |P01,Encoding,enc_01,1,2,980,500,210,200,100
      |P05,Retrieval,ret_04,1,1,260,120,0,200,100
      |P05,Retrieval,ret_04,1,2,1620,980,210,200,100
      |P17,Encoding,enc_03,1,1,1148,456,0,412,206
      |P09,Encoding,enc_01,1,1,2000,420,0,100,50
      |""".stripMargin
  private val bytes: IArray[Byte] = IArray.from(csv.getBytes(UTF_8))

  /** t1 with r3 reading the records above. */
  private lazy val small: AppModel =
    val r3spec = t1.document.dataset(r3).get
    val source = Source(
      SourceRole.Fixations,
      ok(SourcePath.of("inputs/fixations.csv")),
      ByteDigest.sha256(bytes),
      None
    )
    // Fixations only, so there is no trials.csv to map.
    val spec = r3spec.copy(sources = ok(Sources.of(Vector(source))), inventory = None)
    val doc  = ok(
      StudioDocument.of(
        Vector(t1.document.dataset(r2).get, spec),
        t1.document.analyses,
        t1.document.draft,
        t1.document.runs,
        t1.document.reporting,
        t1.document.figures,
        t1.document.presentation,
        t1.document.jobs
      )
    )
    opened(doc)

  private def loaded(model: AppModel): GeometryPanel =
    val (panel, effects) = synced(model)
    val (key, spec)      =
      effects.collectFirst { case GeometryEffect.RequestPlacement(k, s) => (k, s) }.get
    // The backend's answer, as one holding these records gives it.
    val read = eyes4s.studio.core.fixture.FakePlacement.place(spec, bytes).left.map(_.message)
    GeometryPanel.update(panel, model, GeometryIntent.PlacementRead(key, read))._1

  private def picture(panel: GeometryPanel, model: AppModel): GeometryPictures =
    val key = GeometryPictures.keyOf(panel, model).get
    GeometryPictures.of(
      key,
      model.document.dataset(key.dataset).get,
      panel.placement.toOption.get
    )

  test("thumbnails: the first trial, then the trials with most records outside the frame") {
    val panel = loaded(small)
    val pics  = picture(panel, small)
    val vm    = GeometryPanelVM.of(panel, small, Some(pics))
    // Two caption lines, as the board draws them: the trial, then its count
    // (review ticket 24), so a caption never wraps mid-phrase.
    assertEquals(
      vm.thumbnails.map(t => (t.name, t.count)),
      Vector(
        ("P01 · enc_01", "2 records inside"),
        ("P05 · ret_04", "2 of 2 outside"),
        ("P09 · enc_01", "1 of 1 off screen"),
        ("P17 · enc_03", "1 record inside")
      )
    )
    // The accessible names, in the singular for a trial of one record.
    assertEquals(
      vm.thumbnails.map(_.accessible),
      Vector(
        "P01 · enc_01: 2 records, 0 outside the image frame, 0 outside the screen",
        "P05 · ret_04: 2 records, 2 outside the image frame, 0 outside the screen",
        "P09 · enc_01: 1 record, 0 outside the image frame, 1 outside the screen",
        "P17 · enc_03: 1 record, 0 outside the image frame, 0 outside the screen"
      )
    )
    assertEquals(
      vm.thumbnails.map(_.ref).head,
      // The thumbnail's counts trace to the backend's tally of the trial.
      Some(StudioRef.TrialPlacementTally(r3, TrialKey("P01", Phase.Encoding, "enc_01", 1)))
    )
    assertEquals(vm.densityCaption.take(9), "6 records")
    assertEquals(pics.density.records, 6)
    // The off-screen record falls in no cell: five cells are drawn. Each
    // holds one record, so m = 1 and s = 4, and every one is at level 1
    // (DESIGN_SPEC section 15.1); the share floor drew them at level 4.
    assertEquals(pics.density.levels.count(_ > 0), 5)
    assertEquals(pics.density.levels.filter(_ > 0), Vector.fill(5)(1))
    assertEquals(pics.density.steps, DensitySteps.Default)
    // Pictures stay shown until their redraw; the marked trial shows at once.
    val marked = GeometryPanel.update(panel, small, GeometryIntent.MarkTrial(p05ret04))._1
    assertNotEquals(GeometryPictures.keyOf(marked, small), Some(pics.key))
    assertEquals(
      GeometryPanelVM.of(marked, small, Some(pics)).thumbnails.map(_.marked),
      Vector(false, true, false, false)
    )
    // Pictures of other records are never shown.
    val (other, _) = GeometryPanel.sync(GeometryPanel.empty, t1)
    assertEquals(GeometryPanelVM.of(other, t1, Some(pics)).thumbnails, Vector.empty)
  }

  test("the density key: its grid and four closed count ranges at the drawn levels") {
    val panel = loaded(small)
    val pics  = picture(panel, small)
    // s = 4 (DESIGN_SPEC section 15.1): the owner's closed ranges, not the
    // board's former "≥1, ≥4, ≥16, ≥64".
    assertEquals(
      GeometryPanelVM.of(panel, small, Some(pics)).densityKey,
      DensityKeyVM(
        "Records per cell, log steps · 64 × 36 screen cells",
        Vector("1–3", "4–15", "16–63", "≥64")
      )
    )
    // The labels follow the drawn picture's own steps: s = 256, with
    // thousands separators.
    val dense = pics.copy(density = pics.density.copy(steps = DensitySteps.anchored(300L)))
    assertEquals(
      GeometryPanelVM.of(panel, small, Some(dense)).densityKey.labels,
      Vector("1–255", "256–1,023", "1,024–4,095", "≥4,096")
    )
    // Before the pictures are drawn, the default levels over the grid placed.
    assertEquals(
      GeometryPanelVM.of(panel, small, None).densityKey,
      DensityKeyVM(
        "Records per cell, log steps · 64 × 36 screen cells",
        Vector("1–3", "4–15", "16–63", "≥64")
      )
    )
  }

  test("an empty study keeps the default density key") {
    val panel = loaded(small)
    val pics  = picture(panel, small)
    val none  = ok(PlacementDensityGrid.of(64, 36, Vector.fill(64 * 36)(0.0), 0))
    val empty = pics.copy(density = DensityPicture.of(none))
    assertEquals(empty.density.steps, DensitySteps.Default)
    assertEquals(
      GeometryPanelVM.of(panel, small, Some(empty)).densityKey,
      DensityKeyVM(
        "Records per cell, log steps · 64 × 36 screen cells",
        Vector("1–3", "4–15", "16–63", "≥64")
      )
    )
    // No placed records at all: dashes for the grid, still the default key.
    assertEquals(
      GeometryPanelVM.densityKey(None, DensitySteps.Default),
      DensityKeyVM(
        "Records per cell, log steps · — × — screen cells",
        Vector("1–3", "4–15", "16–63", "≥64")
      )
    )
  }

  test("records with no finite position are counted, one in the singular") {
    val panel                                = loaded(small)
    val ps                                   = panel.placement.toOption.get
    def withUnplaced(n: Int): Option[String] =
      val unplaced = Vector.tabulate(n)(i => UnplacedSourceRecord(100 + i, "x is not finite"))
      val preview  = PlacementPreview
        .of(r3, ps.records, unplaced, ps.trials, ps.density)
        .fold(e => fail(e.message), identity)
      val read = GeometryPanel
        .update(
          panel,
          small,
          GeometryIntent.PlacementRead(panel.placementKey.get, Right(preview))
        )
        ._1
      GeometryPanelVM.of(read, small, None).positionsNote
    assertEquals(withUnplaced(0), None)
    assertEquals(
      withUnplaced(1),
      Some("1 record has no finite position and is not drawn.")
    )
    assertEquals(
      withUnplaced(2),
      Some("2 records have no finite position and are not drawn.")
    )
  }

  test("a thumbnail's count line, and the missing-asset caption") {
    val trial = TrialKey("P01", Phase.Encoding, "enc_08", 1)
    def count(records: Int, window: Int, screen: Int): String =
      GeometryPanelVM.thumbCount(
        ThumbnailPicture(trial, records, window, screen, Vector.empty, FrameState.Present)
      )
    assertEquals(count(12, 0, 0), "12 records inside")
    assertEquals(count(1, 0, 0), "1 record inside")
    assertEquals(count(12, 1, 0), "1 of 12 outside")
    assertEquals(count(1_200, 1_100, 0), "1,100 of 1,200 outside")
    // Off screen is named before outside the frame.
    assertEquals(count(12, 3, 2), "2 of 12 off screen")
    assertEquals(GeometryPanelVM.trialLabel(trial), "P01 · enc_08")
    // A missing or unreadable asset is named before any count (the Data
    // board's caption, review ticket 25).
    val forest                            = ok(AssetFile.of("forest-044.png"))
    def framed(frame: FrameState): String =
      GeometryPanelVM.thumbCount(ThumbnailPicture(trial, 12, 3, 2, Vector.empty, frame))
    assertEquals(framed(FrameState.Missing(forest)), "Missing asset · forest-044.png")
    assertEquals(
      framed(FrameState.Unreadable(forest, "not a PNG")),
      "Unreadable asset · forest-044.png"
    )
  }

  test("the fixture's missing images are hatched thumbnails captioned by their files") {
    // t1's r3 reads the golden trials: 257 of 259 images, forest-044.png and
    // kitchen-081.png missing (P01 enc_08 and enc_15 among their trials).
    val (synced0, effects) = synced(t1)
    val spec = effects.collectFirst { case GeometryEffect.RequestDisplays(s) => s }.get
    val key  =
      effects.collectFirst { case GeometryEffect.RequestPlacement(k, _) => k }.get
    def read(panel: GeometryPanel, intent: GeometryIntent) =
      GeometryPanel.update(panel, t1, intent)._1
    val placed = read(
      synced0,
      GeometryIntent.PlacementRead(key, FakePlacement.of(spec).left.map(_.message))
    )
    val registry = GoldenAssets.registry(spec).fold(fail(_), identity)
    val shown    =
      read(placed, GeometryIntent.DisplaysRead(r3, Right(DisplaySource.Served(registry))))
    val p01enc08 = TrialKey("P01", Phase.Encoding, "enc_08", 1)
    val p01enc15 = TrialKey("P01", Phase.Encoding, "enc_15", 1)
    val frames   = GeometryPanel.frames(shown, t1)
    assertEquals(frames.size, 4)
    assertEquals(
      frames.get(p01enc08),
      Some(FrameState.Missing(ok(AssetFile.of("forest-044.png"))))
    )
    assertEquals(
      frames.get(p01enc15),
      Some(FrameState.Missing(ok(AssetFile.of("kitchen-081.png"))))
    )
    // Before the displays are read, no frame is missing and none is drawn hatched.
    assertEquals(GeometryPanel.frames(placed, t1), Map.empty)
    assert(
      picture(placed, t1).thumbnails.forall(_.frame == FrameState.Present),
      picture(placed, t1).thumbnails
    )
    // The first missing trial is a representative, captioned by its file.
    val vm      = GeometryPanelVM.of(shown, t1, Some(picture(shown, t1)))
    val missing =
      vm.thumbnails.find(_.trial == p01enc08).getOrElse(fail(vm.thumbnails.toString))
    assertEquals(
      (missing.name, missing.count),
      ("P01 · enc_08", "Missing asset · forest-044.png")
    )
    assert(missing.assetMissing)
    assert(
      missing.accessible.startsWith(
        "P01 · enc_08: missing asset, forest-044.png not found, field drawn hatched; "
      ),
      missing.accessible
    )
    assertEquals(vm.thumbnails.count(_.assetMissing), 1)
    // The marked trial is shown too: kitchen-081.png's.
    val marked  = read(shown, GeometryIntent.MarkTrial(p01enc15))
    val again   = GeometryPanelVM.of(marked, t1, Some(picture(marked, t1)))
    val kitchen =
      again.thumbnails.find(_.trial == p01enc15).getOrElse(fail(again.thumbnails.toString))
    assertEquals(kitchen.count, "Missing asset · kitchen-081.png")
    // A present frame keeps its records count and its plain accessible name.
    again.thumbnails.filterNot(_.assetMissing).foreach { t =>
      assert(!t.count.startsWith("Missing"), t.count)
      assert(!t.accessible.contains("missing asset"), t.accessible)
    }
    // Displays of another revision are not this one's.
    val other =
      read(placed, GeometryIntent.DisplaysRead(r2, Right(DisplaySource.Served(registry))))
    assertEquals(other.displays, placed.displays)
  }

  test("the worked example of the selected record: raw → image frame → degrees") {
    val record = StudioRef.SourceRecord(
      TrialKey("P17", Phase.Encoding, "enc_03", 1),
      None,
      SourceRole.Fixations,
      ok(RecordNumber.of(5))
    )
    val model = play(small, m => StoryModels.select(m, "data.geometry", record))
    val panel = loaded(model)
    val vm    = GeometryPanelVM.of(panel, model, Some(picture(panel, model)))
    assertEquals(
      vm.example.map(r => r.label -> r.value),
      Vector(
        "Record 5 raw"  -> "screen (1148, 456) px",
        "→ image frame" -> "(700, 300) px · (+5.4°, +2.4°)",
        "Degrees"       -> "from image centre · x right · y up"
      )
    )
    assertEquals(vm.exampleRef, Some(record))
  }

  test("a recorded flip moves the drawn marks and the example, never the raw record") {
    val flip  = CorrectionRule(CorrectionTarget.Trial(p05ret04), CoordinateCorrection.FlipX)
    val model = perform(
      small,
      Vector(GeometryEffect.App(Intent.Dispatch(Command.AddCorrection(r3, 0, flip))))
    )
    val panel =
      GeometryPanel.update(loaded(model), model, GeometryIntent.MarkTrial(p05ret04))._1
    val pics  = picture(panel, model)
    val thumb = pics.thumbnails.find(_.trial == p05ret04).get
    assertEquals(
      thumb.marks.map(m => (m.x, m.y, m.corrected)),
      Vector((1660.0, 120.0, true), (300.0, 980.0, true))
    )
    val vm = GeometryPanelVM.of(panel, model, Some(pics))
    assertEquals(
      vm.example.take(2).map(r => r.label -> r.value),
      Vector(
        "Record 3 raw"         -> "screen (260, 120) px",
        "→ corrected (rule 1)" -> "screen (1660, 120) px · flip horizontally"
      )
    )
    assertEquals(
      panel.placement.toOption.get.record(3).map(p => (p.rawX, p.rawY)),
      Some((260.0, 120.0))
    )
  }

  test("without its trial in the records read, an overlap is still refused, by the rules") {
    val p05 = CorrectionRule(
      CorrectionTarget.Participant(ok(ParticipantId.of("P05"))),
      CoordinateCorrection.FlipY
    )
    val model =
      perform(
        t1,
        Vector(GeometryEffect.App(Intent.Dispatch(Command.AddCorrection(r3, 0, p05))))
      )
    // The records are not read: no trial of the source is known.
    val (panel, _) = synced(model)
    val form       = Vector(
      GeometryIntent.MarkTrial(p05ret04),
      GeometryIntent.OpenOrientation,
      GeometryIntent.ChooseScope(OrientationScope.ThisParticipant)
    ).foldLeft(panel)((p, i) => GeometryPanel.update(p, model, i)._1)
    val (after, effects) = GeometryPanel.update(form, model, GeometryIntent.RecordOrientation)
    assertEquals(effects, Vector.empty)
    assert(after.problem.exists(_.contains("rules 1 and 2 both cover a trial")), after.problem)
  }

  test(
    "a geometry or rule change asks the backend to place the records again; the pictures stay meanwhile"
  ) {
    val panel = loaded(small)
    val pics  = picture(panel, small)
    // The off-screen policy decides admission, not placement: nothing is asked.
    val policy = perform(
      small,
      Vector(
        GeometryEffect.App(
          Intent.Dispatch(Command.SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial))
        )
      )
    )
    val (samePlacement, none) = GeometryPanel.sync(panel, policy)
    assertEquals(none.collect { case e: GeometryEffect.RequestPlacement => e }, Vector.empty)
    assertEquals(samePlacement.placement, panel.placement)
    // A recorded rule changes where eyes4s places them: asked again, with the rule.
    val flip  = CorrectionRule(CorrectionTarget.Trial(p05ret04), CoordinateCorrection.FlipX)
    val ruled = perform(
      small,
      Vector(GeometryEffect.App(Intent.Dispatch(Command.AddCorrection(r3, 0, flip))))
    )
    val (waiting, asks) = GeometryPanel.sync(panel, ruled)
    assertEquals(
      asks.collect { case GeometryEffect.RequestPlacement(_, s) => s.admission.corrections },
      Vector(Vector(flip))
    )
    assertEquals(waiting.placement, Loading.Waiting)
    // The last pictures stay shown (same records) until the new placement
    // arrives, said to be the previous placement's; their counts cite no tally
    // of r3, which the rule edited in place.
    val stale = GeometryPanelVM.of(waiting, ruled, Some(pics))
    assertEquals(stale.thumbnails.size, pics.thumbnails.size)
    assertEquals(stale.thumbnails.map(_.ref).distinct, Vector(None))
    assertEquals(
      stale.positionsNote,
      Some(
        "Drawn for the previous rules or geometry; inputs/fixations.csv is being placed again " +
          "under the new ones."
      )
    )
    // Before any change, the same pictures cite the tallies and say nothing.
    val fresh = GeometryPanelVM.of(panel, small, Some(pics))
    assert(fresh.thumbnails.forall(_.ref.nonEmpty), fresh.thumbnails)
    assertEquals(fresh.positionsNote, None)
    // A refused placement blanks the pictures and says why.
    val refused = GeometryPanel
      .update(
        waiting,
        ruled,
        GeometryIntent
          .PlacementRead(waiting.placementKey.get, Left("the backend holds another file"))
      )
      ._1
    assertEquals(refused.placement, Loading.Failed("the backend holds another file"))
    // A preview of another revision is not this one's placement.
    val stranger = GeometryPanel
      .update(
        waiting,
        ruled,
        GeometryIntent.PlacementRead(
          waiting.placementKey.get,
          panel.placement.toOption
            .map(p =>
              eyes4s.studio.core.backend.PlacementPreview
                .of(r2, p.records, p.unplaced, p.trials, p.density)
                .fold(e => fail(e.message), identity)
            )
            .toRight("no preview")
        )
      )
      ._1
    assertEquals(
      stranger.placement,
      Loading.Failed("the backend placed the records of another revision than r3")
    )
    val blank = GeometryPanelVM.of(refused, ruled, Some(pics))
    assertEquals(
      (blank.thumbnails, blank.example, blank.exampleRef),
      (Vector.empty, Vector.empty, None)
    )
    assertEquals(
      blank.positionsNote,
      Some("Fixation positions are not available: the backend holds another file")
    )
    // A geometry edit also changes where eyes4s places them: asked again.
    val edited = GeometryPanel
      .update(panel, small, GeometryIntent.EditField(GeometryField.ImageLeft, "450"))
      ._1
    val moved =
      perform(small, GeometryPanel.update(edited, small, GeometryIntent.CommitFields)._2)
    val (moving, more) = GeometryPanel.sync(panel, moved)
    assertEquals(
      more.collect { case GeometryEffect.RequestPlacement(_, s) => s.geometry.image.left },
      Vector(450)
    )
    assertEquals(moving.placement, Loading.Waiting)
  }

  test("the panel's focus stops: none without a revision, then its controls in Tab order") {
    import eyes4s.studio.app.vm.{A11yRole, FocusStop}
    val nothing = GeometryPanel.sync(GeometryPanel.empty, StoryModels.firstRun)._1
    assertEquals(
      GeometryPanelVM.focusStops(GeometryPanelVM.of(nothing, StoryModels.firstRun, None), true),
      Vector.empty
    )
    val (panel, _) = synced(t1)
    val counted    =
      GeometryPanel.update(panel, t1, GeometryIntent.CountsRead(r3, Right(summary)))._1
    val marked = GeometryPanel.update(counted, t1, GeometryIntent.MarkTrial(p05ret04))._1
    val vm     = GeometryPanelVM.of(marked, t1, None)
    val tail   = Vector(
      FocusStop(A11yRole.RadioButton, "Exclude the record (default)"),
      FocusStop(A11yRole.Button, "Outside image frame: 543 of 11,520 records · 409 trials"),
      FocusStop(A11yRole.Button, "Outside screen: 0 of 11,520 records · 0 trials")
    )
    val density = FocusStop(A11yRole.Plot, vm.densityAccessible)
    val mark    = FocusStop(A11yRole.Button, "Mark trial as wrong orientation…")
    // The window's draft form edits the fields: the panel's own are no stops.
    assertEquals(GeometryPanelVM.focusStops(vm, fields = false), Vector(density, mark) ++ tail)
    assertEquals(
      GeometryPanelVM.focusStops(vm, fields = true),
      vm.fields.map(f => FocusStop(A11yRole.TextField, f.label)) ++ Vector(
        density,
        mark
      ) ++ tail
    )
    // The open marking form: its chosen fix and scope, then its buttons.
    val opened = GeometryPanel.update(marked, t1, GeometryIntent.OpenOrientation)._1
    assertEquals(
      GeometryPanelVM.focusStops(GeometryPanelVM.of(opened, t1, None), fields = false),
      Vector(
        density,
        FocusStop(A11yRole.RadioButton, "Flip horizontally (x)"),
        FocusStop(A11yRole.RadioButton, "This trial"),
        FocusStop(A11yRole.Button, "Record correction"),
        FocusStop(A11yRole.Button, "Cancel")
      ) ++ tail
    )
  }

  test(
    "the drawn thumbnails are one list stop before the density, named as the board names it"
  ) {
    import eyes4s.studio.app.vm.{A11yRole, FocusStop}
    val panel = loaded(small)
    val vm    = GeometryPanelVM.of(panel, small, Some(picture(panel, small)))
    assert(vm.thumbnails.size > 1)
    val stops = GeometryPanelVM.focusStops(vm, fields = false)
    assertEquals(
      stops.take(2),
      Vector(
        FocusStop(A11yRole.List, vm.thumbnailsAccessible),
        FocusStop(A11yRole.Plot, vm.densityAccessible)
      )
    )
    assertEquals(stops.count(_.role == A11yRole.List), 1)
    assertEquals(stops.count(_.role == A11yRole.ToggleButton), 0)
    assertEquals(
      vm.thumbnailsAccessible,
      s"Placement check, ${vm.thumbnails.size} trials; arrow keys move, Enter or Space " +
        "chooses the trial to mark"
    )
    // No thumbnail drawn, no list stop.
    val none = GeometryPanelVM.of(panel, small, None)
    assertEquals(none.thumbnails, Vector.empty)
    assert(!GeometryPanelVM.focusStops(none, fields = false).exists(_.role == A11yRole.List))
  }

  test("the thumbnail strip's cursor: home on the marked thumbnail, else the first") {
    val panel  = loaded(small)
    val vm     = GeometryPanelVM.of(panel, small, Some(picture(panel, small)))
    val thumbs = vm.thumbnails
    assert(thumbs.size > 1)
    assertEquals(GeometryPanelVM.thumbnailHome(Vector.empty), None)
    assertEquals(GeometryPanelVM.thumbnailHome(thumbs), Some(0))
    val second = thumbs(1).trial
    val marked = GeometryPanel.update(panel, small, GeometryIntent.MarkTrial(second))._1
    val vm2    = GeometryPanelVM.of(marked, small, Some(picture(marked, small)))
    assertEquals(GeometryPanelVM.thumbnailHome(vm2.thumbnails), Some(1))
  }

  test("the thumbnail strip's keys: arrows step and move a row, Home and End, never wrap") {
    import eyes4s.studio.app.plot.RovingMove.*
    def move(at: Int, m: eyes4s.studio.app.plot.RovingMove, count: Int = 4) =
      GeometryPanelVM.thumbnailMove(at, m, count)
    // Two columns: 0 1 / 2 3.
    assertEquals(GeometryPanelVM.ThumbnailColumns, 2)
    assertEquals(Vector(move(0, Right), move(1, Right), move(3, Right)), Vector(1, 2, 3))
    assertEquals(Vector(move(3, Left), move(2, Left), move(0, Left)), Vector(2, 1, 0))
    assertEquals(Vector(move(0, Next), move(3, Next), move(1, Previous)), Vector(1, 3, 0))
    assertEquals(Vector(move(0, Down), move(1, Down), move(2, Down)), Vector(2, 3, 2))
    assertEquals(Vector(move(3, Up), move(2, Up), move(1, Up)), Vector(1, 0, 1))
    assertEquals(Vector(move(2, First), move(0, Last)), Vector(0, 3))
    // Three thumbnails: below 1 there is none, so Down stays put.
    assertEquals(move(1, Down, 3), 1)
    assertEquals(move(0, Down, 3), 2)
    assertEquals(move(0, Last, 3), 2)
    assertEquals(move(0, Right, 1), 0)
  }
