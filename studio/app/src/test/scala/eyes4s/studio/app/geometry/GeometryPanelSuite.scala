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
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{MockStudy, StoryMoments}
import eyes4s.studio.core.geometry.SourcePositions
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
      s.inventoryTrials,
      s.admitted,
      Vector.empty,
      0,
      s.absent,
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
      ""
    )

  test("the panel follows the Data selection and asks for its records and counts") {
    val (panel, effects) = synced(t1)
    assertEquals(panel.shown.map(_.id), Some(r3))
    assertEquals(panel.fields.field(GeometryField.ImageLeft), "448")
    assertEquals(
      effects.map(_.productPrefix),
      Vector("ReadPositions", "RequestCounts")
    )
    assertEquals(effects.last, GeometryEffect.RequestCounts(r3))
    // r2 shares r3's source file but not its mapping: its records are read again.
    val onR2 =
      play(t1, _ => Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(r2)))))
    val (moved, again) = GeometryPanel.sync(panel, onR2)
    assertEquals(moved.shown.map(_.id), Some(r2))
    assertEquals(again.map(_.productPrefix), Vector("ReadPositions", "RequestCounts"))
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
          Some(r3spec.admission.copy(offScreen = OffScreenChoice.QuarantineTrial))
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
    // The new draft shares r3's records; only its counts are asked for.
    val (onR4, asks) = GeometryPanel.sync(panel, followed)
    assertEquals(asks, Vector(GeometryEffect.RequestCounts(r4)))
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
        Command.ReviseDataset(r3, r3spec.mapping, r3spec.units, geometry, r3spec.attributes)
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
    val spec = r3spec.copy(sources = ok(Sources.of(Vector(source))))
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
    val key  = effects.collectFirst { case GeometryEffect.ReadPositions(k, _) => k }.get
    val spec = panel.shown.get
    val read = SourcePositions.read(spec, bytes).left.map(_.message)
    GeometryPanel.update(panel, model, GeometryIntent.PositionsRead(key, read))._1

  private def picture(panel: GeometryPanel, model: AppModel): GeometryPictures =
    val key = GeometryPictures.keyOf(panel, model).get
    GeometryPictures
      .of(key, model.document.dataset(key.dataset).get, panel.positions.toOption.get)
      .fold(p => fail(p.message), identity)

  test("thumbnails: the first trial, then the trials with most records outside the frame") {
    val panel = loaded(small)
    val pics  = picture(panel, small)
    val vm    = GeometryPanelVM.of(panel, small, Some(pics))
    assertEquals(
      vm.thumbnails.map(_.label),
      Vector(
        "P01 · enc_01 · 2 records inside",
        "P05 · ret_04 · 2 of 2 outside",
        "P09 · enc_01 · 1 of 1 off screen",
        "P17 · enc_03 · 1 records inside"
      )
    )
    assertEquals(
      vm.thumbnails.map(_.ref).head,
      StudioRef.Trial(TrialKey("P01", Phase.Encoding, "enc_01", 1))
    )
    assertEquals(vm.densityCaption.take(9), "6 records")
    assertEquals(pics.density.records, 6)
    // The off-screen record falls in no cell: five cells are drawn.
    assertEquals(pics.density.levels.count(_ > 0), 5)
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
      panel.positions.toOption.get.position(3).map(p => (p.x, p.y)),
      Some((260.0, 120.0))
    )
  }
