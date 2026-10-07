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

package eyes4s.studio.app.importing

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.vm.{A11y, A11yRole, FocusStop}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.{ChangeKind, Command, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.*

import java.nio.charset.StandardCharsets.UTF_8

/** The Data perspective's column-mapping pane, headless (bead
  * bd-01M3G8PH400E7F6716ZBHZ2XVH): it follows the selected dataset
  * revision, re-maps it with the import wizard on its own files, commits one
  * "Dataset · re-admit" command, and one undo restores the revision it
  * reloads to.
  */
class ColumnMappingPaneSuite extends munit.FunSuite:
  import StoryModels.{ok, t1}

  val golden: String =
    """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
       |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
       |P01,Encoding,enc_01,1,2,571.8,835.1,178,314,157
       |""".stripMargin

  val r2: DatasetRevision = DatasetRevision(2)
  val r3: DatasetRevision = DatasetRevision(3)

  def select(m: AppModel, id: DatasetRevision): AppModel =
    AppModel
      .update(m, Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(id)))))
      ._1

  /** `source` as the project holds it: `text` previewed, standing for the
    * revision's own bytes (the digest is the source's).
    */
  def stored(source: Source, text: String): SniffedSource =
    ok(SniffedSource.read(source.role, source.path.value, IArray.from(text.getBytes(UTF_8))))
      .copy(bytes = source.bytes)

  def run(w: ImportWizard, document: StudioDocument, intents: WizardIntent*): ImportWizard =
    intents.foldLeft(w)((x, i) => ImportWizard.update(x, i, document)._1)

  /** Open the pane on `m`'s selection and deliver its sources as the project
    * would.
    */
  /** The first record of the golden trials.csv: the story revisions map its
    * columns as their inventory (S5.4).
    */
  val trials: String =
    "participant,phase,trial,occurrence,item,display_kind,image_file,response\n" +
      "P01,Encoding,enc_01,1,beach-007,image,beach-007.png,\n"

  def opened(m: AppModel): (DatasetRevisionSpec, ImportWizard) =
    val spec          = ColumnMappingPane.selected(m).getOrElse(fail("nothing selected"))
    val (w0, sources) = ok(ColumnMappingPane.open(m.document, spec, ImportPresets.empty))
    val read          = sources.foldLeft(w0) { (w, s) =>
      val text = if s.role == SourceRole.Fixations then golden else trials
      ImportWizard.update(w, WizardIntent.SourceRead(stored(s, text)), m.document)._1
    }
    (spec, read)

  test("the pane follows the Data selection, else the latest revision") {
    val m = StoryModels.t1Data
    assertEquals(ColumnMappingPane.selected(m).map(_.id), Some(r3))
    assertEquals(ColumnMappingPane.selected(select(m, r2)).map(_.id), Some(r2))
    // A trail naming a revision the document does not hold shows the latest.
    assertEquals(ColumnMappingPane.selected(select(m, DatasetRevision(9))).map(_.id), Some(r3))
    val first = AppModel.newProject.fold(e => fail(e.message), identity)
    assertEquals(ColumnMappingPane.selected(first), None)
    assertEquals(
      ColumnMappingPane.vm(None, reading = false).empty,
      Some("No dataset revision yet. Import sources to map their columns.")
    )
  }

  test("it reloads when the selection or the revision changes, not otherwise") {
    val m    = StoryModels.t1Data
    val spec = ColumnMappingPane.selected(m)
    assert(!ColumnMappingPane.mustReload(spec, m))
    assert(!ColumnMappingPane.mustReload(spec, AppModel.update(m, Intent.RequestImport)._1))
    assert(ColumnMappingPane.mustReload(spec, select(m, r2)))
    assert(ColumnMappingPane.mustReload(None, m))
  }

  test("it opens a re-map of the selection on the revision's own files, fixations first") {
    val m            = select(StoryModels.t1Data, r2)
    val spec         = ColumnMappingPane.selected(m).get
    val (w, sources) = ok(ColumnMappingPane.open(m.document, spec, ImportPresets.empty))
    assertEquals(w.target, WizardTarget.Remap(r2))
    assertEquals(sources, spec.sources.entries)
    assertEquals(sources.map(_.role), Vector(SourceRole.Fixations, SourceRole.Trials))
    val vm = ImportWizardVM.of(w, m.document)
    // No file to choose, no "choose a file" text: the pane reads the project.
    assertEquals(vm.fixations.canChoose, false)
    assertEquals(vm.trials.canChoose, false)
    assertEquals(vm.fixations.empty, None)
    assertEquals(vm.cancel, "Revert")
    assertEquals(vm.commit, "Re-admit as r4")
    assertEquals(vm.kind, ChangeKind.DatasetReadmit.label)
    assertEquals(
      ColumnMappingPane.vm(Some(spec), reading = true).reading,
      Some("Reading r2’s files from the project…")
    )
    assertEquals(ColumnMappingPane.vm(Some(spec), reading = false).reading, None)
    // File › Import keeps its own words and its file choice.
    val fresh = ImportWizardVM.of(ImportWizard.newImport(t1, ImportPresets.empty), t1)
    assertEquals((fresh.cancel, fresh.fixations.canChoose), ("Cancel", true))
  }

  test("selection to re-admit: one ReviseDataset; one undo restores the revision") {
    val m         = StoryModels.t1Data
    val (spec, w) = opened(m)
    assertEquals(spec.id, r3)
    assertEquals(w.problem, None)
    // The draft is the revision's own mapping.
    assertEquals(
      w.fixations.map(_._2.columnsFor(ColumnRole.Occurrence)),
      Some(Vector(ok(ColumnName.of("occurrence"))))
    )
    assertEquals(ImportWizardVM.of(w, m.document).commit, "Apply to r3")
    val occurrence  = ok(ColumnName.of("occurrence"))
    val (after, fx) = Seq(
      WizardIntent.Choose(SourceRole.Fixations, occurrence, ColumnChoice.Attribute),
      WizardIntent.Commit
    ).foldLeft((w, Vector.empty[WizardEffect])) { case ((x, acc), i) =>
      val (next, more) = ImportWizard.update(x, i, m.document)
      (next, acc ++ more)
    }
    assertEquals(after.problem, None)
    val commands = fx.collect { case WizardEffect.Dispatch(c) => c }
    commands match
      case Vector(Command.ReviseDataset(id, mapping, units, geometry, attributes, _)) =>
        assertEquals(id, r3)
        assertEquals(mapping.column(ColumnRole.Occurrence), None)
        assertEquals(attributes.columns, Vector(occurrence))
        assertEquals((units, geometry), (spec.units, spec.geometry))
      case other => fail(s"expected one ReviseDataset, got $other")
    assertEquals(fx.last, WizardEffect.Close)
    assertEquals(commands.map(_.kind), Vector(ChangeKind.DatasetReadmit))
    val applied = AppModel.run(m, WizardEffect.appIntents(fx))._1
    // The pane reloads onto the revised r3 …
    assert(ColumnMappingPane.mustReload(Some(spec), applied))
    assertEquals(
      ColumnMappingPane.selected(applied).map(_.attributes.columns),
      Some(Vector(occurrence))
    )
    // … and one undo restores r3 exactly, which it reloads to.
    val undone = AppModel.update(applied, Intent.Undo(HistoryStack.Science))._1
    assertEquals(undone.document, m.document)
    assertEquals(ColumnMappingPane.selected(undone), Some(spec))
  }

  test("an admitted revision re-maps as one re-import under it") {
    val m       = select(StoryModels.t1Data, r2)
    val (_, w)  = opened(m)
    val (_, fx) = ImportWizard.update(
      ImportWizard
        .update(w, WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)), m.document)
        ._1,
      WizardIntent.Commit,
      m.document
    )
    fx.collect { case WizardEffect.Dispatch(c) => c } match
      case Vector(Command.ImportSources(Some(parent), sources, _, units, _, _, _, _)) =>
        assertEquals(parent, r2)
        assertEquals(sources, m.document.dataset(r2).get.sources)
        assertEquals(units.time, Some(TimeUnit.Milliseconds))
      case other => fail(s"expected one re-import, got $other")
    // The model applies it: a new pending r4 under r2.
    val applied = AppModel.run(m, WizardEffect.appIntents(fx))._1
    val r4      = applied.document.dataset(DatasetRevision(4)).getOrElse(fail("no r4"))
    assertEquals((r4.parent, r4.decision), (Some(r2), AdmissionDecision.Pending))
    assertEquals(r4.units.time, Some(TimeUnit.Milliseconds))
    // The Data selection follows the new revision, so the pane reloads onto
    // r4 and holds no edit already applied to r2.
    val follow = ColumnMappingPane.follow(m.document, applied)
    assertEquals(
      follow,
      Some(
        Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(DatasetRevision(4)))))
      )
    )
    val moved = AppModel.run(applied, follow.toVector)._1
    assertEquals(ColumnMappingPane.selected(moved), Some(r4))
    assert(ColumnMappingPane.mustReload(ColumnMappingPane.selected(m), moved))
    // Re-mapping r4 as it now is changes nothing: no second revision.
    val (_, again)      = opened(moved)
    val (refused, none) = ImportWizard.update(again, WizardIntent.Commit, moved.document)
    assertEquals(none, Vector.empty)
    assertEquals(refused.problem, Some(WizardProblem.NoChange(DatasetRevision(4))))
    // One undo restores the document.
    val undone = AppModel.update(moved, Intent.Undo(HistoryStack.Science))._1
    assertEquals(undone.document, m.document)
    // An edit in place moves nothing.
    assertEquals(ColumnMappingPane.follow(m.document, m), None)
  }

  test("a re-map shows the mapping page and its trial metadata; edits and notices are named") {
    val m      = StoryModels.t1Data
    val (_, w) = opened(m)
    val vm     = ImportWizardVM.of(w, m.document)
    assertEquals((vm.showTabs, vm.tab), (true, WizardTab.FixationMapping))
    // The revision has a trials file: its inventory mapping is re-mapped here
    // too (S5.4); geometry and the issues belong to the sibling panes.
    assertEquals(
      vm.tabs.map(_.tab),
      Vector(WizardTab.FixationMapping, WizardTab.TrialMetadata)
    )
    val trialPage = ImportWizardVM.of(
      ImportWizard.update(w, WizardIntent.ChooseTab(WizardTab.TrialMetadata), m.document)._1,
      m.document
    )
    assertEquals(trialPage.tab, WizardTab.TrialMetadata)
    val geometryPage = ImportWizardVM.of(
      ImportWizard.update(w, WizardIntent.ChooseTab(WizardTab.Geometry), m.document)._1,
      m.document
    )
    assertEquals(geometryPage.tab, WizardTab.FixationMapping)
    // A blocked commit stays on the mapping page and says why there.
    val blocked = run(
      w,
      m.document,
      WizardIntent.Choose(SourceRole.Fixations, ok(ColumnName.of("x")), ColumnChoice.Attribute),
      WizardIntent.Commit
    )
    assert(blocked.problem.exists {
      case WizardProblem.Blocked(_) => true
      case _                        => false
    })
    assertEquals(ImportWizardVM.of(blocked, m.document).tab, WizardTab.FixationMapping)
    assert(ImportWizardVM.of(blocked, m.document).problem.nonEmpty)
    assert(ImportWizardVM.of(ImportWizard.newImport(t1, ImportPresets.empty), t1).showTabs)
    // Edits are the mapping, units, trial roles and geometry; presets are not.
    assert(!w.editedSince(w))
    assert(blocked.editedSince(w))
    assert(!w.withPresets(ImportPresets.empty).editedSince(w))
    assertEquals(
      PaneNotice.EditsDropped(r3).message,
      "r3 changed, so the edits not yet applied to it were dropped."
    )
    assertEquals(
      ColumnMappingPane
        .vm(
          Some(ColumnMappingPane.selected(m).get),
          false,
          Some(PaneNotice.PresetsUnreadable(Vector("a.json: bad")))
        )
        .notice,
      Some("Some saved import presets could not be read: a.json: bad")
    )
    assertEquals(
      PaneNotice.CannotOpen(WizardProblem.UnknownDataset(DatasetRevision(9))).message,
      "Dataset r9 is not in the project."
    )
  }

  test("a file that is not the revision's own is refused; an unreadable one names its path") {
    val m             = StoryModels.t1Data
    val spec          = ColumnMappingPane.selected(m).get
    val (w0, sources) = ok(ColumnMappingPane.open(m.document, spec, ImportPresets.empty))
    val fixations     = sources.head
    val other         =
      ok(
        SniffedSource.read(
          SourceRole.Fixations,
          "inputs/fixations.csv",
          IArray.from(golden.getBytes(UTF_8))
        )
      )
    val (refused, _) = ImportWizard.update(w0, WizardIntent.SourceRead(other), m.document)
    assertEquals(
      refused.problem,
      Some(WizardProblem.NotDatasetSource(r3, "inputs/fixations.csv"))
    )
    assertEquals(refused.fixations, None)
    val (failed, _) = ImportWizard.update(
      w0,
      ColumnMappingPane.unreadable(fixations, ColumnMappingPane.noProject),
      m.document
    )
    assertEquals(
      ImportWizardVM.of(failed, m.document).problem,
      Some("inputs/fixations.csv could not be read: no project is open to read it from")
    )
  }

  test("the wizard's controls are stops after the pane's own, in the view's order") {
    val m      = StoryModels.t1Data
    val (_, w) = opened(m)
    val vm     = ImportWizardVM.of(w, m.document)
    val stops  = ColumnMappingPane.focusStops(vm)
    // A re-map of a revision with a trials file shows its two pages: the
    // selected tab, then the time unit.
    assertEquals(
      stops.take(2),
      Vector(
        FocusStop(A11yRole.ToggleButton, "Column mapping"),
        FocusStop(A11yRole.ComboBox, "Time unit (declared)")
      )
    )
    assertEquals(
      stops.slice(2, 2 + vm.fixations.rows.size).map(_.name),
      vm.fixations.rows.map(r => s"Role for ${r.column.value}")
    )
    assertEquals(
      stops.takeRight(4),
      Vector(
        FocusStop(A11yRole.TextField, "Preset name"),
        // The trial key builder's occurrence toggle (S5.3).
        FocusStop(A11yRole.ToggleButton, "Occurrence: column occurrence"),
        FocusStop(A11yRole.Button, "Revert"),
        FocusStop(A11yRole.Button, "Apply to r3")
      )
    )
    // A new import's wizard shows its tabs: one stop, the selected tab.
    val fresh = ImportWizard.newImport(t1, ImportPresets.empty)
    assertEquals(
      ColumnMappingPane.focusStops(ImportWizardVM.of(fresh, t1)).head,
      FocusStop(A11yRole.ToggleButton, "Column mapping")
    )
    val geometry = ColumnMappingPane.focusStops(
      ImportWizardVM.of(
        ImportWizard.update(fresh, WizardIntent.ChooseTab(WizardTab.Geometry), t1)._1,
        t1
      )
    )
    assertEquals(geometry.count(_.role == A11yRole.TextField), GeometryField.values.length)
    // The shell's Tab order places them right after the pane's stop.
    val order = A11y.tabOrder(
      m,
      inside = p => if p == StudioLayouts.columnMapping then stops else Vector.empty
    )
    val pane = order.indexOf(FocusStop(A11yRole.Region, "Column mapping"))
    assert(pane >= 0, order.toString)
    assertEquals(order.slice(pane + 1, pane + 1 + stops.size), stops)
    assertEquals(order.size, A11y.tabOrder(m).size + stops.size)
  }

  test("display controls persist mappings, follow undo and keep dropped inventory edits") {
    val m      = StoryModels.t1Data
    val (_, w) = opened(m)
    val ds     =
      DisplayColumns(ok(ColumnName.of("display_kind")), Some(ok(ColumnName.of("image_file"))))
    val changed = run(
      w,
      m.document,
      WizardIntent.DeclareDisplays(Some(ds)),
      WizardIntent.ChooseTab(WizardTab.TrialMetadata)
    )
    assert(changed.editedSince(w))
    val vm = ImportWizardVM.of(changed, m.document)
    assertEquals(vm.displays.selected, Some(ds))
    assertEquals(
      vm.displays.options.tail.map(_.label),
      changed.trials.get._2.preview.header.map(_.value)
    )
    assertEquals(
      ColumnMappingPane.focusStops(vm).takeRight(4).map(_.name),
      Vector("Display kind column", "Image file column", "Revert", "Apply to r3")
    )
    val (_, fx) = ImportWizard.update(changed, WizardIntent.Commit, m.document)
    val after   = AppModel.run(m, WizardEffect.appIntents(fx))._1
    assertEquals(after.document.dataset(r3).flatMap(_.inventory).flatMap(_.displays), Some(ds))
    val (_, reopened) = opened(after)
    assertEquals(reopened.trials.flatMap(_._2.displays), Some(ds))
    assertEquals(
      ImportWizard.commands(reopened, after.document),
      Left(WizardProblem.NoChange(r3))
    )
    val dropped = run(changed, m.document, WizardIntent.DropTrials(true))
    assert(!ImportWizardVM.of(dropped, m.document).displays.enabled)
    assertEquals(
      run(dropped, m.document, WizardIntent.DropTrials(false)).trials.flatMap(_._2.displays),
      Some(ds)
    )
    assertEquals(
      AppModel.update(after, Intent.Undo(HistoryStack.Science))._1.document,
      m.document
    )
  }
