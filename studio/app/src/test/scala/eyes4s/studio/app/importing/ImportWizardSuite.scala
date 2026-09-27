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

import eyes4s.studio.app.{AppEffect, AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.{ChangeKind, Command}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.*

import java.nio.charset.StandardCharsets.UTF_8

/** The import wizard's behaviour, headless (ticket S5.2): roles, declared
  * units, presets, the data issues, and the "Dataset · re-admit" commands a
  * commit dispatches.
  */
class ImportWizardSuite extends munit.FunSuite:
  import StoryModels.{empty, ok, t1, t2}

  val golden: String =
    """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
       |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
       |P01,Encoding,enc_01,1,2,571.8,835.1,178,314,157
       |P01,Encoding,enc_01,1,3,1153.5,188.3,520,80,40
       |P01,Encoding,enc_01,1,4,1289.6,704.7,667,298,149
       |""".stripMargin

  val board: String =
    """|Subject,Phase,TrialID,Block,Image,FixNum,FixX,FixY,FixStart,FixDur,NSamples
       |P01,enc,enc_01,1,beach-007,1,1030,456,0,198,99
       |P01,enc,enc_01,1,beach-007,2,812,603,214,262,131
       |P01,enc,enc_01,1,beach-007,3,930,512,498,341,170
       |P01,enc,enc_01,1,beach-007,4,1301,388,861,175,87
       |""".stripMargin

  val trials: String =
    """|participant,phase,trial,occurrence,item,display_kind,image_file,response
       |P01,Encoding,enc_01,1,beach-007,image,beach-007.png,
       |""".stripMargin

  def source(role: SourceRole, path: String, text: String): SniffedSource =
    ok(SniffedSource.read(role, path, IArray.from(text.getBytes(UTF_8))))

  def col(s: String): ColumnName = ok(ColumnName.of(s))

  /** `text` previewed as `dataset`'s own fixation file (a re-map reads that
    * file; the first records stand for it here).
    */
  def own(document: StudioDocument, dataset: DatasetRevision, text: String): SniffedSource =
    source(SourceRole.Fixations, "inputs/fixations.csv", text)
      .copy(bytes = document.dataset(dataset).flatMap(_.sources.fixations).get.bytes)

  def run(
      w: ImportWizard,
      document: StudioDocument,
      intents: WizardIntent*
  ): (ImportWizard, Vector[WizardEffect]) =
    intents.foldLeft((w, Vector.empty[WizardEffect])) { case ((m, fx), i) =>
      val (next, more) = ImportWizard.update(m, i, document)
      (next, fx ++ more)
    }

  def commands(effects: Vector[WizardEffect]): Vector[Command] =
    effects.collect { case WizardEffect.Dispatch(c) => c }

  val geometry: Seq[WizardIntent] = Seq(
    GeometryField.ScreenWidth     -> "1920",
    GeometryField.ScreenHeight    -> "1080",
    GeometryField.ImageLeft       -> "448",
    GeometryField.ImageTop        -> "156",
    GeometryField.ImageWidth      -> "1024",
    GeometryField.ImageHeight     -> "768",
    GeometryField.PixelsPerDegree -> "35"
  ).map(WizardIntent.EditGeometry(_, _))

  test("board parity: the board's header shows the board's rows, roles and declared units") {
    val w0     = ImportWizard.newImport(empty, ImportPresets.empty)
    val (w, _) = run(
      w0,
      empty,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", board)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds))
    )
    val vm = ImportWizardVM.of(w, empty)
    assertEquals(vm.kind, "Dataset · re-admit")
    assertEquals(
      vm.fixations.headers,
      Vector("fixations.csv column", "First records", "Role", "Units (declared)")
    )
    assertEquals(
      vm.fixations.rows.map(r => (r.column.value, r.choiceLabel, r.units)),
      Vector(
        ("Subject", "Participant", "—"),
        ("Phase", "Phase", "—"),
        ("TrialID", "Trial", "—"),
        ("Block", "Occurrence", "—"),
        ("Image", "Match item", "—"),
        ("FixNum", "Ordinal", "—"),
        ("FixX", "x", "screen px"),
        ("FixY", "y", "screen px"),
        ("FixStart", "Onset", "ms"),
        ("FixDur", "Duration", "ms"),
        ("NSamples", "Sample count", "samples")
      )
    )
    assertEquals(vm.fixations.rows.head.samples, "P01, P01, P01, P01")
    assertEquals(vm.fixations.rows(6).samples, "1030, 812, 930, 1301")
    assertEquals(vm.fixations.rows.head.accessible, "Role for Subject")
    assertEquals(
      vm.tabs.map(_.label),
      Vector("Column mapping", "Trial metadata", "Geometry (1)", "Data issues")
    )
  }

  test("time units start undeclared and block the import until declared") {
    val (w, _) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden))
    )
    val vm = ImportWizardVM.of(w, t2)
    assertEquals(vm.time.selected, None)
    assertEquals(vm.fixations.rows.map(_.units).filter(_ == "undeclared").size, 2)
    val (refused, effects) = run(w, t2, WizardIntent.Commit)
    assertEquals(commands(effects), Vector.empty)
    assertEquals(refused.tab, WizardTab.DataIssues)
    val issues = ImportWizardVM.of(refused, t2).issues
    assertEquals(issues.map(_.column), Vector(Some("onset_ms")))
    assert(issues.head.text.contains("never inferred"), issues.head.text)
  }

  test("required roles: removing one blocks the commit with an issue naming the column") {
    val (w, effects) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Choose(SourceRole.Fixations, col("sample_count"), ColumnChoice.Attribute),
      WizardIntent.Commit
    )
    assertEquals(commands(effects), Vector.empty)
    w.problem match
      case Some(WizardProblem.Blocked(errors)) =>
        assertEquals(
          errors.toVector,
          Vector(
            MappingError.MissingRole(
              "fixations.csv",
              ColumnRole.SampleCount,
              Vector(col("sample_count"))
            )
          )
        )
      case other => fail(s"expected a blocked commit, got $other")
    val vm = ImportWizardVM.of(w, t2)
    assertEquals(vm.problem, Some("1 issue blocks the import."))
    assert(vm.issues.head.text.contains("sample count role"), vm.issues.head.text)
    assert(vm.issues.head.text.contains("sample_count"), vm.issues.head.text)
    assertEquals(vm.tabs.head.label, "Column mapping (1)")
  }

  test("a new import commits one ImportSources re-import of the latest dataset") {
    val (w, effects) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "session2/fixations.csv", golden)),
      WizardIntent.SourceRead(source(SourceRole.Trials, "session2/trials.csv", trials)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    assertEquals(w.problem, None)
    assertEquals(effects.last, WizardEffect.Close)
    commands(effects) match
      case Vector(
            c @ Command.ImportSources(parent, sources, mapping, units, geometry, attrs, _, _)
          ) =>
        assertEquals(c.kind, ChangeKind.DatasetReadmit)
        assertEquals(parent, Some(DatasetRevision(3)))
        assertEquals(sources.fixations.map(_.path.value), Some("session2/fixations.csv"))
        assertEquals(sources.trials.map(_.path.value), Some("session2/trials.csv"))
        assertEquals(mapping.column(ColumnRole.Onset), Some(col("onset_ms")))
        assertEquals(units, DeclaredUnits(Some(TimeUnit.Milliseconds)))
        assertEquals(geometry, t2.datasets.last.geometry)
        assertEquals(attrs, DeclaredAttributes.empty)
      case other => fail(s"expected one ImportSources, got $other")
    // The app applies it: a new pending dataset r4.
    val app        = AppModel.open(t2, None)
    val (next, fx) = AppModel.run(app, WizardEffect.appIntents(effects))
    assertEquals(next.document.datasets.last.id, DatasetRevision(4))
    assertEquals(next.document.datasets.last.decision, AdmissionDecision.Pending)
    assert(fx.exists { case AppEffect.Persist(_) => true; case _ => false })
    assertEquals(ImportWizardVM.of(w, t2).commit, "Import as r4")
  }

  test("a first import with no parent needs geometry, declared on the Geometry tab") {
    val (w, effects) = run(
      ImportWizard.newImport(empty, ImportPresets.empty),
      empty,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    assertEquals(commands(effects), Vector.empty)
    assertEquals(w.tab, WizardTab.Geometry)
    assertEquals(
      w.problem,
      Some(
        WizardProblem.BadGeometry(
          GeometryInputError.NotAWholeNumber(GeometryField.ScreenWidth, "")
        )
      )
    )
    val (done, fx) = run(w, empty, (geometry :+ WizardIntent.Commit)*)
    assertEquals(done.problem, None)
    commands(fx) match
      case Vector(Command.ImportSources(None, _, _, _, g, _, _, _)) =>
        assertEquals(g.image.render, "1024×768 px at (448, 156)")
      case other => fail(s"expected one ImportSources, got $other")
  }

  test("re-mapping a pending revision is one ReviseDataset; one undo restores it exactly") {
    // t1: r3 is the pending re-import, units declared ms.
    val w0     = ok(ImportWizard.remap(t1, DatasetRevision(3), ImportPresets.empty))
    val (w, _) = run(
      w0,
      t1,
      WizardIntent.SourceRead(own(t1, DatasetRevision(3), golden))
    )
    // Its own mapping comes back as the draft.
    assertEquals(
      w.fixations.map(_._2.columnsFor(ColumnRole.Occurrence)),
      Some(Vector(col("occurrence")))
    )
    assertEquals(ImportWizardVM.of(w, t1).commit, "Apply to r3")
    val (same, none) = run(w, t1, WizardIntent.Commit)
    assertEquals(commands(none), Vector.empty)
    assertEquals(same.problem, Some(WizardProblem.NoChange(DatasetRevision(3))))
    val (_, fx) = run(
      w,
      t1,
      WizardIntent.DeclareTime(Some(TimeUnit.Microseconds)),
      WizardIntent.Commit
    )
    val r3 = t1.dataset(DatasetRevision(3)).get
    assertEquals(
      commands(fx),
      Vector(
        Command.ReviseDataset(
          DatasetRevision(3),
          r3.mapping,
          DeclaredUnits(Some(TimeUnit.Microseconds)),
          r3.geometry,
          r3.attributes,
          r3.inventory
        )
      )
    )
    val applied = AppModel.run(AppModel.open(t1, None), WizardEffect.appIntents(fx))._1
    assertEquals(
      applied.document.dataset(DatasetRevision(3)).map(_.units.time),
      Some(Some(TimeUnit.Microseconds))
    )
    // One Undo of the science stack restores r3 exactly.
    val undone =
      AppModel.update(applied, Intent.Undo(eyes4s.studio.core.command.HistoryStack.Science))._1
    assertEquals(undone.document.dataset(DatasetRevision(3)), Some(r3))
    assertEquals(undone.document, t1)
  }

  test("re-mapping an admitted revision re-imports it as a new pending revision") {
    val w0      = ok(ImportWizard.remap(t2, DatasetRevision(3), ImportPresets.empty))
    val (w, fx) = run(
      w0,
      t2,
      WizardIntent.SourceRead(own(t2, DatasetRevision(3), golden)),
      WizardIntent.Choose(SourceRole.Fixations, col("occurrence"), ColumnChoice.Attribute),
      WizardIntent.Commit
    )
    assertEquals(w.problem, None)
    val r3 = t2.dataset(DatasetRevision(3)).get
    commands(fx) match
      case Vector(Command.ImportSources(Some(parent), sources, mapping, _, _, attrs, _, _)) =>
        assertEquals(parent, DatasetRevision(3))
        assertEquals(sources, r3.sources)
        assertEquals(mapping.column(ColumnRole.Occurrence), None)
        // The column that lost its role passes through as an attribute.
        assertEquals(attrs.columns, Vector(col("occurrence")))
      case other => fail(s"expected a re-import, got $other")
    assertEquals(ImportWizardVM.of(w0, t2).commit, "Re-admit as r4")
  }

  test("a re-map restores the revision's own inventory mapping and re-maps it (S5.4)") {
    // r2 keeps trials.csv's occurrence as an attribute, which the suggested
    // roles would not: the draft is the revision's mapping, not a proposal.
    val r2        = t2.dataset(DatasetRevision(2)).get
    val ownTrials =
      source(SourceRole.Trials, "inputs/trials.csv", trials)
        .copy(bytes = r2.sources.trials.get.bytes)
    val w0     = ok(ImportWizard.remap(t2, r2.id, ImportPresets.empty))
    val (w, _) = run(
      w0,
      t2,
      WizardIntent.SourceRead(own(t2, r2.id, golden)),
      WizardIntent.SourceRead(ownTrials)
    )
    assertEquals(w.problem, None)
    assertEquals(w.trials.map(_._2.resolve), Some(Right(r2.inventory.get)))
    // Mapping the occurrence changes the inventory mapping the re-import records
    // (r2's onsets were undeclared; the re-map declares them).
    val (after, fx) = run(
      w,
      t2,
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent
        .Choose(SourceRole.Trials, col("occurrence"), ColumnChoice.Role(ColumnRole.Occurrence)),
      WizardIntent.Commit
    )
    commands(fx) match
      case Vector(Command.ImportSources(Some(parent), _, _, _, _, _, _, Some(inventory))) =>
        assertEquals(parent, r2.id)
        assertEquals(inventory.column(ColumnRole.Occurrence), Some(col("occurrence")))
        assertEquals(
          inventory.attributes.columns,
          Vector(col("display_kind"), col("image_file"))
        )
      case other =>
        fail(s"expected a re-import with an inventory, got $other (${after.problem})")
    // Another file than the revision's own trials file is refused.
    val (refused, _) =
      run(w0, t2, WizardIntent.SourceRead(source(SourceRole.Trials, "other.csv", trials)))
    assertEquals(refused.problem, Some(WizardProblem.NotDatasetSource(r2.id, "other.csv")))
  }

  test("a preset saved from one file re-applies to a second file") {
    val first = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", board)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.TypePresetName("Lab export"),
      WizardIntent.SavePreset
    )
    val stored = first._2.collect { case WizardEffect.StorePreset(p) => p }
    assertEquals(stored.map(_.name.value), Vector("Lab export"))
    assertEquals(ImportWizardVM.of(first._1, t2).status, Some("Saved preset Lab export."))
    // A second wizard, on another file with the same export's columns, reordered.
    val second = board.linesIterator.toVector
      .map(_.split(",", -1).toVector)
      .map(cols => (cols.drop(5) ++ cols.take(5)).mkString(","))
      .mkString("\n")
    val presets = ok(ImportPresets.of(stored))
    val (w, fx) = run(
      ImportWizard.newImport(t2, presets),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "session2.csv", second)),
      WizardIntent.ApplyPreset("Lab export"),
      WizardIntent.Commit
    )
    assertEquals(w.problem, None)
    commands(fx) match
      case Vector(Command.ImportSources(_, _, mapping, units, _, _, _, _)) =>
        assertEquals(mapping.column(ColumnRole.Ordinal), Some(col("FixNum")))
        assertEquals(mapping.column(ColumnRole.Participant), Some(col("Subject")))
        assertEquals(units.time, Some(TimeUnit.Milliseconds))
      case other => fail(s"expected one ImportSources, got $other")
  }

  test("preset refusals name their operand; nothing else changes") {
    val (w, _) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden)),
      WizardIntent.ApplyPreset("missing")
    )
    assertEquals(
      w.problem,
      Some(WizardProblem.Preset(PresetError.UnknownPreset("missing", Vector.empty)))
    )
    val (blank, fx) = run(w, t2, WizardIntent.TypePresetName("  "), WizardIntent.SavePreset)
    assertEquals(fx, Vector.empty)
    assertEquals(blank.problem, Some(WizardProblem.Preset(PresetError.BlankName)))
  }

  test("trial metadata: positions are refused in trials.csv; its extras are attributes") {
    val (w, _) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Trials, "trials.csv", trials)),
      WizardIntent.Choose(
        SourceRole.Trials,
        col("display_kind"),
        ColumnChoice.Role(ColumnRole.X)
      )
    )
    assertEquals(
      w.problem,
      Some(
        WizardProblem.Mapping(
          MappingError.RoleNotRead("trials.csv", col("display_kind"), ColumnRole.X)
        )
      )
    )
    val vm = ImportWizardVM.of(w, t2)
    assertEquals(
      vm.trials.choices.map(_.menuLabel).take(3),
      Vector("Participant (required)", "Phase (required)", "Trial (required)")
    )
    assert(!vm.trials.choices.exists(_.choice == ColumnChoice.Role(ColumnRole.X)))
    assertEquals(
      vm.trials.rows.filter(_.choice == ColumnChoice.Attribute).map(_.column.value),
      Vector("display_kind", "image_file")
    )
  }

  test("a file that cannot be read is reported with its path; cancel closes") {
    val bad    = SourceReadError.Unpreviewable(SniffError.Empty("f.csv"))
    val (w, _) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.ReadFailed("f.csv", bad)
    )
    assertEquals(
      ImportWizardVM.of(w, t2).problem,
      Some("f.csv could not be read: f.csv is empty: it has no header line.")
    )
    assertEquals(run(w, t2, WizardIntent.Cancel)._2, Vector(WizardEffect.Close))
    assertEquals(
      run(w, t2, WizardIntent.RequestFile(SourceRole.Fixations))._2,
      Vector(WizardEffect.OpenFile(SourceRole.Fixations))
    )
  }

  test("a new import re-imports the latest revision at commit time, not at open") {
    // Opened on t1 (latest r3, pending); r4 arrives before the commit.
    val w0    = ImportWizard.newImport(t1, ImportPresets.empty)
    val r3    = t1.datasets.last
    val later = AppModel
      .run(
        AppModel.open(t1, None),
        Vector(
          Intent.Dispatch(
            Command.ImportSources(
              Some(r3.id),
              r3.sources,
              r3.mapping,
              DeclaredUnits(Some(TimeUnit.Seconds)),
              r3.geometry,
              DeclaredAttributes.empty,
              None,
              r3.inventory
            )
          )
        )
      )
      ._1
      .document
    assertEquals(later.datasets.last.id, DatasetRevision(4))
    val (_, fx) = run(
      w0,
      later,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    commands(fx) match
      case Vector(Command.ImportSources(parent, _, _, _, _, _, _, _)) =>
        assertEquals(parent, Some(DatasetRevision(4)))
      case other => fail(s"expected one ImportSources, got $other")
  }

  test("a store failure is shown and applies nothing") {
    val (w, fx) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.StoreFailed("presets/x.json: disk full")
    )
    assertEquals(fx, Vector.empty)
    assertEquals(ImportWizardVM.of(w, t2).problem, Some("Not saved: presets/x.json: disk full"))
  }

  test("a re-map refuses a file that is not the revision's own") {
    val w0     = ok(ImportWizard.remap(t1, DatasetRevision(3), ImportPresets.empty))
    val (w, _) = run(
      w0,
      t1,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "session2.csv", golden))
    )
    assertEquals(w.fixations, None)
    assertEquals(
      w.problem,
      Some(WizardProblem.NotDatasetSource(DatasetRevision(3), "session2.csv"))
    )
    assert(
      ImportWizardVM
        .of(w, t1)
        .problem
        .exists(_.contains("session2.csv is not one of r3's files"))
    )
  }

  test("unknown columns reach the command as declared attributes") {
    val withPupil =
      board.linesIterator.zipWithIndex
        .map((l, i) => l + (if i == 0 then ",Pupil,Notes" else ",3.1,ok"))
        .mkString("\n")
    val (_, fx) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", withPupil)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    commands(fx) match
      case Vector(Command.ImportSources(_, _, _, _, _, attrs, _, _)) =>
        assertEquals(attrs.columns.map(_.value), Vector("Pupil", "Notes"))
      case other => fail(s"expected one ImportSources, got $other")
    val next = AppModel.run(AppModel.open(t2, None), WizardEffect.appIntents(fx))._1
    assertEquals(
      next.document.datasets.last.attributes.columns.map(_.value),
      Vector("Pupil", "Notes")
    )
  }

  test("trial metadata issues block the import (S5.4), naming the column and the file") {
    val noPhase = trials.replace("participant,phase,", "participant,stage_x,")
    val (w, _)  = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", golden)),
      WizardIntent.SourceRead(source(SourceRole.Trials, "trials.csv", noPhase)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds))
    )
    assertEquals(w.trialIssues.size, 1)
    assertEquals(w.issues, w.trialIssues)
    val vm    = ImportWizardVM.of(w, t2)
    val issue = vm.issues.find(_.blocking).get
    assert(
      issue.text.startsWith("trials.csv: no column has the required phase role"),
      issue.text
    )
    assert(!vm.trialsNote.contains("S5.4"), vm.trialsNote)
    val (refused, fx) = run(w, t2, WizardIntent.Commit)
    assertEquals(commands(fx), Vector.empty)
    assertEquals(refused.tab, WizardTab.DataIssues)
    // Mapping the phase clears it, and the commit records the inventory mapping.
    val (fixed, done) = run(
      w,
      t2,
      WizardIntent
        .Choose(SourceRole.Trials, col("stage_x"), ColumnChoice.Role(ColumnRole.Phase)),
      WizardIntent.Commit
    )
    assertEquals(fixed.issues, Vector.empty)
    commands(done) match
      case Vector(Command.ImportSources(_, _, _, _, _, _, _, Some(inventory))) =>
        assertEquals(inventory.column(ColumnRole.Phase), Some(col("stage_x")))
        assertEquals(inventory.column(ColumnRole.Item), Some(col("item")))
      case other => fail(s"expected one ImportSources with an inventory, got $other")
  }

  test("a header-only file is refused on commit, naming the file") {
    val headerOnly = golden.linesIterator.next()
    val (w, fx)    = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", headerOnly)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    assertEquals(commands(fx), Vector.empty)
    assert(
      ImportWizardVM
        .of(w, t2)
        .issues
        .exists(_.text.contains("fixations.csv has a header but no records"))
    )
  }
