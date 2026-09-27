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

import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{DatasetRevision, Phase, TrialKey}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.importing.*
import eyes4s.studio.core.selection.{RecordNumber, StudioRef}

import java.nio.charset.StandardCharsets.UTF_8

/** The trial key builder in the import wizard, headless (ticket S5.3;
  * Data.dc.html, key builder): the board's key line on a golden-shaped
  * study, live duplicate detection as the key changes, repeated keys
  * reported with every trial (never first-match resolved), the Studio check
  * that blocks the import, and the key change as a "Dataset · re-admit"
  * mapping edit.
  */
class TrialKeyBuilderSuite extends munit.FunSuite:
  import StoryModels.{ok, t1, t2}

  def source(role: SourceRole, path: String, text: String): SniffedSource =
    ok(SniffedSource.read(role, path, IArray.from(text.getBytes(UTF_8))))

  def col(s: String): ColumnName = ok(ColumnName.of(s))

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

  /** A new import of the golden-shaped study (or its `blocks` variant), with
    * the time unit declared; t2's latest revision supplies the geometry.
    */
  def opened(blocks: Boolean, inventory: Boolean = true): ImportWizard =
    val reads =
      Vector(
        WizardIntent.SourceRead(
          source(SourceRole.Fixations, "fixations.csv", KeyStudies.fixations(blocks))
        )
      ) ++ Option
        .when(inventory)(
          WizardIntent.SourceRead(
            source(SourceRole.Trials, "trials.csv", KeyStudies.inventory(blocks))
          )
        )
        .toVector :+ WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds))
    run(ImportWizard.newImport(t2, ImportPresets.empty), t2, reads*)._1

  def key(w: ImportWizard): TrialKeyVM = ImportWizardVM.of(w, t2).key

  // --- board parity -------------------------------------------------------------------

  test("board parity: Participant + Phase + Trial + Occurrence = 960 unique keys") {
    val vm = key(opened(blocks = false))
    assertEquals(vm.title, "Trial identity")
    assertEquals(
      vm.rule,
      "Every record and every inventory entry must resolve to exactly one key."
    )
    assertEquals(vm.blocks.map(_.label), Vector("Participant", "Phase", "Trial", "Occurrence"))
    assertEquals(
      vm.blocks.map(_.column),
      Vector(Some("Subject"), Some("Phase"), Some("TrialID"), Some("Block"))
    )
    assert(vm.blocks.forall(_.included))
    assertEquals((vm.plus, vm.equals), ("+", "="))
    // The inventory's line is the board's: every inventory entry.
    val first = vm.lines.head
    assertEquals(first.file, None)
    assertEquals(first.count, "960 unique keys")
    assertEquals(first.detail, "· 0 duplicates · Occurrence is 1 for every trial")
    assertEquals(first.tone, KeyTone.Unique)
    assertEquals(
      vm.lines(1).accessible,
      "fixations.csv: 960 unique keys · 0 duplicates · Occurrence is 1 for every trial"
    )
    assertEquals(vm.check, None)
    assertEquals(vm.empty, None)
  }

  test("before a file is read the key builder says what it needs") {
    val vm = key(ImportWizard.newImport(t2, ImportPresets.empty))
    assertEquals(vm.empty, Some("Choose fixations.csv to compose the trial key."))
    assertEquals(vm.lines, Vector.empty)
    assert(vm.blocks.forall(b => b.column.isEmpty && !b.included))
  }

  // --- live duplicate detection ----------------------------------------------------------

  test("leaving Occurrence out: 38 keys repeat without Occurrence, live, with both trials") {
    val w         = opened(blocks = true)
    val (out, fx) = run(w, t2, WizardIntent.IncludeOccurrence(false))
    assertEquals(fx, Vector.empty)
    // The key change is the mapping's: Block is an attribute now, in both files.
    assertEquals(
      out.fixations.map(_._2.choice(col("Block"))),
      Some(Some(ColumnChoice.Attribute))
    )
    assertEquals(
      out.trials.map(_._2.columns.find(_._1.name == col("Block")).map(_._2)),
      Some(Some(ColumnChoice.Attribute))
    )
    val vm         = key(out)
    val occurrence = vm.blocks.last
    assert(!occurrence.included)
    assertEquals(occurrence.toggle.map(_.include), Some(true))
    assertEquals(
      occurrence.toggle.map(_.label),
      Some("Add Occurrence to the key (column Block)")
    )
    val fixLine = vm.lines.find(_.source == SourceRole.Fixations).get
    assertEquals(fixLine.count, "922 keys")
    assertEquals(
      fixLine.detail,
      "· 38 keys repeat without Occurrence · Occurrence left out: 1 for every trial"
    )
    assertEquals(fixLine.tone, KeyTone.Blocking)
    // Every repeated key is listed with every trial: never the first match alone.
    assertEquals(fixLine.repeated.size, 38)
    fixLine.repeated.foreach(r => assertEquals(r.trials.size, 2, r.key))
    val p01 = fixLine.repeated.head
    assertEquals(p01.label, "fixations.csv · P01 · Encoding · enc_01")
    assertEquals(
      p01.trials.map(_.text),
      Vector("occurrence 1 · records 1–3", "occurrence 2 · records 55–57")
    )
    assertEquals(p01.summary, "occurrence 1 · records 1–3  |  occurrence 2 · records 55–57")
    // Each record traces to a StudioRef: the key eyes4s would give it (occurrence 1 for both).
    val eyes4sKey = TrialKey("P01", Phase.Encoding, "enc_01", 1)
    assertEquals(
      p01.trials.flatMap(_.refs),
      // Each trial's first and last record.
      Vector(1, 3, 55, 57).map(n =>
        StudioRef.SourceRecord(eyes4sKey, None, SourceRole.Fixations, ok(RecordNumber.of(n)))
      )
    )
    // Putting Occurrence back restores the mapping and the first line.
    val (back, _) = run(out, t2, WizardIntent.IncludeOccurrence(true))
    assertEquals(back.fixations.map(_._2), w.fixations.map(_._2))
    assertEquals(back.trials.map(_._2), w.trials.map(_._2))
  }

  test("the Studio check blocks the commit; the wizard opens the key; nothing is dispatched") {
    val (w, fx) =
      run(opened(blocks = true), t2, WizardIntent.IncludeOccurrence(false), WizardIntent.Commit)
    assertEquals(commands(fx), Vector.empty)
    assertEquals(w.tab, WizardTab.FixationMapping)
    w.problem match
      case Some(
            WizardProblem.TrialKey(KeyBlock.RepeatWithoutOccurrence(file, column, count, first))
          ) =>
        assertEquals(file, "fixations.csv")
        assertEquals(column, col("Block"))
        assertEquals(count, 38)
        assertEquals(first, KeyStudies.repeatedKeys.sorted.headOption)
        val listed = w.keys.fixationReport.get.repeated
        assertEquals(listed.map(_.key), KeyStudies.repeatedKeys.sorted)
        assert(listed.forall(_.trials.size == 2))
      case other => fail(s"expected the trial key's Studio check, got $other")
    val vm = ImportWizardVM.of(w, t2)
    assert(vm.problem.exists(_.startsWith("Studio check · fixations.csv: 38 keys repeat")))
    assert(vm.key.check.exists(_.contains("38 keys repeat without Occurrence")))
    assertEquals(vm.issuesSummary, "1 issue blocks the import.")
    assertEquals(vm.issues.filter(_.blocking).map(_.column), Vector(Some("Block")))
    assert(vm.tabs.exists(_.label == "Column mapping (1)"), vm.tabs.toString)
  }

  test("with Occurrence the keys still repeat: reported as eyes4s occurrence conflicts") {
    val w       = opened(blocks = true)
    val vm      = ImportWizardVM.of(w, t2)
    val fixLine = vm.key.lines.find(_.source == SourceRole.Fixations).get
    assertEquals(fixLine.count, "922 keys")
    assertEquals(
      fixLine.detail,
      "· 38 keys name more than one occurrence · Occurrence 1–2 · 38 later presentations"
    )
    assertEquals(fixLine.tone, KeyTone.Warning)
    assertEquals(vm.key.check, None)
    // The records keep their own occurrence: two eyes4s keys for one label.
    assertEquals(
      fixLine.repeated.head.trials.map(_.refs.head),
      Vector(1, 55)
        .zip(Vector(1, 2))
        .map((n, o) =>
          StudioRef.SourceRecord(
            TrialKey("P01", Phase.Encoding, "enc_01", o),
            None,
            SourceRole.Fixations,
            ok(RecordNumber.of(n))
          )
        )
    )
    // Warnings, not blocks: eyes4s quarantines each (occurrence conflict).
    val warnings = vm.issues.filterNot(_.blocking).map(_.text)
    assert(
      warnings.exists(_.contains("fixations.csv: 38 keys name more than one occurrence")),
      warnings.toString
    )
    assert(warnings.exists(_.contains("trials.csv: 38 keys repeat on more than one record")))
    val (done, fx) = run(w, t2, WizardIntent.Commit)
    assertEquals(done.problem, None)
    commands(fx) match
      case Vector(Command.ImportSources(_, _, mapping, _, _, _, _, _)) =>
        assertEquals(mapping.column(ColumnRole.Occurrence), Some(col("Block")))
      case other => fail(s"expected one ImportSources, got $other")
  }

  test("the inventory line reports repeated inventory entries with their records") {
    val vm   = key(opened(blocks = true))
    val line = vm.lines.head
    assertEquals(line.source, SourceRole.Trials)
    assertEquals(line.count, "922 keys")
    assertEquals(line.detail, "· 38 keys repeat · Occurrence 1–2 · 38 later presentations")
    assertEquals(
      line.repeated.head.trials.map(_.text),
      Vector("occurrence 1 · record 1", "occurrence 2 · record 19")
    )
  }

  // --- the key's parts ------------------------------------------------------------------------

  test("Phase is a required role: eyes4s reads a trial key from phase too") {
    val text    = KeyStudies.fixations(false).replace("Subject,Phase,", "Subject,Stage_x,")
    val (w, fx) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", text)),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.Commit
    )
    assertEquals(commands(fx), Vector.empty)
    w.problem match
      case Some(WizardProblem.Blocked(errors)) =>
        assertEquals(
          errors.toVector.collect { case MappingError.MissingRole(_, role, _) => role },
          Vector(ColumnRole.Phase)
        )
      case other => fail(s"expected the missing Phase role, got $other")
    // The key builder shows the gap; it adds no block of its own.
    assertEquals(TrialKeyVM.keyBlocks(w), Vector.empty)
    val vm = key(w)
    assertEquals(vm.blocks(1).accessible, "Phase: no column")
    assert(!vm.blocks(1).included)
    assertEquals(vm.lines, Vector.empty)
    // Mapping Phase lifts the block.
    val (fixed, more) = run(
      w,
      t2,
      WizardIntent
        .Choose(SourceRole.Fixations, col("Stage_x"), ColumnChoice.Role(ColumnRole.Phase)),
      WizardIntent.Commit
    )
    assertEquals(fixed.problem, None)
    assertEquals(commands(more).size, 1)
  }

  test("adding Occurrence with no column that can hold it is refused, naming the file") {
    val text   = KeyStudies.fixations(false).replace(",Block,", ",Session,")
    val (w, _) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", text)),
      WizardIntent.IncludeOccurrence(true)
    )
    assertEquals(w.problem, Some(WizardProblem.NoOccurrenceColumn("fixations.csv")))
    val vm = ImportWizardVM.of(w, t2)
    assertEquals(vm.key.blocks.last.toggle, None)
    assertEquals(
      vm.key.blocks.last.accessible,
      "Occurrence: not in the key; eyes4s takes 1 for every trial"
    )
    assertEquals(
      vm.key.lines.head.detail,
      "· 0 duplicates · Occurrence left out: 1 for every trial"
    )
    assert(vm.problem.exists(_.startsWith("fixations.csv has no column that can hold")))
  }

  test("records that resolve to no key are counted and warned, first record named") {
    val text = KeyStudies
      .fixations(false)
      .replaceFirst("P01,Encoding,enc_01,1,1,", ",Encoding,enc_01,1,1,")
    val w = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(source(SourceRole.Fixations, "fixations.csv", text))
    )._1
    val vm = ImportWizardVM.of(w, t2)
    assertEquals(
      vm.key.lines.head.detail,
      "· 0 duplicates · 1 record resolves to no key · Occurrence is 1 for every trial"
    )
    assertEquals(vm.key.lines.head.count, "960 keys")
    assert(
      vm.issues.exists(i =>
        !i.blocking && i.text.contains("record 1: Subject is '', not a non-blank value")
      ),
      vm.issues.toString
    )
  }

  // --- a key on a column the sniffer did not keep ----------------------------------------

  test("a key on a many-valued column is checked by one streaming pass; until then it blocks") {
    // 5,000 distinct trial labels: past the sniffer's cap, so the column is not kept.
    val rows = (1 to 5000).flatMap(i =>
      Vector(s"P01,Enc,t$i,1,1,960,540,300,200,100", s"P01,Enc,t$i,1,2,960,540,600,200,100")
    ) ++ Vector("P01,Enc,t1,2,3,960,540,900,200,100")
    val text =
      ("Subject,Phase,TrialID,Block,FixNum,FixX,FixY,FixStart,FixDur,NSamples" +: rows)
        .mkString("", "\n", "\n")
    val read          = source(SourceRole.Fixations, "big.csv", text)
    val (w, requests) = run(
      ImportWizard.newImport(t2, ImportPresets.empty),
      t2,
      WizardIntent.SourceRead(read),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.IncludeOccurrence(false)
    )
    val checks = requests.collect { case c: WizardEffect.CheckKey => c }
    // One request per key composition: with Occurrence, then without it.
    assertEquals(checks.map(_.columns.occurrence), Vector(Some(col("Block")), None))
    assertEquals(checks.map(_.path.value).distinct, Vector("big.csv"))
    assertEquals(w.keys.fixations.map(_.result), Some(KeyResult.Pending))
    assertEquals(key(w).lines.map(_.detail), Vector("checking every record…"))
    val (blocked, none) = run(w, t2, WizardIntent.Commit)
    assertEquals(commands(none), Vector.empty)
    assertEquals(
      blocked.problem,
      Some(WizardProblem.TrialKey(KeyBlock.Checking("big.csv")))
    )
    // A stale answer (for the key with Occurrence) changes nothing.
    val (stale, _) = run(w, t2, KeyChecks.run(checks.head, text))
    assertEquals(stale.keys, w.keys)
    // The current answer fills the line and the Studio check.
    val (done, _) = run(w, t2, KeyChecks.run(checks.last, text))
    val line      = key(done).lines.head
    assertEquals(line.count, "5,000 keys")
    assertEquals(
      line.detail,
      "· 1 key repeats without Occurrence · Occurrence left out: 1 for every trial"
    )
    assertEquals(
      line.repeated.map(_.summary),
      Vector("occurrence 1 · records 1–2  |  occurrence 2 · record 10,001")
    )
    assert(TrialKeyVM.keyBlocks(done).exists {
      case KeyBlock.RepeatWithoutOccurrence("big.csv", _, 1, _) => true
      case _                                                    => false
    })
  }

  // --- a project saved before the phase was required -------------------------------------

  test(
    "a revision saved without a phase needs a re-map; committing it without one is refused"
  ) {
    val legacy = ok(LegacyMappings.legacy(t2))
    val golden =
      """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
         |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
         |""".stripMargin
    val own = source(SourceRole.Fixations, "inputs/fixations.csv", golden)
      .copy(bytes = legacy.dataset(DatasetRevision(3)).flatMap(_.sources.fixations).get.bytes)
    val w0 = ok(ImportWizard.remap(legacy, DatasetRevision(3), ImportPresets.empty))
    assertEquals(
      w0.problem,
      Some(WizardProblem.NeedsRemap(DatasetRevision(3), Vector(ColumnRole.Phase)))
    )
    assertEquals(
      ImportWizardVM.of(w0, legacy).problem,
      Some(
        "r3 was saved without a phase column, which import now requires: map one to re-admit it."
      )
    )
    val (w, fx) = run(w0, legacy, WizardIntent.SourceRead(own), WizardIntent.Commit)
    assertEquals(commands(fx), Vector.empty)
    w.problem match
      case Some(WizardProblem.Blocked(errors)) =>
        assertEquals(
          errors.toVector.collect { case MappingError.MissingRole(_, role, _) => role },
          Vector(ColumnRole.Phase)
        )
      case other => fail(s"expected the missing phase, got $other")
    // Mapping the phase column commits the re-map.
    val (fixed, more) = run(
      w,
      legacy,
      WizardIntent
        .Choose(SourceRole.Fixations, col("phase"), ColumnChoice.Role(ColumnRole.Phase)),
      WizardIntent.Commit
    )
    assertEquals(fixed.problem, None)
    commands(more) match
      case Vector(Command.ImportSources(Some(_), _, mapping, _, _, _, _, _)) =>
        assertEquals(mapping.column(ColumnRole.Phase), Some(col("phase")))
      case other => fail(s"expected a re-import, got $other")
  }

  // --- state ----------------------------------------------------------------------------------

  test("a key check is recomputed only when its file or its key columns change") {
    val w          = opened(blocks = true)
    val before     = w.keys.fixations.get
    val (typed, _) =
      run(w, t2, WizardIntent.TypePresetName("x"), WizardIntent.ChooseTab(WizardTab.Geometry))
    assert(typed.keys.fixations.get eq before)
    val (remapped, _) = run(
      w,
      t2,
      WizardIntent.Choose(SourceRole.Fixations, col("FixX"), ColumnChoice.Attribute)
    )
    // A column outside the key changes nothing.
    assert(remapped.keys.fixations.get eq before)
    val (out, _) = run(w, t2, WizardIntent.IncludeOccurrence(false))
    assert(!(out.keys.fixations.get eq before))
  }

  // --- a key change is a "Dataset · re-admit" edit --------------------------------------------

  test("changing an admitted revision's key re-imports it as a new pending revision") {
    // t2's r3 is admitted; its golden file keeps occurrence 1, so leaving it out is allowed.
    val golden =
      """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
         |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
         |P01,Encoding,enc_01,1,2,571.8,835.1,178,314,157
         |""".stripMargin
    val own = source(SourceRole.Fixations, "inputs/fixations.csv", golden)
      .copy(bytes = t2.dataset(DatasetRevision(3)).flatMap(_.sources.fixations).get.bytes)
    val w0      = ok(ImportWizard.remap(t2, DatasetRevision(3), ImportPresets.empty))
    val (w, fx) = run(
      w0,
      t2,
      WizardIntent.SourceRead(own),
      WizardIntent.IncludeOccurrence(false),
      WizardIntent.Commit
    )
    assertEquals(w.problem, None)
    val r3 = t2.dataset(DatasetRevision(3)).get
    commands(fx) match
      case Vector(Command.ImportSources(Some(parent), sources, mapping, _, _, attrs, _, _)) =>
        assertEquals(parent, DatasetRevision(3))
        assertEquals(sources, r3.sources)
        assertEquals(mapping.column(ColumnRole.Occurrence), None)
        assertEquals(attrs.columns, Vector(col("occurrence")))
      case other => fail(s"expected a re-import, got $other")
    val applied = AppModel.run(AppModel.open(t2, None), WizardEffect.appIntents(fx))._1
    assertEquals(applied.document.datasets.last.id, DatasetRevision(4))
    assertEquals(applied.document.datasets.last.decision, AdmissionDecision.Pending)
    assertEquals(applied.document.datasets.last.mapping.column(ColumnRole.Occurrence), None)
  }

  test("changing a pending revision's key is one ReviseDataset") {
    val golden =
      """|participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
         |P01,Encoding,enc_01,1,1,570.4,609.3,70,80,40
         |""".stripMargin
    val own = source(SourceRole.Fixations, "inputs/fixations.csv", golden)
      .copy(bytes = t1.dataset(DatasetRevision(3)).flatMap(_.sources.fixations).get.bytes)
    val w0      = ok(ImportWizard.remap(t1, DatasetRevision(3), ImportPresets.empty))
    val (_, fx) = run(
      w0,
      t1,
      WizardIntent.SourceRead(own),
      WizardIntent.IncludeOccurrence(false),
      WizardIntent.Commit
    )
    commands(fx) match
      case Vector(Command.ReviseDataset(id, mapping, _, _, attrs, _)) =>
        assertEquals(id, DatasetRevision(3))
        assertEquals(mapping.column(ColumnRole.Occurrence), None)
        assertEquals(attrs.columns, Vector(col("occurrence")))
      case other => fail(s"expected one ReviseDataset, got $other")
  }
