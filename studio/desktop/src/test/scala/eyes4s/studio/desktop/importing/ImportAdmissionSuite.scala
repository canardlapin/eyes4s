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

package eyes4s.studio.desktop.importing

import eyes4s.plan.{AttributeColumn, AttributeKind}
import eyes4s.studio.app.driver.{DriverRecord, StudioDriver}
import eyes4s.studio.app.importing.{ImportWizard, WizardEffect, WizardIntent}
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.importing.{ColumnChoice, ImportPresets, SniffedSource}
import eyes4s.studio.desktop.trial.GoldenTrials

import java.nio.file.Files
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

/** End to end on the fake backend (ticket S5.2): a column the wizard leaves
  * without a role reaches the admitted dataset as a declared attribute, so
  * UI-H admission (which reads only declared columns) keeps it. Story moment
  * t1: pending r3, the fixture's own fixations.csv, served by
  * FakeStudyBackend.
  */
class ImportAdmissionSuite extends munit.FunSuite:
  import StoryModels.{ok, t1}

  override val munitTimeout: Duration = 60.seconds

  private given ExecutionContext = ExecutionContext.global

  private val r3 = DatasetRevision(3)

  test("an unknown-role column passes through verification and admission as an attribute") {
    val bytes =
      IArray.unsafeFromArray(Files.readAllBytes(GoldenTrials.golden.resolve("fixations.csv")))
    val source     = ok(SniffedSource.read(SourceRole.Fixations, "inputs/fixations.csv", bytes))
    val column     = ok(ColumnName.of("occurrence"))
    val trialBytes =
      IArray.unsafeFromArray(Files.readAllBytes(GoldenTrials.golden.resolve("trials.csv")))
    val trials = ok(SniffedSource.read(SourceRole.Trials, "inputs/trials.csv", trialBytes))
    // The wizard re-maps r3: occurrence loses its role in both files, which
    // must name the trial by the same key (S5.4 follow-up), and passes through.
    val wizard       = ok(ImportWizard.remap(t1, r3, ImportPresets.empty))
    val (_, effects) = Vector(
      WizardIntent.SourceRead(source),
      WizardIntent.SourceRead(trials),
      WizardIntent.Choose(SourceRole.Fixations, column, ColumnChoice.Attribute),
      WizardIntent.Choose(SourceRole.Trials, column, ColumnChoice.Attribute),
      WizardIntent.Commit
    ).foldLeft((wizard, Vector.empty[WizardEffect])) { case ((w, fx), i) =>
      val (next, more) = ImportWizard.update(w, i, t1)
      (next, fx ++ more)
    }
    val remapped = StudioDriver
      .open(AppModel.open(t1, None))
      .dispatchAll(WizardEffect.appIntents(effects))
    assertEquals(remapped.model.notice, None)
    HeadlessSession.open(StoryMoment.T1).flatMap { session =>
      val flow =
        for
          verifying <- Future.fromTry(
            remapped
              .command(Command.VerifyDataset(r3))
              .left
              .map(e => Exception(e.message))
              .toTry
          )
          requested = verifying.model.document.dataset(r3).map(_.decision) match
            case Some(AdmissionDecision.Verifying(c)) => c
            case other                                => fail(s"r3 is not verifying: $other")
          // The backend holds the re-mapped r3 as the saved project stores it
          // (protocol 1.9: it verifies only content it holds).
          _       <- session.holdContent(r3, requested)
          settled <- StudioDriver.settle(verifying, session)
        yield
          assertEquals(
            settled.records.collect { case r: DriverRecord.Refused => r },
            Vector.empty
          )
          assertEquals(settled.admissions.map(_.dataset), Vector(r3))
          val content = settled.model.document.dataset(r3).map(_.decision) match
            case Some(AdmissionDecision.Verifying(c)) => c
            case other                                => fail(s"r3 is not verifying: $other")
          val admitted = settled
            .command(
              Command.Admit(
                r3,
                content,
                Some(eyes4s.plan.AdmissionDecision.ReviewExclusions),
                CoreBinding.unbound,
                CoreBinding.unbound
              )
            )
            .fold(e => fail(e.message), identity)
          val spec = admitted.model.document.dataset(r3).get
          assert(spec.decision.isAdmitted)
          assertEquals(spec.mapping.column(ColumnRole.Occurrence), None)
          // What UI-H admission is given: the declared attribute columns.
          assertEquals(
            spec.attributes.core,
            Vector(AttributeColumn("occurrence", AttributeKind.Text))
          )
      flow.transformWith(result => session.close.transform(_ => result))
    }
  }

  test(
    "wizard display mapping survives project reopen and serves a real stored-source asset registry"
  ) {
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global
    import eyes4s.studio.core.assets.{AssetRegistry, DisplayKind, DisplayState}
    import eyes4s.studio.core.bundle.{InputEntry, InputKind, LockOwner, SharingOptions}
    import eyes4s.studio.core.session.ProjectSession
    import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
    import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
    import java.nio.charset.StandardCharsets.UTF_8
    import scala.jdk.CollectionConverters.*

    val document      = StoryModels.empty
    val fixationBytes =
      IArray.unsafeFromArray(Files.readAllBytes(GoldenTrials.golden.resolve("fixations.csv")))
    // Rename the real fixture's display columns: no golden-digest shortcut can serve this import.
    val trialText = Files
      .readString(GoldenTrials.golden.resolve("trials.csv"), UTF_8)
      .replace("display_kind,image_file", "presentation,asset_path")
    val trialBytes = IArray.from(trialText.getBytes(UTF_8))
    val source     =
      ok(SniffedSource.read(SourceRole.Fixations, "inputs/fixations.csv", fixationBytes))
    val trials   = ok(SniffedSource.read(SourceRole.Trials, "inputs/trials.csv", trialBytes))
    val displays =
      DisplayColumns(ok(ColumnName.of("presentation")), Some(ok(ColumnName.of("asset_path"))))
    val geometry =
      eyes4s.studio.core.importing.GeometryFields.of(StoryModels.t2.datasets.last.geometry)
    val initial = eyes4s.studio.core.importing.GeometryField.values
      .foldLeft(ImportWizard.newImport(document, ImportPresets.empty)) { (w, f) =>
        ImportWizard.update(w, WizardIntent.EditGeometry(f, geometry.field(f)), document)._1
      }
    val ready = Vector(
      WizardIntent.SourceRead(source),
      WizardIntent.SourceRead(trials),
      WizardIntent.DeclareTime(Some(TimeUnit.Milliseconds)),
      WizardIntent.DeclareDisplays(Some(displays))
    ).foldLeft(initial)((w, i) => ImportWizard.update(w, i, document)._1)
    val commands = ok(ImportWizard.commands(ready, document))
    val imported = AppModel
      .run(AppModel.open(document, None), commands.map(eyes4s.studio.app.Intent.Dispatch(_)))
      ._1
      .document
    val dir   = Files.createTempDirectory("eyes4s-display-import")
    val owner = ok(LockOwner.of("ImportAdmissionSuite"))
    val store = FileProjectStore.at[IO](dir.resolve("mapped.eyes")).unsafeRunSync()
    try
      val inputs = Vector(source -> fixationBytes, trials -> trialBytes).map { (s, bytes) =>
        ok(
          InputEntry.of(
            InputKind.Source(s.role),
            s.path.value.split('/').last,
            s.bytes,
            bytes.length.toLong
          )
        )
      }
      val session = ok(
        ProjectSession
          .create(store, owner, imported, SharingOptions.complete, inputs)
          .unsafeRunSync()
      )
      try
        ok(
          session
            .importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixationBytes)
            .unsafeRunSync()
        )
        ok(
          session
            .importInput(InputKind.Source(SourceRole.Trials), "trials.csv", trialBytes)
            .unsafeRunSync()
        )
        val images = Files.list(GoldenTrials.golden.resolve("stimuli"))
        try
          images.iterator.asScala.filter(Files.isRegularFile(_)).foreach { path =>
            ok(
              session
                .importInput(
                  InputKind.StimulusImage,
                  path.getFileName.toString,
                  IArray.unsafeFromArray(Files.readAllBytes(path))
                )
                .unsafeRunSync()
            )
          }
        finally images.close()
        ok(session.save.unsafeRunSync())
      finally ok(session.close.unsafeRunSync())
      val reopened = ok(ProjectSession.open(store, owner).unsafeRunSync()).session
      val port     = SessionPort.start(reopened)
      try
        val saved = reopened.document.unsafeRunSync()
        val spec  = saved.datasets.last
        assertEquals(spec.inventory.flatMap(_.displays), Some(displays))
        assert(!eyes4s.studio.core.fixture.GoldenAssets.describes(spec))
        val registry = DatasetSourceHosts
          .stored(port)
          .assets(spec)
          .unsafeRunSync()
          .getOrElse(fail("no registry after reopening"))
        assertEquals(registry.count(DisplayKind.Image), 480)
        assertEquals(registry.count(DisplayKind.BlankWithFixationCross), 480)
        assertEquals(registry.summary.present, 257)
        assertEquals(
          registry.summary.missing.map(_.file.value),
          Vector("forest-044.png", "kitchen-081.png")
        )
        assertEquals(registry.trials.count(_.state == DisplayState.BlankWithFixationCross), 480)
        assertEquals(AssetRegistry.check(registry, spec), Right(()))
      finally
        port.close()
        ok(reopened.close.unsafeRunSync())
    finally TempDirs.remove(dir)
  }
