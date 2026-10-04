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
    val source = ok(SniffedSource.read(SourceRole.Fixations, "inputs/fixations.csv", bytes))
    val column = ok(ColumnName.of("occurrence"))
    // The wizard re-maps r3: occurrence loses its role and passes through.
    val wizard       = ok(ImportWizard.remap(t1, r3, ImportPresets.empty))
    val (_, effects) = Vector(
      WizardIntent.SourceRead(source),
      WizardIntent.Choose(SourceRole.Fixations, column, ColumnChoice.Attribute),
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
