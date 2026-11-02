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

package eyes4s.studio.desktop.locations

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.artifacts.LocationNativePackage
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.importing.SpatialFixture
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.harness.{StudioTheme, SnapshotScale}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Actual document commands, native executor, persistent port and normal four-tab window. */
class SpatialWorkflowFxSuite extends ShellFxSuite:
  override val munitTimeout: Duration           = 180.seconds
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val owner                             = get(LockOwner.of("spatial window regression"))
  private val files                             = SpatialFixture.texts.toVector
    .sortBy(_._1.ordinal)
    .map((role, text) => role -> text.getBytes(UTF_8).toIndexedSeq)
  private val sourceFiles = get(
    LocationBundleSources.of(
      files.map((role, bytes) =>
        Source(
          role,
          get(SourcePath.of(s"${role.toString}.txt")),
          ByteDigest.sha256(IArray.from(bytes)),
          None
        )
      )
    )
  )
  private val admitted = get(
    LocationNativePackage.admit(
      LocationDatasetRevision(
        DatasetRevision(1),
        None,
        sourceFiles,
        LocationProfile.FigrimV1,
        LocationAdmissionDecision.Pending
      ),
      files
    )
  )
  private val document = get(
    StudioDocument.of(
      Vector(admitted.dataset),
      Vector.empty,
      None,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      Vector.empty
    )
  )

  private def create(): (ProjectStore[IO], ProjectSession[IO]) = (for
    store   <- InMemoryProjectStore.create[IO]
    lock    <- store.acquire(owner).map(get)
    entries <- files
      .traverse((role, bytes) =>
        ProjectBundle
          .importInput(
            store,
            lock,
            InputKind.Source(role),
            sourceFiles.entries.find(_.role == role).get.path.value,
            IArray.from(bytes)
          )
          .map(get)
      )
      .guarantee(store.release(lock).void)
    session <- ProjectSession
      .create(store, owner, document, SharingOptions.complete, entries)
      .map(get)
  yield store -> session).unsafeRunSync()

  private def text(node: javafx.scene.Node): String = node match
    case label: javafx.scene.control.Labeled => Option(label.getText).getOrElse("")
    case parent: javafx.scene.Parent         =>
      parent.getChildrenUnmodifiable.asScala.map(text).mkString("\n")
    case _ => ""

  fxStage.test(
    "spatial dataset runs in main project, shows Results and Report, and cold reopens"
  ) { fx =>
    val (store, session) = create()
    val port             = SessionPort.start(session, message => fail(message))
    val w                = boot(
      fx,
      AppModel.open(document, None),
      project = Some(port),
      nativeSources = Some(DatasetSourceHosts.stored(port))
    )
    eventually(fx, "verified spatial admission") { !w.spatial.createAnalysis.isDisabled }
    assertEquals(runOnFx(w.runtime.model.layout.id.value), "data.locations")
    runOnFx(w.spatial.createAnalysis.fire())
    eventually(fx, "saved spatial analysis") {
      w.runtime.model.document.analyses.nonEmpty && !w.spatial.run.isDisabled
    }
    assertEquals(runOnFx(w.runtime.model.layout.id.value), "analysis.locations")
    assertEquals(runOnFx(w.runtime.model.document.draft), None)
    runOnFx(w.spatial.run.fire())
    eventually(fx, "completed stored spatial run") {
      w.runtime.model.document.runs.lastOption.exists(r =>
        r.state == RunLifecycle.Completed && r.archive.isInstanceOf[CoreBinding.Bound[?]]
      )
    }
    val run = runOnFx(w.runtime.model.document.runs.last.id)
    dispatch(fx, w, Intent.Dispatch(Command.ShowRun(Some(run))))
    dispatch(fx, w, Intent.SwitchPerspective(Perspective.Results))
    eventually(fx, "stored spatial score table") { w.spatial.scores.getItems.size == 1 }
    assertEquals(runOnFx(w.runtime.model.layout.id.value), "results.locations")
    assertEquals(runOnFx(w.spatial.scores.getItems.get(0).difference), Right(1.0))
    assert(runOnFx(text(w.spatial.summary)).contains("1 missing phases"))
    dispatch(fx, w, Intent.SwitchPerspective(Perspective.Report))
    assertEquals(runOnFx(w.runtime.model.layout.id.value), "report.locations")
    assert(runOnFx(text(w.spatial.report)).contains("Equal-query mean D: 1"))
    assertEquals(runOnFx(w.runtime.model.document.reporting), Vector.empty)
    assert(!runOnFx(text(w.root)).contains("No data"))
    fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1)): Unit
    val saved = CompletableFuture[Either[String, eyes4s.studio.core.session.SaveReceipt]]()
    port.save(answer => { saved.complete(answer): Unit })
    get(saved.get(20, TimeUnit.SECONDS)): Unit
    runOnFx(w.close())
    get(session.close.unsafeRunSync())
    val openedProject = get(ProjectSession.open(store, owner).unsafeRunSync())
    val reopenedDoc   = openedProject.session.document.unsafeRunSync()
    val reopenedPort  = SessionPort.start(openedProject.session, message => fail(message))
    val reopened      = boot(
      fx,
      AppModel.open(reopenedDoc, None),
      project = Some(reopenedPort),
      nativeSources = Some(DatasetSourceHosts.stored(reopenedPort))
    )
    eventually(fx, "cold restored report") {
      text(reopened.spatial.report).contains("Equal-query mean D: 1")
    }
    assertEquals(reopened.session.await(reopened.session.backend.jobs).size, 0)
    dispatch(fx, reopened, Intent.SwitchPerspective(Perspective.Results))
    eventually(fx, "cold restored scores") { reopened.spatial.scores.getItems.size == 1 }
    assertEquals(runOnFx(reopened.spatial.scores.getItems.get(0).difference), Right(1.0))
    runOnFx(reopened.close())
    get(openedProject.session.close.unsafeRunSync())
  }
