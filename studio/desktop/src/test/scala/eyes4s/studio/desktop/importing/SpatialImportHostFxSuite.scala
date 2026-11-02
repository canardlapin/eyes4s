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

import cats.effect.IO
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.desktop.harness.{FxStage, StudioFxSuite}
import eyes4s.studio.desktop.platform.TempDirs
import javafx.scene.layout.StackPane

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.collection.mutable

class SpatialImportHostFxSuite extends StudioFxSuite:
  private val texts = SpatialFixture.texts

  private final class FilesPlatform(paths: Map[SourceRole, Path]) extends ImportPlatform:
    val stored = mutable.ArrayBuffer.empty[String]
    def chooseFile(role: SourceRole): IO[Either[String, Option[ChosenSource]]] =
      IO.pure(
        Right(
          paths
            .get(role)
            .map(path =>
              ChosenSource(HostPath.of(path.toString).toOption.get, path.getFileName.toString)
            )
        )
      )
    def storePreset(
        preset: eyes4s.studio.core.importing.ImportPreset
    ): IO[Either[String, Unit]] = IO.pure(Right(()))
    override def importInput(
        source: eyes4s.studio.core.document.Source,
        path: HostPath
    ): IO[Either[String, Unit]] = IO.delay { stored += source.path.value; Right(()) }

  private def await(fx: FxStage, host: SpatialImportHost): Unit =
    val deadline = System.nanoTime + TimeUnit.SECONDS.toNanos(20)
    while runOnFx(host.view.commit.isDisable) && System.nanoTime < deadline do
      Thread.sleep(20)
      fx.awaitLayout()
    assert(!runOnFx(host.view.commit.isDisable), runOnFx(host.view.problem.getText))

  private def choose(fx: FxStage, host: SpatialImportHost, role: SourceRole): Unit =
    runOnFx(host.view.files(role).fire())
    val deadline = System.nanoTime + TimeUnit.SECONDS.toNanos(20)
    while runOnFx(host.view.files(role).isDisabled) && System.nanoTime < deadline do
      Thread.sleep(20)
      fx.awaitLayout()
    assert(!runOnFx(host.view.files(role).isDisabled), runOnFx(host.view.problem.getText))

  private def mount(fx: FxStage, platform: FilesPlatform) =
    val intents = mutable.ArrayBuffer.empty[Intent]
    var closed  = false
    val host    = runOnFx(
      SpatialImportHost(() => StoryModels.empty, intents += _, platform, () => closed = true)
    )
    runOnFx(fx.scene.setRoot(StackPane(host.view.node)))
    (host, intents, () => closed)

  fxStage.test(
    "the five checked FIGRIM sources store before one spatial append, with no timing"
  ) { fx =>
    val directory = Files.createTempDirectory("eyes4s-spatial-import-")
    try
      val paths =
        (SourceRole.LocationTrials :: SourceRole.Locations :: SourceRole.LocationPairs :: SourceRole.PhaseLedger :: SourceRole.LocationLedger :: Nil)
          .map(role =>
            role -> Files
              .writeString(directory.resolve(s"${role.ordinal}.txt"), texts(role), UTF_8)
          )
          .toMap
      val platform                = FilesPlatform(paths)
      val (host, intents, closed) = mount(fx, platform)
      SourceRole.LocationTrials :: SourceRole.Locations :: SourceRole.LocationPairs :: SourceRole.PhaseLedger :: SourceRole.LocationLedger :: Nil foreach {
        role =>
          choose(fx, host, role)
      }
      await(fx, host)
      assert(runOnFx(host.view.summary.getText).contains("3 maps"))
      assert(runOnFx(host.view.node.lookupAll(".time-select").isEmpty))
      runOnFx(host.view.commit.fire())
      host.importCompleted.get(20, TimeUnit.SECONDS)
      assertEquals(platform.stored.size, 5)
      assert(intents.collect { case Intent.Dispatch(command: Command.AppendLocationDataset) =>
        command
      }.size == 1)
      assert(closed())
    finally TempDirs.remove(directory)
  }

  fxStage.test("a malformed spatial source is refused before storage or append") { fx =>
    val directory = Files.createTempDirectory("eyes4s-spatial-import-bad-")
    try
      val paths = (texts.updated(SourceRole.LocationPairs, "bad\n")).map { (role, text) =>
        role -> Files.writeString(directory.resolve(s"${role.ordinal}.txt"), text, UTF_8)
      }
      val platform                = FilesPlatform(paths)
      val (host, intents, closed) = mount(fx, platform)
      texts.keys.toVector.sortBy(_.ordinal).foreach(role => choose(fx, host, role))
      val deadline = System.nanoTime + TimeUnit.SECONDS.toNanos(20)
      while runOnFx(host.view.problem.getText.isEmpty) && System.nanoTime < deadline do
        Thread.sleep(20)
      assert(runOnFx(host.view.problem.getText).nonEmpty)
      assertEquals(platform.stored.toVector, Vector.empty)
      assertEquals(intents.toVector, Vector.empty)
      assert(!closed())
    finally TempDirs.remove(directory)
  }
