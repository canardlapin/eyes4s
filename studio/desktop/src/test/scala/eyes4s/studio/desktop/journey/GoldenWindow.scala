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

package eyes4s.studio.desktop.journey

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.instances.future.*
import cats.syntax.all.*
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.driver.{DriverRecord, StudioDriver}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.FileProjectStore
import eyes4s.studio.desktop.runtime.SessionPort
import eyes4s.studio.desktop.shell.ShellFxSuite

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*

/** What the window routes of E2E-01 share (S10.1, S10.5b): the headless route
  * they are held to, the project they start from, Repair…'s stand-in image,
  * and the byte-for-byte comparison of their end with the headless route's.
  */
trait GoldenWindow extends ShellFxSuite:

  protected given ExecutionContext = ExecutionContext.global

  protected def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  /** The headless route, run to its end: what the UI route is held to. */
  protected def headless(): GoldenRoute.Route =
    val sessions = Await.result(
      (HeadlessSession.open(StoryMoment.T1), HeadlessSession.open(StoryMoment.T2)).tupled,
      60.seconds
    )
    val (s, views) = sessions
    try
      val route = GoldenRoute.Route(s, views)
      Await.result(
        route.scenario.run(StudioDriver.open(StoryModels.firstRun)),
        120.seconds
      ) match
        case Left(failure) => fail(s"the headless route: ${failure.message}")
        case Right(end)    =>
          assertEquals(end.records.collect { case r: DriverRecord.Refused => r }, Vector.empty)
          route
    finally Await.result((s.close, views.close).tupled, 30.seconds): Unit

  /** A project at `root` holding the fixture's history, its inputs stored. */
  protected def project(root: Path, document: StudioDocument): SessionPort =
    val owner   = ok(LockOwner.of(getClass.getSimpleName))
    val sources =
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv")
    (for
      store  <- FileProjectStore.at[IO](root)
      lock   <- store.acquire(owner).map(ok)
      inputs <- sources.traverse { (role, name) =>
        val bytes = IArray.unsafeFromArray(
          Files.readAllBytes(FixtureDoc.root.resolve(s"fixtures/studio-golden/$name"))
        )
        ProjectBundle.importInput(store, lock, InputKind.Source(role), name, bytes).map(ok)
      }
      _       <- store.release(lock)
      session <- ProjectSession.create(store, owner, document, SharingOptions.complete, inputs)
    yield SessionPort.start(ok(session))).unsafeRunSync()

  /** Answers on a thread of its own, as AssetFiles.chooser's reader does. */
  protected def answerOffFx(answer: => Unit): Unit =
    val t = Thread(() => answer)
    t.setDaemon(true)
    t.start()

  /** Repair…'s file chooser, answered as the headless route answers it: the
    * missing file's restored stand-in, another stimulus's bytes; off the
    * JavaFX thread, as the platform's chooser reads the chosen file.
    */
  protected val standIn: eyes4s.studio.desktop.data.AssetFiles = (file, done) =>
    answerOffFx(
      done(
        Right(
          Some(
            (
              ok(
                eyes4s.studio.core.assets.AssetFile
                  .of(file.value.stripSuffix(".png") + "_restored.png")
              ),
              IArray.unsafeFromArray(
                Files.readAllBytes(
                  FixtureDoc.root.resolve("fixtures/studio-golden/stimuli/beach-042.png")
                )
              )
            )
          )
        )
      )
    )

  /** `ours` is `theirs`, byte for byte; a text file's difference is shown as text. */
  protected def same(name: String, ours: Option[Vector[Byte]], theirs: Vector[Byte]): Unit =
    val text = Set(".json", ".csv", ".md", ".txt", ".journal").exists(name.endsWith)
    if text then
      assertNoDiff(
        ours.fold("<absent>")(b => String(b.toArray, "UTF-8")),
        String(theirs.toArray, "UTF-8"),
        name
      )
    assertEquals(ours, Some(theirs), name)

  /** Every file under `root`, by its path relative to `root`, except the
    * store's lock files (FileProjectStore's .lock, which stays empty after a
    * release, and .lock.owner): volatile, as in the headless route.
    */
  protected def folder(root: Path): Map[String, Vector[Byte]] =
    val all = Files.walk(root)
    try
      all.iterator.asScala
        .filter(Files.isRegularFile(_))
        .filterNot(p =>
          Set(FileProjectStore.LockName, FileProjectStore.OwnerName)(p.getFileName.toString)
        )
        .map(p => root.relativize(p).toString -> Files.readAllBytes(p).toVector)
        .toMap
    finally all.close()

  /** The window route's end held to the headless route's: the document
    * `ui` it closed with, the project folder it saved at `projectDir` and the
    * export bundle it wrote at `bundleDir`.
    */
  protected def heldToHeadless(
      route: GoldenRoute.Route,
      ui: StudioDocument,
      projectDir: Path,
      bundleDir: Path
  ): Unit =
    val closed = route.closed.getOrElse(fail("the headless route did not close"))

    // The project folder, reopened: its inputs are the journey's real
    // inputs (the golden sources and the two repaired images).
    val reopened = (for
      store  <- FileProjectStore.at[IO](projectDir)
      opened <- ProjectBundle.open(store)
    yield opened).unsafeRunSync().fold(e => fail(e.message), identity)
    val inputs = reopened.manifest.inputs
    assertEquals(inputs.count(_.kind == InputKind.StimulusImage), 2, inputs)
    def science(d: StudioDocument) =
      ok(ProjectBundle.encode(d, SharingOptions.complete, inputs)).parts.toMap.view
        .mapValues(Vector.from(_))
        .toMap
    assertEquals(science(ui).keySet, science(closed).keySet)
    science(ui).foreach((path, bytes) => assertEquals(bytes, science(closed)(path), path))
    assertEquals(science(reopened.document), science(closed))

    // The export bundle, every file under it (the project snapshot's
    // project/ included), both ways, byte for byte.
    val written = folder(bundleDir)
    assert(written.keySet.exists(_.endsWith(".svg")), written.keySet)
    assert(written.keySet.exists(_.startsWith("project/inputs/")), written.keySet)
    assertEquals(written.keySet, route.bundle.keySet)
    written.foreach((name, bytes) => same(name, Some(bytes), route.bundle(name)))
    // The project folder, every file, both ways (the lock files excluded,
    // as named in folder()). What the window's manifest lists (itself, its
    // inputs and its parts) is exactly the headless folder, byte for byte.
    // Every other file is the live session's history, named: the save
    // journal, the previous manifest, and the parts of earlier saves (a
    // part's path names its content), none of them listed.
    val saved  = folder(projectDir)
    val listed = Set(ProjectStore.ManifestName) ++
      reopened.manifest.inputs.flatMap(_.path.map(_.value)) ++
      reopened.manifest.parts.all.map(_.path.value)
    assertEquals(listed, route.savedFolder.keySet)
    route.savedFolder.foreach((name, bytes) => same(name, saved.get(name), bytes))
    val sidecars          = Sidecar.values.map(_.fileName).toSet
    def dir(name: String) = name.takeWhile(_ != '/')
    val partDirs          = reopened.manifest.parts.all.map(e => dir(e.path.value)).toSet
    (saved.keySet -- listed).foreach(name =>
      assert(sidecars(name) || (partDirs(dir(name)) && name.endsWith(".json")), name)
    )
