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

package eyes4s.studio.desktop.explore

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.core.bundle.{InputEntry, InputKind}
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.{
  ColumnName,
  DatasetRevisionSpec,
  DisplayColumns,
  InventoryMapping,
  Source,
  SourceRole,
  Sources
}
import eyes4s.studio.core.session.SaveReceipt
import eyes4s.studio.desktop.runtime.ProjectPort

import java.nio.charset.StandardCharsets.UTF_8
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.desktop.StudioMain

/** Where the trials navigator's display kinds come from (tickets S6.1 and
  * S5.7): a story session (the application's, on the fake backend) serves
  * the golden registry, and only for the golden trial inventory, byte for
  * byte; a real-backend session serves its project's own stored inventory
  * and stimulus images, and none without a project. Headless.
  */
class NavigatorDisplaysSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val r3 = StoryModels.t2.dataset(StoryMoments.r3).get

  /** r3 with its trials.csv replaced by other bytes under the same name. */
  private val otherTrials =
    val other = ByteDigest.parse("ab" * 32).toOption.get
    r3.copy(sources =
      Sources
        .of(r3.sources.entries.map {
          case s if s.role == SourceRole.Trials => s.copy(bytes = other)
          case s                                => s
        })
        .toOption
        .get
    )

  /** What `displays` answers for `dataset` (these answer at once). */
  private def read(displays: NavigatorDisplays, dataset: DatasetRevisionSpec) =
    var answer = Option.empty[Either[String, DisplaySource]]
    displays.read(dataset, a => answer = Some(a))
    answer.getOrElse(fail("no answer"))

  test("the application's story session serves the golden registry, and only for it") {
    assertEquals(StudioMain.backend, SessionBackend.Story)
    assertEquals(StudioMain.displays, NavigatorDisplays.golden)
    assert(
      read(StudioMain.displays, r3).exists {
        case DisplaySource.Served(_) => true
        case DisplaySource.NotServed => false
      }
    )
    assertEquals(read(StudioMain.displays, otherTrials), Right(DisplaySource.NotServed))
  }

  test("a real-backend session without a project serves no display kinds") {
    val real = NavigatorDisplays.of(SessionBackend.Real, None)
    assertEquals(real, NavigatorDisplays.notServed)
    assertEquals(read(real, r3), Right(DisplaySource.NotServed))
  }

  test("the golden registry is served only for the golden trial inventory") {
    read(NavigatorDisplays.golden, r3) match
      case Right(DisplaySource.Served(registry)) =>
        assertEquals(registry.dataset, StoryMoments.r3)
        assertEquals(registry.summary.missing.size, 2)
      case other => fail(s"expected the golden registry, got $other")
    assertEquals(read(NavigatorDisplays.golden, otherTrials), Right(DisplaySource.NotServed))
    // Without a trial inventory there is nothing to describe.
    val noTrials = r3.copy(sources =
      Sources.of(r3.sources.entries.filter(_.role != SourceRole.Trials)).toOption.get
    )
    assertEquals(read(NavigatorDisplays.golden, noTrials), Right(DisplaySource.NotServed))
  }

  // --- A real project's own displays (S5.7) ----------------------------------------------

  private val csv =
    """participant,phase,trial,occurrence,item,display_kind,image_file,response
      |P01,Encoding,enc_01,1,beach-042,image,beach-042.png,
      |P01,Encoding,enc_02,1,forest-044,image,forest-044.png,
      |P01,Retrieval,ret_01,1,beach-042,blank+fixation-cross,,remembered
      |""".stripMargin
  private val csvBytes = IArray.from(csv.getBytes(UTF_8))

  /** r3 storing `csv` as its trial inventory, its display columns mapped. */
  private val mapped =
    val sources = ok(Sources.of(r3.sources.entries.map {
      case s if s.role == SourceRole.Trials => s.copy(bytes = ByteDigest.sha256(csvBytes))
      case s                                => s
    }))
    val columns =
      DisplayColumns(ok(ColumnName.of("display_kind")), Some(ok(ColumnName.of("image_file"))))
    r3.copy(
      sources = sources,
      inventory = r3.inventory.map(m => ok(InventoryMapping.withDisplays(m, Some(columns))))
    )

  /** A project storing `csv` and one stimulus image, beach-042.png. */
  private final class Project(bytes: IArray[Byte]) extends ProjectPort:
    def journal(entry: JournalEntry): Unit                    = ()
    def save(done: Either[String, SaveReceipt] => Unit): Unit = done(Left("not in a test"))
    def close(): Unit                                         = ()
    override def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
      done(Right(bytes))
    override def storedInputs(done: Either[String, Vector[InputEntry]] => Unit): Unit =
      done(
        InputEntry
          .of(
            InputKind.StimulusImage,
            "beach-042.png",
            ByteDigest.sha256(IArray.from("png".getBytes(UTF_8))),
            3L
          )
          .left
          .map(_.message)
          .map(Vector(_))
      )

  test("a real project serves the registry its stored trials.csv and images state") {
    val project = NavigatorDisplays.of(SessionBackend.Real, Some(Project(csvBytes)))
    read(project, mapped) match
      case Right(DisplaySource.Served(r)) =>
        assertEquals(r.dataset, StoryMoments.r3)
        assertEquals((r.summary.files, r.summary.present), (2, 1))
        assertEquals(r.missing.map(_.file.value), Vector("forest-044.png"))
      case other => fail(s"expected a registry, got $other")
    // A revision that maps no display columns has none to serve.
    assertEquals(read(project, r3), Right(DisplaySource.NotServed))
    // Bytes that are not the source's are refused, not served.
    val changed =
      NavigatorDisplays.of(SessionBackend.Real, Some(Project(IArray.from("x".getBytes(UTF_8)))))
    assert(
      read(changed, mapped).left.exists(_.contains("its source recorded")),
      read(changed, mapped)
    )
  }
