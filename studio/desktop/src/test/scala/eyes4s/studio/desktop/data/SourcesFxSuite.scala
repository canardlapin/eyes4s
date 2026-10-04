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

package eyes4s.studio.desktop.data

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.assets.AssetFile
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.core.session.SaveReceipt
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.runtime.ProjectPort
import eyes4s.studio.desktop.shell.ShellFxSuite

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.Duration

/** The Data perspective's Sources pane in a studio window at t1 (ticket
  * S5.7; Data.dc.html, left): r3's sources, 257 of 259 images found, what
  * each trial displayed, the two missing files, and Repair… storing a chosen
  * image in the project and relinking it, after which the counts move.
  */
class SourcesFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(120, "s")

  /** A project that stores what it is given and records it. */
  private final class Stores extends ProjectPort:
    val stored = scala.collection.mutable.ArrayBuffer.empty[(InputKind, String, Int)]
    def journal(entry: JournalEntry): Unit                    = ()
    def save(done: Either[String, SaveReceipt] => Unit): Unit = done(
      Left("not saved in a test")
    )
    def close(): Unit = ()
    override def importInput(
        kind: InputKind,
        name: String,
        bytes: IArray[Byte],
        done: Either[String, Unit] => Unit
    ): Unit =
      stored += ((kind, name, bytes.length))
      done(Right(()))

  private val chosen: AssetFiles = (_, done) =>
    done(
      Right(
        Some(
          AssetFile.of("forest_044_found.png").toOption.get -> IArray.from(
            "png".getBytes(UTF_8)
          )
        )
      )
    )

  private def ready(
      fx: FxStage,
      project: Option[ProjectPort],
      files: AssetFiles
  ): StudioWindow =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = project, assetFiles = files)
    eventually(fx, "the displays")(w.sources.vm.missing.isDefined)
    w

  fxStage.test("t1: r3's sources, 257 of 259 images found, what each trial displayed") { fx =>
    val w    = ready(fx, None, chosen)
    val pane = runOnFx(w.host.node(StudioLayouts.sources)).getOrElse(fail("no Sources pane"))
    assert(runOnFx(pane.getScene eq fx.scene))
    val v = w.sources.view
    assertEquals(
      runOnFx(v.sourceLines).map(_.take(3)),
      Vector(
        Vector("fixations.csv", "Fixations", "byte digest recorded · copied into project"),
        Vector("trials.csv", "Inventory", "960 trials · inventory"),
        Vector("stimuli/", "Images", "257 of 259 images found")
      )
    )
    assertEquals(runOnFx(v.title.getText), "What each trial displayed")
    assertEquals(
      runOnFx(v.kindRows),
      Vector(
        ("Image · Encoding", "476"),
        ("Blank + fixation cross · Retrieval", "480"),
        ("Blank · Cue · Unknown", "0")
      )
    )
    assertEquals(runOnFx(v.missingTitle.getText), "2 image files missing")
    assertEquals(
      runOnFx(v.missingBody.getText),
      "forest-044.png, kitchen-081.png. Their 4 Encoding trials (P01 enc_08, enc_15; " +
        "P24 enc_05, enc_18) are drawn as missing asset, never as blank. Gaze and scores are unaffected."
    )
    assertEquals(
      runOnFx((v.repair.getText, v.showTrials.getText)),
      ("Repair…", "Show 4 trials")
    )
    // Without a project, Repair… says it cannot store an image.
    runOnFx(v.repair.fire())
    fx.awaitLayout()
    assertEquals(
      runOnFx(v.note.getText),
      "forest-044.png could not be repaired: Save the project to store a repaired image."
    )
  }

  fxStage.test("Repair… stores the chosen image and relinks forest-044; the counts move") {
    fx =>
      val project = Stores()
      val w       = ready(fx, Some(project), chosen)
      val v       = w.sources.view
      runOnFx(v.repair.fire())
      eventually(fx, "the relink")(
        w.runtime.model.document.relinks.of(StoryMoments.r3).nonEmpty
      )
      assertEquals(
        project.stored.toVector,
        Vector((InputKind.StimulusImage, "forest_044_found.png", 3))
      )
      val relink = runOnFx(w.runtime.model.document.relinks.of(StoryMoments.r3)).head
      assertEquals(
        (relink.file.value, relink.asset.file.value),
        ("forest-044.png", "forest_044_found.png")
      )
      eventually(fx, "the counts")(
        w.sources.view.kindRows.headOption.contains(("Image · Encoding", "478"))
      )
      assertEquals(runOnFx(v.missingTitle.getText), "1 image file missing")
      assertEquals(runOnFx(v.sourceLines(2)(2)), "258 of 259 images found")
      assertEquals(runOnFx(v.note.getText), "forest-044.png repaired with forest_044_found.png")
      // Show 2 trials opens the first in Explore.
      runOnFx(v.showTrials.fire())
      fx.awaitLayout()
      assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Explore)
      assertEquals(
        runOnFx(w.runtime.model.location.trail.last),
        Place.At(StudioRef.Trial(TrialKey("P01", Phase.Encoding, "enc_15", 1)))
      )
  }
