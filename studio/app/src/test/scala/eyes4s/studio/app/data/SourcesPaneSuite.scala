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

package eyes4s.studio.app.data

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.assets.{AssetFile, AssetRef, DisplayKind}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import eyes4s.studio.core.selection.{DisplayCount, InventoryKind, StudioRef}

/** The Data perspective's Sources pane, headless (ticket S5.7; Data.dc.html,
  * left), on t2's r3 with the golden registry: 960 trials, 257 of 259 images
  * found, 476 encoding images and 480 retrieval blanks with a cross shown,
  * forest-044 and kitchen-081 missing for P01 and P24, and Repair… recording
  * a relink that the counts then reflect.
  */
class SourcesPaneSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val model = StoryModels.t2Explore
  private val r3    = StoryMoments.r3

  private def file(name: String) = ok(AssetFile.of(name))

  /** The pane with r3's golden registry read. */
  private def loaded(m: AppModel): SourcesPane =
    val (pane, effects) = SourcesPane.sync(SourcesPane.empty, m)
    val spec            = m.document.dataset(r3).get
    assertEquals(effects, Vector(SourcesEffect.ReadRegistry(spec, 1)))
    val served = GoldenAssets.registry(spec).map(DisplaySource.Served(_))
    SourcesPane.update(pane, m, SourcesIntent.RegistryRead(r3, 1, served))._1

  private def tally(c: DisplayCount): StudioRef = StudioRef.DisplayTally(r3, c)

  test(
    "the sources, what each trial displayed, and the two missing files, each with its refs"
  ) {
    val vm = SourcesVM.of(loaded(model), model)
    assertEquals(
      vm.sources.map(s => (s.name, s.kind, s.count)),
      Vector(
        ("fixations.csv", "Fixations", None),
        ("trials.csv", "Inventory", Some("960 trials · inventory")),
        ("stimuli/", "Images", Some("257 of 259 images found"))
      )
    )
    assertEquals(
      vm.sources(1).refs,
      Vector(StudioRef.InventoryCount(r3, InventoryKind.Inventory))
    )
    assertEquals(
      vm.sources(2).refs,
      Vector(tally(DisplayCount.ImagesFound), tally(DisplayCount.ImagesNamed))
    )
    assertEquals(vm.displaysTitle, "What each trial displayed")
    assertEquals(
      vm.displays.map(d => (d.label, d.count, d.shown)),
      Vector(
        ("Image · Encoding", "476", true),
        ("Blank + fixation cross · Retrieval", "480", true),
        ("Blank · Cue · Unknown", "0", false)
      )
    )
    assertEquals(
      vm.displays.head.refs,
      Vector(tally(DisplayCount.Shown(DisplayKind.Image, Phase.Encoding)))
    )
    val missing = vm.missing.getOrElse(fail("no missing files"))
    assertEquals(missing.title, "2 image files missing")
    assertEquals(
      missing.body,
      "forest-044.png, kitchen-081.png. Their 4 Encoding trials (P01 enc_08, enc_15; " +
        "P24 enc_05, enc_18) are drawn as missing asset, never as blank. Gaze and scores are unaffected."
    )
    assertEquals((missing.repair, missing.showTrials), ("Repair…", "Show 4 trials"))
    assertEquals(
      missing.refs,
      Vector(tally(DisplayCount.MissingFiles), tally(DisplayCount.MissingTrials))
    )
    // A found count lies under the named count; missing trials under missing files.
    assertEquals(tally(DisplayCount.ImagesFound).parent, Some(tally(DisplayCount.ImagesNamed)))
    assert(tally(DisplayCount.MissingTrials).isAggregate)
  }

  test("Repair… locates the first missing file; the stored file is recorded as a relink") {
    val pane            = loaded(model)
    val (asked, locate) = SourcesPane.update(pane, model, SourcesIntent.Repair)
    val forest          = file("forest-044.png")
    assertEquals(locate, Vector(SourcesEffect.Locate(r3, forest)))
    val sha                = ByteDigest.sha256(IArray.from("restored".getBytes("UTF-8")))
    val restored           = file("forest_044_restored.png")
    val (located, effects) =
      SourcesPane.update(asked, model, SourcesIntent.Located(r3, forest, restored, sha))
    val command = Command.RelinkAsset(r3, forest, Some(AssetRef(restored, sha)))
    assertEquals(effects, Vector(SourcesEffect.App(Intent.Dispatch(command))))
    assertEquals(located.note, Some("forest-044.png repaired with forest_044_restored.png"))
    // With the relink in the document, the counts are the repaired registry's.
    val repaired = AppModel.update(model, Intent.Dispatch(command))._1
    assertEquals(repaired.document.relinks.of(r3).size, 1)
    val vm = SourcesVM.of(located, repaired)
    assertEquals(vm.sources(2).count, Some("258 of 259 images found"))
    assertEquals(vm.displays.head.count, "478")
    assertEquals(vm.missing.map(_.title), Some("1 image file missing"))
    assertEquals(vm.missing.map(_.files), Some(Vector(file("kitchen-081.png"))))
    // A failure is said, naming the file.
    val failed =
      SourcesPane.update(asked, model, SourcesIntent.NotLocated(forest, "cancelled"))._1
    assertEquals(
      SourcesVM.of(failed, model).note,
      Some("forest-044.png could not be repaired: cancelled")
    )
  }

  test(
    "Show 4 trials opens the first in Explore; an old read is ignored; a failed read retries"
  ) {
    val pane = loaded(model)
    assertEquals(
      SourcesPane.update(pane, model, SourcesIntent.ShowTrials)._2,
      Vector(
        SourcesEffect.App(
          Intent.Navigate(
            Location(
              Perspective.Explore,
              Vector(Place.At(StudioRef.Trial(TrialKey("P01", Phase.Encoding, "enc_08", 1))))
            )
          )
        )
      )
    )
    val stale =
      SourcesPane.update(pane, model, SourcesIntent.RegistryRead(r3, 0, Left("old")))._1
    assertEquals(stale, pane)
    val (asked, _) = SourcesPane.sync(SourcesPane.empty, model)
    val failed     =
      SourcesPane.update(asked, model, SourcesIntent.RegistryRead(r3, 1, Left("no project")))._1
    val vm = SourcesVM.of(failed, model)
    assertEquals(
      (vm.status, vm.retry),
      (Some("The trial displays cannot be read: no project"), true)
    )
    assertEquals(
      SourcesPane.update(failed, model, SourcesIntent.Retry),
      (
        failed.copy(ask = 2, registry = Loading.Waiting),
        Vector(SourcesEffect.ReadRegistry(failed.dataset.get, 2))
      )
    )
    // Not served: the trials are counted nowhere, and the pane says so.
    val notServed =
      SourcesPane
        .update(asked, model, SourcesIntent.RegistryRead(r3, 1, Right(DisplaySource.NotServed)))
        ._1
    val ns = SourcesVM.of(notServed, model)
    assertEquals(ns.sources(2).count, Some("Display kinds are not served for this revision"))
    assertEquals((ns.displays, ns.missing), (Vector.empty, None))
  }
