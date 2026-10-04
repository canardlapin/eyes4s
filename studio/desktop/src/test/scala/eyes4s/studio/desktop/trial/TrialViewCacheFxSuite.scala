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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.assets.{AssetLink, AssetRef, TrialDisplay}
import eyes4s.studio.desktop.harness.{FxStage, StageSize, StudioFxSuite}
import eyes4s.studio.viz.trial.*
import javafx.scene.layout.StackPane

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration

/** The trial view's stimulus loading (S8.2, from the S4.3a review): a failed
  * read keeps its typed StimulusError, retry reads it again, and the decoded
  * rasters held are bounded, never dropping the one the trial draws.
  */
class TrialViewCacheFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(480, 360)

  override val munitTimeout: Duration = Duration(120, "s")

  private val golden: StimulusSource = StimulusSource.directory(GoldenTrials.stimuli)

  private def input(d: TrialDisplay) =
    TrialSceneInput(
      d,
      GoldenTrials.screen,
      Vector.empty,
      MarkStyle.Role(TrialRole.Matched),
      Theme.Light,
      StageVariant.Dark
    )

  private def assetOf(d: TrialDisplay): AssetRef = d.display.link match
    case Some(AssetLink.Present(a)) => a
    case other                      => fail(s"${d.trial.label} has no stored image: $other")

  private def eventually(fx: FxStage, what: String)(cond: => Boolean): Unit =
    val deadline = System.nanoTime + 20_000_000_000L
    while !runOnFx(cond) do
      if System.nanoTime > deadline then fail(s"timed out waiting for $what")
      Thread.sleep(20)
      fx.awaitLayout()

  private def loaded(v: TrialView, a: AssetRef): Boolean = v.status.get match
    case TrialViewStatus.Shown(s) => s.frameArt == FrameArt.Picture(a)
    case _                        => false

  fxStage.test("a failed read keeps its typed error; retry reads it again") { fx =>
    val enc03 = GoldenTrials.display("P17", "enc_03")
    val asset = assetOf(enc03)
    val reads = AtomicInteger(0)
    // The first read fails as unreadable; later reads succeed.
    val flaky: StimulusSource = a =>
      if reads.getAndIncrement() == 0 then Left(StimulusError.Unreadable(a.file, "busy"))
      else golden.bytes(a)
    val view = runOnFx(TrialView(flaky))
    fx.show(runOnFx(StackPane(view)))
    runOnFx(view.show(input(enc03)))
    eventually(fx, "the failure is recorded")(!view.stimulusFailures.get.isEmpty)
    assertEquals(
      runOnFx(view.stimulusFailures.get),
      Map(asset -> StimulusError.Unreadable(asset.file, "busy"))
    )
    runOnFx(view.retry())
    eventually(fx, "the image is loaded after retry")(loaded(view, asset))
    assertEquals(runOnFx(view.stimulusFailures.get), Map.empty)
    runOnFx(view.dispose())
  }

  fxStage.test("the decoded rasters held are bounded and keep the one drawn") { fx =>
    val trials = (1 to 9)
      .map(i => GoldenTrials.display("P17", f"enc_$i%02d"))
      .toVector
      .filter(_.display.link.exists { case AssetLink.Present(_) => true; case _ => false })
    assert(trials.size > TrialView.RasterLimit, trials.size)
    val view = runOnFx(TrialView(golden))
    fx.show(runOnFx(StackPane(view)))
    trials.foreach { d =>
      runOnFx(view.show(input(d)))
      eventually(fx, s"${d.trial.label} is loaded")(loaded(view, assetOf(d)))
      val held = runOnFx(view.heldRasters)
      assert(held.size <= TrialView.RasterLimit, held)
      assert(held.contains(assetOf(d)), (d.trial.label, held))
    }
    // The first trial's raster was let go; showing it again reads it again.
    val first = trials.head
    assert(!runOnFx(view.heldRasters).contains(assetOf(first)))
    runOnFx(view.show(input(first)))
    eventually(fx, "the first trial is loaded again")(loaded(view, assetOf(first)))
    runOnFx(view.dispose())
  }
