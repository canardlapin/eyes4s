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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.desktop.{StudioStyles, StudioWindow}
import eyes4s.studio.desktop.harness.StudioTheme
import javafx.scene.Node

/** The finished shell chrome (S1.6–S1.9, S1.11) at every perspective of the
  * three story moments (the S0.8 seeds): the drawn texts equal the
  * view-models', and each state is snapshotted in light, plus dark for
  * Compare, to `target/studio-snapshots/ShellMomentsFxSuite/<test>/`. A
  * moment a board depicts is snapshotted with that board's resting focus
  * ring ([[RestingFocus]]); the dark ones with none.
  */
class ShellMomentsFxSuite extends ShellFxSuite:

  private val moments: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("t1", () => StoryModels.t1Data, StoryMoment.T1),
    ("t2", () => StoryModels.t2Compare, StoryMoment.T2),
    ("t3", () => StoryModels.t3Summary, StoryMoment.T3)
  )

  /** `n` is in the window and drawn: it and every ancestor are visible. */
  private def onScreen(w: StudioWindow, n: Node): Boolean = runOnFx {
    (n.getScene eq w.root.getScene) &&
    Iterator.iterate(n)(_.getParent).takeWhile(_ != null).forall(_.isVisible)
  }

  fxStage.test(
    "t1 · Data: the Geometry pane draws the geometry panel under the draft's form, on r3"
  ) { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t1Data, StoryMoment.T1)
    val g = w.geometry
    val v = g.view
    eventually(fx, "the placement pictures of r3")(
      g.shownPictures.exists(_.key.dataset == StoryMoments.r3) &&
        v.thumbnails.exists(t => t.count.getText.nonEmpty)
    )
    assertEquals(runOnFx(g.state.shown.map(_.id)), Some(StoryMoments.r3))
    assert(onScreen(w, v.node), "the geometry panel is not in the window")
    // One pane: the draft's geometry form inside the panel, where the
    // panel's own typed fields were, which give way to it.
    val form = w.columnMapping.geometry.node
    assert(
      runOnFx(Iterator.iterate[Node](form)(_.getParent).takeWhile(_ != null).contains(v.node)),
      "the draft's form is not in the panel"
    )
    assert(onScreen(w, form), "the draft's geometry form is not drawn")
    assert(runOnFx(v.fields.values.forall(f => !f.isVisible)), "two geometry editors are drawn")
    // The density key, every range whole (DESIGN_SPEC section 15.1).
    assertEquals(
      runOnFx(v.densitySwatches.map(drawn)),
      Vector("1–3", "4–15", "16–63", "≥64")
    )
    assertEquals(
      runOnFx(drawn(v.densityKey)),
      "Records per cell, log steps · 64 × 36 screen cells"
    )
    assert(onScreen(w, v.densityKey), "the density key is not drawn")
    // The fixture's first missing image (review ticket 25), hatched and captioned.
    val missing = runOnFx(v.thumbnails.find(_.name.getText == "P01 · enc_08"))
      .getOrElse(fail("no P01 · enc_08 thumbnail"))
    assertEquals(runOnFx(drawn(missing.count)), "Missing asset · forest-044.png")
    assert(onScreen(w, missing.picture), "the missing-asset thumbnail is not drawn")
    fx.snapshot(StudioTheme.Light)
    // The panel follows the shown revision: Data on r2 shows r2.
    runOnFx(
      w.runtime.dispatch(
        Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(StoryMoments.r2))))
      )
    )
    eventually(fx, "the panel on r2")(g.state.shown.map(_.id).contains(StoryMoments.r2))
  }

  fxStage.test("t0 · Data: a new project shows no geometry panel and reads nothing") { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.firstRun, StoryMoment.T1)
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Data)
    assert(!onScreen(w, w.geometry.view.node), "a geometry panel is drawn on a new project")
    assertEquals(runOnFx(w.geometry.state.shown), None)
    assertEquals(runOnFx(w.geometry.shownPictures), None)
    assertEquals(runOnFx(w.geometry.receipts), Vector.empty)
    assertEquals(runOnFx(w.geometry.focusStops), Vector.empty)
  }

  for (moment, base, story) <- moments; p <- Perspective.values do
    fxStage.test(s"$moment · ${p.label}: the shell reads as its view-models; snapshot") { fx =>
      assumeFullStage(fx)
      val model = at(base(), Intent.SwitchPerspective(p))
      val w     = boot(fx, model, story)
      assertEquals(runOnFx(w.runtime.model.perspective), p)
      assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
      // The board's resting focus ring, by keyboard (REVIEW ticket 28).
      RestingFocus.moment(story, p).foreach(RestingFocus.show(fx, w, _))
      fx.snapshot(StudioTheme.Light)
      if p == Perspective.Compare then
        RestingFocus.clear(fx, w)
        val sheets = StudioStyles.stylesheets(Theme.Dark).fold(e => fail(e.message), identity)
        runOnFx(w.root.getStylesheets.setAll(sheets*))
        fx.snapshot(StudioTheme.Dark)
        assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
    }
