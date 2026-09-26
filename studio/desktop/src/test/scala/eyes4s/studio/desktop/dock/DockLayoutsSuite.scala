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

package eyes4s.studio.desktop.dock

import eyes4s.studio.app.layout.StudioLayouts
import scaladock.{Header, LayoutCodec, PaneTypes}

/** The LayoutSpec → scaladock mapping (ticket S1.5a), without the toolkit. */
class DockLayoutsSuite extends munit.FunSuite:

  private val spec = StudioLayouts.spec

  test("every layout maps pane for pane, group for group, with the same tab order") {
    spec.all.foreach { l =>
      val s = DockLayouts.state(l)
      assertEquals(s.panes.map(_.id.value), l.panes.map(_.id.value), l.id)
      assertEquals(s.panes.map(_.title), l.panes.map(_.title.text), l.id)
      assertEquals(s.groups.size, l.groups.size, l.id)
      assertEquals(
        s.groups.map(_.active.value),
        l.groups.map(g => l.selectedPane(g).id.value),
        l.id
      )
      assert(s.panes.forall(!_.closable), l.id)
    }
  }

  test("the mapping is deterministic and survives scaladock's codec") {
    val types = PaneTypes(StudioPanes.placeholder)
    spec.all.foreach { l =>
      assertEquals(DockLayouts.state(l), DockLayouts.state(l))
      assertEquals(
        LayoutCodec.decode(LayoutCodec.encode(DockLayouts.state(l)), types),
        Right(DockLayouts.state(l))
      )
    }
  }

  test(
    "one perspective name per layout; Compare's two layouts share the scale profile's pane id"
  ) {
    val names = DockLayouts.states(spec).map(_._1)
    assertEquals(names, spec.all.map(_.id.value))
    assertEquals(names.distinct, names)
    val shared = "compare.scale-profile"
    assert(
      DockLayouts.state(StudioLayouts.compareQuery).findPane(scaladock.PaneId(shared)).isDefined
    )
    assert(
      DockLayouts
        .state(StudioLayouts.compareSummary)
        .findPane(scaladock.PaneId(shared))
        .isDefined
    )
  }

  test("28 px tab headers; navigators may minimize to a strip; nothing closes or pops out") {
    assertEquals(DockLayouts.settings.headerPx, 28.0)
    spec.all.foreach { l =>
      val s = DockLayouts.state(l)
      l.groups.zip(s.groups).foreach { (studio, dock) =>
        val expected =
          if studio.navigator then DockLayouts.navigatorHeader else DockLayouts.defaultHeader
        assertEquals(dock.header, Header.Shown(expected), l.id)
      }
    }
    assert(!DockLayouts.defaultHeader.close && !DockLayouts.defaultHeader.popOut)
    assert(DockLayouts.navigatorHeader.minimize)
  }
