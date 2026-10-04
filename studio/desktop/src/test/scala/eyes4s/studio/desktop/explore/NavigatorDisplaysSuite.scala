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
import eyes4s.studio.core.document.{SourceRole, Sources}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.desktop.StudioMain

/** Where the trials navigator's display kinds come from (ticket S6.1): the
  * application serves none before S5.7, so a project never shows the
  * fixture's; the story sessions' golden registry is served only for the
  * golden trial inventory, byte for byte. Headless.
  */
class NavigatorDisplaysSuite extends munit.FunSuite:

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

  test("the application serves no display kinds, even for the story's golden r3") {
    assertEquals(StudioMain.displays, NavigatorDisplays.notServed)
    assertEquals(StudioMain.displays.displays(r3), Right(DisplaySource.NotServed))
    assertEquals(StudioMain.displays.displays(otherTrials), Right(DisplaySource.NotServed))
  }

  test("the golden registry is served only for the golden trial inventory") {
    NavigatorDisplays.golden.displays(r3) match
      case Right(DisplaySource.Served(registry)) =>
        assertEquals(registry.dataset, StoryMoments.r3)
        assertEquals(registry.summary.missing.size, 2)
      case other => fail(s"expected the golden registry, got $other")
    assertEquals(NavigatorDisplays.golden.displays(otherTrials), Right(DisplaySource.NotServed))
    // Without a trial inventory there is nothing to describe.
    val noTrials = r3.copy(sources =
      Sources.of(r3.sources.entries.filter(_.role != SourceRole.Trials)).toOption.get
    )
    assertEquals(NavigatorDisplays.golden.displays(noTrials), Right(DisplaySource.NotServed))
  }
