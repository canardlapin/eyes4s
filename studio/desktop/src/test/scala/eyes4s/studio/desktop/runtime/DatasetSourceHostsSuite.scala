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

package eyes4s.studio.desktop.runtime

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.desktop.journey.FixtureDoc
import java.nio.file.Files
import munit.CatsEffectSuite

class DatasetSourceHostsSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("the native example host serves actual declared source bytes and image registry") {
    val document = get(StoryMoments.t2)
    val dataset  = document.dataset(StoryMoments.r3).getOrElse(fail("no dataset"))
    val source   = DatasetSourceHosts.golden(FixtureDoc.root.resolve("fixtures/studio-golden"))
    for
      bytes  <- dataset.sources.entries.traverse(s => source.bytes(dataset, s).map(s -> _))
      assets <- source.assets(dataset)
    yield
      bytes.foreach { (recorded, held) =>
        val actual = held.getOrElse(fail(s"missing ${recorded.path.value}"))
        assertEquals(ByteDigest.sha256(actual), recorded.bytes)
      }
      val registry = assets.getOrElse(fail("no native image registry"))
      assertEquals(registry.summary.present, 257)
      assertEquals(registry.summary.files, 259)
  }

  test("a missing example source remains absent rather than falling back to fixture text") {
    val document = get(StoryMoments.t2)
    val dataset  = document.dataset(StoryMoments.r3).getOrElse(fail("no dataset"))
    IO.blocking(Files.createTempDirectory("eyes4s-native-source-absence-")).flatMap { folder =>
      val source = DatasetSourceHosts.golden(folder)
      dataset.sources.entries
        .traverse(s => source.bytes(dataset, s))
        .map { values =>
          assert(values.forall(_.isEmpty))
        }
        .guarantee(IO.blocking(Files.deleteIfExists(folder)).void)
    }
  }
