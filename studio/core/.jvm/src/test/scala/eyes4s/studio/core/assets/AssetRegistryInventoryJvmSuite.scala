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

package eyes4s.studio.core.assets

import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}

import java.nio.charset.StandardCharsets.UTF_8

/** The golden fixture's registry read from fixtures/studio-golden's own
  * trials.csv bytes (S5.7, JVM test scope: `GoldenCsv`), through r3's
  * inventory mapping with its display columns: the same registry the
  * compiled fixture rows give, with 257 of 259 images found.
  */
class AssetRegistryInventoryJvmSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(m => fail(s"unexpected: $m"), identity)

  test("the stored trials.csv gives the golden registry: 257 of 259 images found") {
    val r3      = ok(StoryMoments.t2).dataset(StoryMoments.r3).get
    val columns = DisplayColumns(
      ok(ColumnName.of("display_kind")),
      Some(ok(ColumnName.of("image_file")))
    )
    val mapped = r3.copy(inventory =
      r3.inventory.map(m => ok(InventoryMapping.withDisplays(m, Some(columns))))
    )
    val bytes  = IArray.from(GoldenCsv.trials.getBytes(UTF_8))
    val stored = ok(GoldenAssets.stimuli).map(_.asset)
    val read   = ok(AssetRegistry.fromInventory(mapped, bytes, stored, Vector.empty))
    assertEquals(read, ok(GoldenAssets.registry(r3)))
    val summary = read.summary
    assertEquals((summary.files, summary.present), (259, 257))
    assertEquals(
      summary.missing.map(m => (m.file.value, m.trials.map(t => (t.participant, t.trial)))),
      Vector(
        "forest-044.png"  -> Vector(("P01", "enc_08"), ("P24", "enc_18")),
        "kitchen-081.png" -> Vector(("P01", "enc_15"), ("P24", "enc_05"))
      )
    )
  }
