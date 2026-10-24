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

package eyes4s.studio.desktop.harness

import java.awt.image.BufferedImage

class VisualPixelsSuite extends munit.FunSuite:
  private def image(w: Int = 100, h: Int = 100): BufferedImage =
    BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  private def diff(a: BufferedImage, b: BufferedImage) =
    VisualPixels.compare(a, b).fold(fail(_), identity)
  test("identical rasters pass and a deliberate visible changed pixel is rejected") {
    val a = image(); val b = image()
    assert(diff(a, b).accepted)
    b.setRGB(31, 42, 0xffff0000)
    val result = diff(a, b)
    assertEquals(result.changed, 1L)
    assert(!result.accepted)
  }
  test("bounded one-level rounding passes; a broad one-level change fails") {
    val a = image(); val b = image()
    b.setRGB(0, 0, 1)
    assert(diff(a, b).accepted)
    for x <- 0 until 20 do b.setRGB(x, 0, 1)
    assert(!diff(a, b).accepted)
  }
  test("a missing or differently sized baseline cannot be a match") {
    assert(VisualPixels.compare(image(), image(99)).isLeft)
    intercept[java.io.IOException](
      VisualPixels.read(java.nio.file.Path.of("missing-visual-baseline.png"))
    )
  }
