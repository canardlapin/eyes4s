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

package eyes4s.studio.viz.trial

import eyes4s.codec.ByteDigest
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.core.assets.*
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.{ImagePlacement, ScreenSize}
import eyes4s.studio.core.selection.FixationIndex
import intaglio.{RasterDimensions, RasterImage, Rgba32}

/** Trial displays and fixations for the trial-scene tests: the golden
  * fixture's geometry (1024×768 at (448, 156) in 1920×1080) and its P17
  * trials, typed in from fixtures/studio-golden (records 7,210–7,222 and
  * 7,531–7,542).
  */
object TrialSamples:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => sys.error(x.toString), identity)

  val screen: ScreenSize        = right(ScreenSize.of(1920, 1080))
  val placement: ImagePlacement = right(ImagePlacement.of(448, 156, 1024, 768))

  val ret07: TrialKey = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  val enc03: TrialKey = TrialKey("P17", Phase.Encoding, "enc_03", 1)

  def file(name: String): AssetFile = right(AssetFile.of(name))

  def asset(name: String): AssetRef =
    AssetRef(file(name), ByteDigest.sha256(IArray.from(name.getBytes("UTF-8"))))

  val beach: AssetRef = asset("beach-042.png")
  val item: MatchItem = right(MatchItem.of(enc03, "beach-042"))

  /** A small raster standing in for a decoded stimulus. */
  val raster: RasterImage =
    RasterImage.tabulate(RasterDimensions.unsafe(8, 6))((x, y) =>
      Rgba32.unsafe(x * 30, y * 40, 90)
    )

  def display(trial: TrialKey, d: Display): TrialDisplay =
    TrialDisplay(trial, Some(item), d, placement)

  /** Every display kind, with the missing-asset state, by name. */
  val displays: List[(String, Display)] = List(
    "image"       -> Display.Image(AssetLink.Present(beach)),
    "blank"       -> Display.Blank,
    "blank-cross" -> Display.BlankWithFixationCross,
    "cue"         -> Display.Cue(None),
    "cue-image"   -> Display.Cue(Some(AssetLink.Present(beach))),
    "unknown"     -> Display.Unknown(None),
    "missing"     -> Display.Image(AssetLink.Missing(file("forest-044.png"))),
    "cue-missing" -> Display.Cue(Some(AssetLink.Missing(file("kitchen-081.png"))))
  )

  private def fixations(
      trial: TrialKey,
      rows: Vector[(Double, Double, Int)]
  ): Vector[TrialFixation] =
    rows.zipWithIndex.map { case ((x, y, d), i) =>
      val inside = x >= 448 && x < 1472 && y >= 156 && y < 924
      right(
        TrialFixation.of(
          trial,
          right(FixationIndex.of(i + 1)),
          x,
          y,
          d,
          if inside then MapPlacement.InMap
          else MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)
        )
      )
    }

  /** P17 ret_07: 12 fixations, one (9) outside the image frame. */
  val ret07Fixations: Vector[TrialFixation] = fixations(
    ret07,
    Vector(
      (936.0, 547.9, 258),
      (1197.7, 443.8, 290),
      (1271.6, 301.4, 472),
      (1279.9, 364.7, 246),
      (1317.4, 365.0, 244),
      (592.3, 454.5, 424),
      (643.2, 366.8, 354),
      (1314.9, 378.6, 308),
      (234.7, 252.0, 134),
      (1268.9, 320.1, 260),
      (756.1, 414.9, 152),
      (1200.5, 479.5, 206)
    )
  )

  /** P17 enc_03: 13 fixations, one (10) outside the image frame. */
  val enc03Fixations: Vector[TrialFixation] = fixations(
    enc03,
    Vector(
      (600.5, 323.5, 330),
      (1111.3, 740.7, 380),
      (672.4, 393.5, 350),
      (1300.9, 364.4, 420),
      (603.0, 440.8, 370),
      (1148.0, 456.0, 412),
      (747.3, 595.4, 340),
      (1242.5, 411.1, 310),
      (1295.3, 348.5, 360),
      (327.1, 430.9, 126),
      (696.2, 337.9, 330),
      (1285.9, 361.8, 300),
      (1322.4, 294.3, 350)
    )
  )
