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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.plot.{
  LadderColumns,
  LadderControl,
  LadderScale,
  PlotSource,
  ScaleLadder
}
import eyes4s.studio.core.backend.{PairDesign, Phase, ResultAddress, RunId, TrialKey}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import org.scalacheck.Gen

/** Scale ladders for the ladder's tests (ticket S4.5b), shaped as
  * [[ScaleLadder.load]] reads them: one query, M, B and D at each scale and
  * its controls, some without a served cosine.
  */
object LadderSamples:

  val run: RunId      = RunId(7)
  val query: TrialKey = TrialKey("P17", Phase.Retrieval, "ret_07", 1)

  val columns: LadderColumns =
    LadderColumns.standard.fold(e => throw new AssertionError(e.message), identity)

  def encoding(trial: String): TrialKey = TrialKey("P17", Phase.Encoding, trial, 1)

  /** One scale: its label, M, B, D and each control's cosine, if served. */
  final case class Spec(
      label: String,
      m: Double,
      b: Double,
      d: Double,
      controls: Vector[Option[Double]]
  )

  def scale(index: Int, spec: Spec): LadderScale =
    val s = ScaleIndex.of(index).fold(e => throw new AssertionError(e.message), identity)
    LadderScale(
      s,
      spec.label,
      StudioRef.Pair(run, s, PairDesign.Matched, query, encoding("enc_03")),
      encoding("enc_03"),
      "beach-042",
      spec.m,
      StudioRef
        .fromAddress(run, ResultAddress.Reduction(index, PairDesign.Control, query))
        .fold(e => throw new AssertionError(e.message), identity),
      spec.b,
      spec.controls.size,
      StudioRef.QueryContrast(run, s, query),
      spec.d,
      spec.controls.zipWithIndex.map { (cosine, k) =>
        val trial = encoding(f"enc_c$k%03d")
        LadderControl(
          StudioRef.Pair(run, s, PairDesign.Control, query, trial),
          trial,
          cosine.map(_ => f"item-$k%03d"),
          cosine
        )
      }
    )

  def ladder(specs: Spec*): ScaleLadder =
    ScaleLadder(run, query, specs.toVector.zipWithIndex.map((spec, i) => scale(i, spec)))

  def source(ladder: ScaleLadder): PlotSource =
    ScaleLadder.source(ladder, columns).fold(e => throw new AssertionError(e.message), identity)

  /** The fixture's focus query's 2° control scores (FIXTURE.md). */
  val focusControls: Vector[Double] = Vector(0.61, 0.52, 0.47, 0.44, 0.41, 0.39, 0.37, 0.36,
    0.35, 0.33, 0.31, 0.3, 0.29, 0.27, 0.27, 0.26, 0.26, 0.24, 0.2)

  /** The Main board's ladder: four scales, controls served at 2° only. */
  val board: ScaleLadder = ladder(
    Spec("0.5°", 0.41, 0.22, 0.19, Vector.fill(19)(None)),
    Spec("1°", 0.58, 0.29, 0.29, Vector.fill(19)(None)),
    Spec("2°", 0.73, 0.35, 0.38, focusControls.map(Some(_))),
    Spec("4°", 0.86, 0.63, 0.23, Vector.fill(19)(None))
  )

  /** Cosines on a coarse grid, so controls coincide and fall on bin edges. */
  private val genCosine: Gen[Double] = Gen.oneOf(
    Gen.choose(0.0, 1.0),
    Gen.choose(0, 40).map(_ / 40.0)
  )

  private def genSpec(label: String): Gen[Spec] =
    for
      m        <- genCosine
      b        <- genCosine
      d        <- Gen.choose(-1.0, 1.0)
      n        <- Gen.frequency(4 -> Gen.choose(0, 25), 1 -> Gen.choose(48, 70))
      controls <- Gen.listOfN(
        n,
        Gen.frequency(9 -> genCosine.map(Some(_)), 1 -> Gen.const(None))
      )
    yield Spec(label, m, b, d, controls.toVector)

  /** One to four scales of generated M, B, D and up to 70 controls. */
  val genLadder: Gen[ScaleLadder] =
    for
      n     <- Gen.choose(1, 4)
      specs <- Gen.sequence[Vector[Spec], Spec](
        Vector("0.5°", "1°", "2°", "4°").take(n).map(genSpec)
      )
    yield ladder(specs*)

  /** A ladder builder, focused on one of the ladder's scales or on none. */
  def genBuilder(ladder: ScaleLadder): Gen[ScaleLadderPlot] =
    Gen.option(Gen.oneOf(ladder.scales.map(_.label))).map(ScaleLadderPlot(columns, _))
