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
  GroupGrandMean,
  ParticipantCell,
  ParticipantColumns,
  ParticipantMeans,
  PlotSource
}
import eyes4s.studio.core.backend.{Response, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import org.scalacheck.Gen

/** Participant means for the participant plot's tests (ticket S4.5c),
  * shaped as [[ParticipantMeans.of]] reads them: one to three groups, each
  * with a grand mean and every participant's mean, some missing.
  */
object ParticipantSamples:

  val run: RunId = RunId(7)

  val reporting: ReportingId =
    ReportingId.of("by-retrieval-response").fold(e => throw new AssertionError(e), identity)

  val scale: ScaleIndex = ScaleIndex.of(2).fold(e => throw new AssertionError(e), identity)

  val columns: ParticipantColumns =
    ParticipantColumns.standard.fold(e => throw new AssertionError(e.message), identity)

  /** Each group's grand mean and n, and each participant's mean and n in it,
    * if it has one.
    */
  def means(
      groups: Vector[(String, Double, Int)],
      participants: Vector[(String, Vector[Option[(Double, Int)]])]
  ): ParticipantMeans =
    ParticipantMeans(
      run,
      reporting,
      scale,
      "2°",
      groups.map((g, d, n) =>
        GroupGrandMean(
          StudioRef.GroupCell(run, reporting, scale, Response(g)),
          Response(g),
          d,
          n
        )
      ),
      groups.zipWithIndex.flatMap { case ((g, _, _), k) =>
        participants.map { (p, cells) =>
          val cell = cells.lift(k).flatten
          ParticipantCell(
            StudioRef.ParticipantSummary(run, reporting, scale, Some(Response(g)), p),
            Response(g),
            p,
            cell.map(_._1),
            cell.map(_._2)
          )
        }
      }
    )

  def source(means: ParticipantMeans): PlotSource =
    ParticipantMeans
      .source(means, columns)
      .fold(e => throw new AssertionError(e.message), identity)

  /** FIXTURE.md's participant table at 2°: Remembered and Forgotten D (n). */
  val fixtureRows: Vector[(String, Double, Int, Double, Int)] = Vector(
    ("P01", .26, 11, .06, 8),
    ("P02", .27, 13, .12, 6),
    ("P03", .38, 15, .15, 4),
    ("P04", .36, 12, .18, 7),
    ("P05", -.03, 13, -.23, 4),
    ("P06", .30, 11, .17, 8),
    ("P07", .32, 15, .10, 4),
    ("P08", .31, 12, .15, 7),
    ("P09", .34, 15, .24, 4),
    ("P10", .45, 16, .33, 3),
    ("P11", .28, 13, .11, 6),
    ("P12", .34, 12, .19, 7),
    ("P13", .17, 11, .03, 8),
    ("P14", .29, 14, .22, 5),
    ("P15", .18, 13, -.08, 6),
    ("P16", .45, 12, .28, 7),
    ("P17", .38, 17, .32, 2),
    ("P18", .42, 16, .37, 3),
    ("P19", .29, 15, .06, 4),
    ("P20", .26, 14, .12, 5),
    ("P21", .32, 17, .27, 2),
    ("P22", .27, 12, .06, 7),
    ("P23", .34, 14, .21, 5),
    ("P24", .36, 13, .17, 6)
  )

  /** The Results board's plot: 24 participants, grand means +0.30 and +0.15. */
  val board: ParticipantMeans = means(
    Vector(("Remembered", 0.30, 24), ("Forgotten", 0.15, 24)),
    fixtureRows.map((p, r, rn, f, fn) => (p, Vector(Some((r, rn)), Some((f, fn)))))
  )

  /** Two participants with a zero mean and a missing one in the first group. */
  val zeroAndMissing: ParticipantMeans = means(
    Vector(("Remembered", 0.10, 3), ("Forgotten", 0.05, 2)),
    Vector(
      "P01" -> Vector(Some((0.0, 5)), Some((0.10, 3))),
      "P02" -> Vector(None, Some((0.0, 4))),
      "P03" -> Vector(Some((0.20, 6)), None)
    )
  )

  private val genCell: Gen[Option[(Double, Int)]] =
    Gen.frequency(
      6 -> Gen.zip(Gen.choose(-1.0, 1.0), Gen.choose(1, 20)).map(Some(_)),
      1 -> Gen.zip(Gen.const(0.0), Gen.choose(1, 20)).map(Some(_)),
      1 -> Gen.const(None)
    )

  /** One to three groups of up to twelve participants, some means missing. */
  val genMeans: Gen[ParticipantMeans] =
    for
      g      <- Gen.choose(1, 3)
      groups <- Gen.sequence[Vector[(String, Double, Int)], (String, Double, Int)](
        Vector("Remembered", "Forgotten", "Unsure")
          .take(g)
          .map(label =>
            Gen.zip(Gen.choose(-1.0, 1.0), Gen.choose(0, 30)).map((d, n) => (label, d, n))
          )
      )
      n     <- Gen.choose(0, 12)
      cells <- Gen.listOfN(n, Gen.listOfN(g, genCell).map(_.toVector))
    yield means(groups, cells.toVector.zipWithIndex.map((cs, i) => (f"P${i + 1}%02d", cs)))
