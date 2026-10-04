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
  PlotSource,
  ProfileColumns,
  ProfilePoint,
  ProfileSeries,
  ScaleProfile
}
import eyes4s.studio.core.backend.{Response, RunId}
import eyes4s.studio.core.document.{ReportingId, Sigma}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import org.scalacheck.Gen

/** Scale profiles for the profile plot's tests (ticket S4.5d), shaped as
  * [[ScaleProfile.of]] reads them: groups' grand means and participants'
  * means at each declared scale, some missing.
  */
object ProfileSamples:

  val run: RunId = RunId(7)

  val reporting: ReportingId =
    ReportingId.of("by-retrieval-response").fold(e => throw new AssertionError(e), identity)

  val columns: ProfileColumns =
    ProfileColumns.standard.fold(e => throw new AssertionError(e.message), identity)

  private def sigma(d: Double): Sigma =
    Sigma.of(d).fold(e => throw new AssertionError(e), identity)
  private def index(i: Int): ScaleIndex =
    ScaleIndex.of(i).fold(e => throw new AssertionError(e), identity)

  private def label(d: Double): String =
    if d == math.rint(d) then s"${d.toLong}°" else s"$d°"

  /** A profile at `sigmas`: each group's name, n and D per scale, and each
    * participant's.
    */
  def profile(
      sigmas: Vector[Double],
      groups: Vector[(String, Int, Vector[Option[Double]])],
      participants: Vector[(String, Int, Vector[Option[Double]])]
  ): ScaleProfile =
    def points(ref: ScaleIndex => StudioRef, ds: Vector[Option[Double]]) =
      sigmas.zipWithIndex.map((s, i) =>
        ProfilePoint(ref(index(i)), index(i), label(s), sigma(s), ds.lift(i).flatten)
      )
    ScaleProfile(
      run,
      reporting,
      groups.map((g, n, ds) =>
        ProfileSeries(
          g,
          s"$n participants",
          points(StudioRef.GroupCell(run, reporting, _, Response(g)), ds)
        )
      ),
      participants.map((p, n, ds) =>
        ProfileSeries(
          p,
          s"$n queries",
          points(StudioRef.ParticipantSummary(run, reporting, _, None, p), ds)
        )
      )
    )

  def source(profile: ScaleProfile): PlotSource =
    ScaleProfile
      .source(profile, columns)
      .fold(e => throw new AssertionError(e.message), identity)

  val protocol: Vector[Double] = Vector(0.5, 1.0, 2.0, 4.0)

  /** The Figures board's panel E: Remembered and Forgotten by scale, and
    * three participants, one with no means.
    */
  val board: ScaleProfile = profile(
    protocol,
    Vector(
      ("Remembered", 24, Vector(0.15, 0.24, 0.30, 0.19).map(Some(_))),
      ("Forgotten", 23, Vector(0.07, 0.12, 0.15, 0.09).map(Some(_)))
    ),
    Vector(
      ("P17", 19, Vector(0.19, 0.29, 0.38, 0.23).map(Some(_))),
      ("P05", 17, Vector(Some(-0.04), None, Some(-0.08), Some(-0.05))),
      ("P99", 0, Vector.fill(4)(None))
    )
  )

  private val genD: Gen[Option[Double]] =
    Gen.frequency(8 -> Gen.choose(-1.0, 1.0).map(Some(_)), 1 -> Gen.const(None))

  /** One to five ascending scales, one to three groups, up to eight participants. */
  val genProfile: Gen[ScaleProfile] =
    for
      k      <- Gen.choose(1, 5)
      sigmas <- Gen.pick(k, Vector(0.25, 0.5, 1.0, 2.0, 4.0, 8.0)).map(_.toVector.sorted)
      g      <- Gen.choose(1, 3)
      groups <- Gen.sequence[Vector[
        (String, Int, Vector[Option[Double]])
      ], (String, Int, Vector[Option[Double]])](
        Vector("Remembered", "Forgotten", "Unsure")
          .take(g)
          .map(name => Gen.listOfN(k, genD).map(ds => (name, 24, ds.toVector)))
      )
      n      <- Gen.choose(0, 8)
      people <- Gen.listOfN(n, Gen.listOfN(k, genD).map(_.toVector))
    yield profile(
      sigmas,
      groups,
      people.toVector.zipWithIndex.map((ds, i) => (f"P${i + 1}%02d", 10, ds))
    )
