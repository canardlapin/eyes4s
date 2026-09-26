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

package eyes4s.surface

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class DensityLookupSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val LookupTolerance                   = 1e-12
  private val frame                             = get(Frame.screen("density-lookup", 5, 3))
  private val grid                              = get(Grid.over(frame, 5, 3))
  private val clock                             = ClockId("trajectory")
  private def field(values: Vector[Double])     = get(
    Surface.signed(
      grid,
      IArray.from(values),
      Provenance.raw(ContentHash.of(IArray.from(values)))
    )
  )
  private val surface = field(KdeReference.lookup_values)
  private val points  =
    KdeReference.query_x.zip(KdeReference.query_y).map((x, y) => Pt[Px](x, y))
  private val modes = Vector(
    "none"   -> DensityNormalization.None,
    "max"    -> DensityNormalization.Maximum,
    "sum"    -> DensityNormalization.Sum,
    "zscore" -> DensityNormalization.SampleZScore
  )

  test(
    "all normalizations and one-based even ties match pinned density lookup on a 5 by 3 grid"
  ) {
    modes.foreach { (name, mode) =>
      val prepared = get(DensityLookup.prepare(surface, mode))
      val actual = get(prepared.sample(frame, points, DensityLookupPolicy.NearestClampedRIndex))
      val expected = KdeReference.lookup.find(_._1 == name).get._2
      assertEquals(actual.map(_.point), points)
      actual
        .map(r => get(r.value))
        .zip(expected)
        .foreach((a, b) => assertEqualsDouble(a, b, LookupTolerance))
      assertEquals(prepared.normalization, mode)
    }
    val plain = get(DensityLookup.prepare(surface, DensityNormalization.None))
    val ties  = get(
      plain.sample(
        frame,
        Vector(Pt[Px](1, 0.5), Pt[Px](2, 0.5), Pt[Px](3, 0.5), Pt[Px](4, 0.5)),
        DensityLookupPolicy.NearestClampedRIndex
      )
    )
    assertEquals(ties.map(r => get(r.value)), Vector(2.0, 2.0, 4.0, 4.0))
  }

  test(
    "containing-cell behavior remains distinct and finite outside values clamp only under the named policy"
  ) {
    val prepared  = get(DensityLookup.prepare(surface, DensityNormalization.None))
    val queries   = Vector(Pt[Px](-1, 1.5), Pt[Px](5, 1.5), Pt[Px](2, 0.5))
    val contained = get(prepared.sample(frame, queries, DensityLookupPolicy.ContainingCell))
    assert(contained.take(2).forall(_.value.isLeft))
    assertEquals(get(contained(2).value), 3.0)
    val clamped = get(prepared.sample(frame, queries, DensityLookupPolicy.NearestClampedRIndex))
    assertEquals(clamped.map(r => get(r.value)), Vector(6.0, 10.0, 2.0))
    assert(
      prepared
        .sample(get(Frame.screen("other", 5, 3)), queries, DensityLookupPolicy.ContainingCell)
        .isLeft
    )
    assert(
      get(
        prepared.sample(
          frame,
          Vector(Pt[Px](Double.NaN, 0)),
          DensityLookupPolicy.NearestClampedRIndex
        )
      ).head.value.isLeft
    )
    assert(
      get(
        prepared.sample(frame, Vector.empty, DensityLookupPolicy.NearestClampedRIndex)
      ).isEmpty
    )
  }

  test("sample SD, zero variance, zero maximum/sum and singleton policies are explicit") {
    modes.foreach { (_, mode) =>
      val zero = get(DensityLookup.prepare(field(Vector.fill(15)(0)), mode))
      assertEquals(zero.field.values.toVector, Vector.fill(15)(0.0))
    }
    val constant =
      get(DensityLookup.prepare(field(Vector.fill(15)(3)), DensityNormalization.SampleZScore))
    assertEquals(constant.field.values.toVector, Vector.fill(15)(3.0))
    val z = get(
      DensityLookup.prepare(surface, DensityNormalization.SampleZScore)
    ).field.values.toVector
    val sd = math.sqrt(20.0) // sum((1..15 - 8)^2)/(15-1) = 20
    assertEqualsDouble(z.head, -7 / sd, LookupTolerance)
    assert(z.exists(_ < 0))
    assert(typeCheckErrors("""
      def mass(p: eyes4s.surface.PreparedDensityLookup[eyes4s.kernel.Unit2D.Px]): eyes4s.kernel.Mass[eyes4s.kernel.Unit2D.Px] = p.field
    """).nonEmpty)
    val oneGrid = get(Grid.over(frame, 1, 1))
    val one     = get(Surface.signed(oneGrid, IArray(3.0), Provenance.raw(ContentHash.empty)))
    assertEquals(
      get(DensityLookup.prepare(one, DensityNormalization.SampleZScore)).field.values.toVector,
      Vector(3.0)
    )
    assert(
      DensityLookup
        .prepare(field(Vector.fill(15)(Double.MaxValue)), DensityNormalization.Sum)
        .isLeft
    )
    assert(
      DensityLookup
        .prepare(
          field(
            Vector.tabulate(15)(i => if i % 2 == 0 then Double.MaxValue else -Double.MaxValue)
          ),
          DensityNormalization.SampleZScore
        )
        .isLeft
    )
  }

  test(
    "density-at-time composes the shared fast trajectory and matches both exported reference calls"
  ) {
    val fixations = KdeReference.onsets_us.indices.map { i =>
      get(
        Event.Fixation.of(
          get(
            Interval.of(
              clock,
              Instant.micros(KdeReference.onsets_us(i)),
              Instant.micros(KdeReference.onsets_us(i) + KdeReference.durations_us(i))
            )
          ),
          Pt[Px](KdeReference.x(i), KdeReference.y(i)),
          0.0,
          DispersionMethod.RmsRadius,
          1
        )
      )
    }
    val path       = get(Scanpath.of(frame, clock, IArray.from(fixations)))
    val trajectory = get(
      FixationTrajectory
        .fromScanpath(path)
        .sample(
          clock,
          KdeReference.times_us.map(Instant.micros),
          TrajectoryEndpoint.HoldLastOnset
        )
    )
    val prepared = get(DensityLookup.prepare(surface, DensityNormalization.Sum))
    val rows     = get(prepared.along(trajectory, DensityLookupPolicy.NearestClampedRIndex))
    assertEquals(rows.map(_.time.toMicros), KdeReference.times_us)
    rows.map(_.value.toOption).zip(KdeReference.timed).foreach { (a, b) =>
      assertEquals(a.isDefined, b.isDefined)
      a.zip(b).foreach((x, y) => assertEqualsDouble(x, y, LookupTolerance))
    }
    val raw = get(
      get(DensityLookup.prepare(surface, DensityNormalization.None))
        .along(trajectory, DensityLookupPolicy.NearestClampedRIndex)
    )
    assertEquals(raw.map(_.value.toOption), KdeReference.template_sample)
    // eyesim holds the final fixation after its onset, as HoldLastOnset does.
    assert(rows.head.value.isLeft && rows.last.value.isRight)
  }
