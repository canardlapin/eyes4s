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

package eyes4s.laws

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Norm, Px}

import org.scalacheck.{Arbitrary, Cogen, Gen}

/** ScalaCheck generators for the kernel types.
  *
  * Published as main-scope library code, not test-scope, so that an author
  * writing their own `Warp`, `Smoother` or `Compare` instance can run the same
  * suites against it. That is the point of a `laws` module (PRD PKG-6).
  */
object Generators:

  // -------------------------------------------------------------------------
  // Time
  // -------------------------------------------------------------------------

  /** Bounded well inside `Long` so that sums in a group law cannot overflow --
    * an overflow would fail the law for a reason that has nothing to do with
    * the law.
    */
  val genSpan: Gen[Span] =
    Gen.choose(-1000000000L, 1000000000L).map(Span.micros)

  val genInstant: Gen[Instant] =
    Gen.choose(0L, 1000000000L).map(Instant.micros)

  val genClockId: Gen[ClockId] =
    Gen.oneOf("tracker", "stimulus", "system", "host").map(ClockId.apply)

  val genInterval: Gen[Interval] =
    for
      c <- genClockId
      a <- genInstant
      d <- Gen.choose(0L, 100000000L)
    yield Interval.of(c, a, a + Span.micros(d)).toOption.get

  val genWindow: Gen[Window] =
    for
      a <- genSpan
      d <- Gen.choose(0L, 100000000L)
    yield Window.of(a, a + Span.micros(d)).toOption.get

  val genOverlap: Gen[Overlap] = Gen.oneOf(Overlap.values.toIndexedSeq)

  given Arbitrary[Span]     = Arbitrary(genSpan)
  given Arbitrary[Instant]  = Arbitrary(genInstant)
  given Arbitrary[Interval] = Arbitrary(genInterval)
  given Arbitrary[Window]   = Arbitrary(genWindow)
  given Arbitrary[ClockId]  = Arbitrary(genClockId)
  given Arbitrary[Overlap]  = Arbitrary(genOverlap)

  // Cogen lets ScalaCheck build FUNCTIONS of these types, which the cats law
  // bundles need in order to test properties quantified over transformations.
  given Cogen[Span]    = Cogen[Long].contramap(_.toMicros)
  given Cogen[Instant] = Cogen[Long].contramap(_.toMicros)

  // -------------------------------------------------------------------------
  // Geometry
  // -------------------------------------------------------------------------

  private val genCoord: Gen[Double] = Gen.choose(-1000.0, 1000.0)

  private val genExtent: Gen[Double] = Gen.choose(1.0, 2000.0)

  def genBounds[U <: Unit2D]: Gen[Bounds[U]] =
    for
      x0 <- genCoord
      y0 <- genCoord
      w  <- genExtent
      h  <- genExtent
    yield Bounds.of[U](x0, y0, x0 + w, y0 + h).toOption.get

  val genYAxis: Gen[YAxis] = Gen.oneOf(YAxis.Down, YAxis.Up)

  /** Frame identities are drawn from a small pool on purpose.
    *
    * With unique names, two independently generated frames would never share an
    * identity and every composition law would be vacuously skipped. A small pool
    * means collisions happen often enough to exercise both branches.
    */
  val genFrameId: Gen[FrameId] =
    Gen.oneOf("alpha", "beta", "gamma", "delta").map(FrameId.apply)

  def genFrame[U <: Unit2D]: Gen[Frame[U]] =
    for
      id <- genFrameId
      b  <- genBounds[U]
      y  <- genYAxis
    yield Frame.of(id, b, y)

  def genPtIn[U <: Unit2D](f: Frame[U]): Gen[Pt[U]] =
    for
      tx <- Gen.choose(0.0, 1.0)
      ty <- Gen.choose(0.0, 1.0)
    yield Pt[U](
      f.bounds.xMin + tx * f.bounds.width,
      f.bounds.yMin + ty * f.bounds.height
    )

  val genAngle: Gen[Angle] =
    Gen.choose(-720.0, 720.0).map(Angle.degrees)

  given Arbitrary[Angle]        = Arbitrary(genAngle)
  given Cogen[Angle]            = Cogen[Double].contramap(_.toRadians)
  given Arbitrary[Frame[Norm]]  = Arbitrary(genFrame[Norm])
  given Arbitrary[Bounds[Norm]] = Arbitrary(genBounds[Norm])

  // -------------------------------------------------------------------------
  // Composable warp chains
  // -------------------------------------------------------------------------

  /** Four frames and three warps that compose end to end by construction.
    *
    * Associativity cannot be tested on independently generated warps: they would
    * almost never compose, and a property that is vacuously true for nearly
    * every sample tests nothing. Generating the chain so that it composes, then
    * asserting the law on it, is the honest way to state a law about a partial
    * operation.
    */
  final case class Chain[U <: Unit2D](
      a: Frame[U],
      b: Frame[U],
      c: Frame[U],
      d: Frame[U],
      f: Warp[U, U],
      g: Warp[U, U],
      h: Warp[U, U]
  )

  def genChain[U <: Unit2D]: Gen[Chain[U]] =
    for
      ba <- genBounds[U]
      bb <- genBounds[U]
      bc <- genBounds[U]
      bd <- genBounds[U]
      ya <- genYAxis
      yb <- genYAxis
      yc <- genYAxis
      yd <- genYAxis
    yield
      // Distinct identities, so that the composability of the chain comes from
      // its construction rather than from an accidental name collision.
      val fa = Frame.of[U](FrameId("chain-a"), ba, ya)
      val fb = Frame.of[U](FrameId("chain-b"), bb, yb)
      val fc = Frame.of[U](FrameId("chain-c"), bc, yc)
      val fd = Frame.of[U](FrameId("chain-d"), bd, yd)
      Chain(
        fa,
        fb,
        fc,
        fd,
        Warp.rescale(fa, fb).toOption.get,
        Warp.rescale(fb, fc).toOption.get,
        Warp.rescale(fc, fd).toOption.get
      )

  given Arbitrary[Chain[Norm]] = Arbitrary(genChain[Norm])

  // -------------------------------------------------------------------------
  // Tangent setups
  // -------------------------------------------------------------------------

  val genPerspective: Gen[Perspective] =
    for
      d <- Gen.choose(300.0, 1200.0)
      w <- Gen.choose(200.0, 700.0)
      h <- Gen.choose(150.0, 500.0)
    yield Perspective.of(Length.mm(d), Length.mm(w), Length.mm(h)).toOption.get

  /** A linear frame, an angular frame, and the projection between them. */
  final case class TangentSetup(
      linear: Frame[Px],
      angular: Frame[Deg],
      perspective: Perspective,
      warp: Warp[Px, Deg]
  )

  val genTangentSetup: Gen[TangentSetup] =
    for
      wPx <- Gen.choose(320, 3840)
      hPx <- Gen.choose(240, 2160)
      p   <- genPerspective
    yield
      val lin = Frame.screen("linear", wPx, hPx).toOption.get
      val ang = Frame
        .angular(
          "angular",
          p.horizontalExtent.toDegrees,
          p.verticalExtent.toDegrees
        )
        .toOption
        .get
      TangentSetup(lin, ang, p, Warp.tangent(lin, ang, p))

  given Arbitrary[Perspective]  = Arbitrary(genPerspective)
  given Arbitrary[TangentSetup] = Arbitrary(genTangentSetup)

end Generators
