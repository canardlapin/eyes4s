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

import eyes4s.kernel.*

/** What happens to kernel mass that falls outside the grid.
  *
  * ==Why this is required rather than defaulted==
  *
  * A fixation near the edge of a display puts a large share of its kernel off
  * the grid, and what a library does with it changes the answer materially --
  * an edge fixation can lose half its mass. Every implementation makes a
  * choice; most make it silently, so two packages disagree on the same data and
  * neither says why.
  */
enum EdgePolicy derives CanEqual:

  /** Mass leaving the grid is gone. The estimate is a density over the grid
    * only, and its total is less than the input's -- honestly so, since the
    * fixation really was partly off-screen.
    */
  case Truncate

  /** Each kernel is rescaled so that the part landing on the grid carries the
    * whole of its point's mass. Total mass is preserved, at the cost of
    * inflating the density near the edges.
    */
  case Renormalise

/** Failures during estimation. */
enum EstimateError derives CanEqual:
  case FrameMismatch(measure: FrameId, grid: FrameId)
  case NoMass
  case DegenerateBandwidth(sigma: Double, cellSize: Double)
  case Surface(underlying: SurfaceError)

  def message: String = this match
    case FrameMismatch(m, g) =>
      s"The measure is in frame '$m' and the grid is over '$g'."
    case NoMass =>
      "The measure has no mass to smooth. A surface with nothing in it has no " +
        "density interpretation, and producing a flat one would invent data."
    case DegenerateBandwidth(s, c) =>
      f"A bandwidth of $s%.4g is smaller than a fifth of the $c%.4g cell size, " +
        "so the kernel would fall inside a single cell and the estimate would " +
        "be a histogram wearing a smoother's name. Use a coarser grid or a " +
        "wider bandwidth."
    case Surface(e) => e.message

/** Turns a discrete measure into a continuous-looking one.
  *
  * ==Separable, and therefore cheap==
  *
  * A two-dimensional Gaussian is the product of two one-dimensional ones, so
  * the convolution is two passes of a short kernel rather than one pass of a
  * square one. That is the whole reason the heaviest computation in this
  * library needs no linear-algebra dependency: `O(n * k)` with two 1-D passes
  * instead of `O(n * k^2)`, over plain arrays.
  */
trait Smoother[U <: Unit2D]:

  def bandwidth: Sigma[U]
  def edges: EdgePolicy

  def smooth(m: PointMeasure[U], g: Grid[U]): Either[EstimateError, Intensity[U]]

  /** Estimate and normalise in one step, which is the common case. */
  def density(m: PointMeasure[U], g: Grid[U]): Either[EstimateError, Mass[U]] =
    smooth(m, g).flatMap(_.normalised.left.map(EstimateError.Surface.apply))

object Smoother:

  /** An isotropic Gaussian kernel of the given standard deviation.
    *
    * The bandwidth is a standard deviation in frame units. Always. See
    * [[Sigma]] for why that is stated so insistently.
    */
  def gaussian[U <: Unit2D](
      sigma: Sigma[U],
      edgePolicy: EdgePolicy
  ): Smoother[U] =
    new Smoother[U]:
      val bandwidth: Sigma[U] = sigma
      val edges: EdgePolicy   = edgePolicy

      def smooth(m: PointMeasure[U], g: Grid[U]): Either[EstimateError, Intensity[U]] =
        for
          _ <- Either.cond(
            m.frame.id == g.frame.id,
            (),
            EstimateError.FrameMismatch(m.frame.id, g.frame.id)
          )
          _      <- Either.cond(m.total > 0.0, (), EstimateError.NoMass)
          _      <- checkResolution(g)
          binned <- m
            .binned(g)
            .left
            .map(_ => EstimateError.FrameMismatch(m.frame.id, g.frame.id))
          out <- Surface
            .intensity(
              g,
              convolve(binned, g),
              m.provenance.andThen(
                Provenance.Step(
                  "smooth",
                  Vector(
                    "kernel" -> Provenance.Param.Text("gaussian"),
                    "sigma"  -> Provenance.Param.Num(sigma.value),
                    "edges"  -> Provenance.Param.Text(edgePolicy.toString)
                  )
                )
              )
            )
            .left
            .map(EstimateError.Surface.apply)
        yield out

      /** A kernel narrower than the grid can express is a histogram in
        * disguise, and silently returning one is how a "smoothed" map ends up
        * with visible cell edges nobody can account for.
        */
      private def checkResolution(g: Grid[U]): Either[EstimateError, Unit] =
        val cell = math.min(g.cellWidth, g.cellHeight)
        if sigma.value >= cell / 5.0 then Right(())
        else Left(EstimateError.DegenerateBandwidth(sigma.value, cell))

      private def kernel(sigmaCells: Double): Array[Double] =
        val radius = math.max(1, math.ceil(3.0 * sigmaCells).toInt)
        val k      = Array.tabulate(2 * radius + 1) { i =>
          val d = (i - radius).toDouble
          math.exp(-0.5 * d * d / (sigmaCells * sigmaCells))
        }
        val s = k.sum
        var i = 0
        while i < k.length do
          k(i) = k(i) / s
          i += 1
        k

      private def convolve(binned: IArray[Double], g: Grid[U]): IArray[Double] =
        val kx = kernel(sigma.value / g.cellWidth)
        val ky = kernel(sigma.value / g.cellHeight)
        val rx = kx.length / 2
        val ry = ky.length / 2

        // Renormalisation is a SOURCE-side correction, not a destination-side
        // one. The policy says each kernel is rescaled so that the part landing
        // on the grid carries the whole of its point's mass -- so the factor
        // depends on where the point IS, not on which cell is receiving. A
        // destination-side average looks similar, preserves nothing, and was
        // what the first version of this did: an edge point came out at 0.64 of
        // its mass under a policy documented to preserve it.
        //
        // For a separable kernel the on-grid share factorises, so it is two
        // one-dimensional sums rather than a two-dimensional one.
        val onGridX = Array.tabulate(g.nx) { ix =>
          var w = 0.0
          var t = -rx
          while t <= rx do
            if ix + t >= 0 && ix + t < g.nx then w += kx(t + rx)
            t += 1
          w
        }
        val onGridY = Array.tabulate(g.ny) { iy =>
          var w = 0.0
          var t = -ry
          while t <= ry do
            if iy + t >= 0 && iy + t < g.ny then w += ky(t + ry)
            t += 1
          w
        }

        val source = Array.tabulate(g.size) { i =>
          edgePolicy match
            case EdgePolicy.Truncate    => binned(i)
            case EdgePolicy.Renormalise =>
              val f = onGridX(g.columnOf(i)) * onGridY(g.rowOf(i))
              if f > 0.0 then binned(i) / f else binned(i)
        }

        val rowPass = Array.fill(g.size)(0.0)
        var iy      = 0
        while iy < g.ny do
          var ix = 0
          while ix < g.nx do
            var acc = 0.0
            var t   = -rx
            while t <= rx do
              val sx = ix + t
              if sx >= 0 && sx < g.nx then acc += source(g.indexAt(sx, iy)) * kx(t + rx)
              t += 1
            rowPass(g.indexAt(ix, iy)) = acc
            ix += 1
          iy += 1

        val out = Array.fill(g.size)(0.0)
        var jx  = 0
        while jx < g.nx do
          var jy = 0
          while jy < g.ny do
            var acc = 0.0
            var t   = -ry
            while t <= ry do
              val sy = jy + t
              if sy >= 0 && sy < g.ny then acc += rowPass(g.indexAt(jx, sy)) * ky(t + ry)
              t += 1
            out(g.indexAt(jx, jy)) = acc
            jy += 1
          jx += 1

        IArray.unsafeFromArray(out)

end Smoother

/** Rules for choosing a bandwidth from the data.
  *
  * All of these are heuristics borrowed from univariate density estimation and
  * applied per axis. They are a starting point, not an answer: the right
  * bandwidth for a gaze density depends on what the map is for, and a rule that
  * minimises integrated squared error is not optimising for anything a reader
  * of an attention map cares about.
  *
  * The literature's usual default for gaze is one degree of visual angle,
  * which is roughly the width of the fovea and therefore a statement about the
  * measurement rather than about the data. Prefer it when the frame is angular.
  */
object Bandwidth:

  /** Silverman's rule of thumb, per axis, taking the smaller result.
    *
    * Assumes something near a normal distribution, which gaze data is not --
    * it oversmooths a multimodal map, which is what most scene-viewing data is.
    */
  def silverman[U <: Unit2D](m: PointMeasure[U]): Either[GeometryError, Sigma[U]] =
    rule(m, factor = 1.0)

  /** Scott's rule, per axis. Slightly wider than Silverman's. */
  def scott[U <: Unit2D](m: PointMeasure[U]): Either[GeometryError, Sigma[U]] =
    rule(m, factor = 1.06)

  /** Shared body: the narrower of the two axes' spreads, scaled by the sample
    * size.
    *
    * ==A degenerate axis is ignored, not fatal==
    *
    * Taking the minimum across both axes is the conservative choice for an
    * isotropic kernel -- it preserves detail rather than oversmoothing the
    * narrow direction. But an axis with no variance at all would then drive the
    * bandwidth to zero and refuse every estimate, which is wrong: gaze
    * confined to a horizontal line is unusual data, not invalid data. So an
    * axis with no spread is dropped, and only a cloud degenerate in BOTH
    * directions -- every point identical -- has no bandwidth.
    */
  private def rule[U <: Unit2D](
      m: PointMeasure[U],
      factor: Double
  ): Either[GeometryError, Sigma[U]] =
    val n = m.size
    if n < 2 then Left(GeometryError.NonPositiveSigma(0.0))
    else
      val spreads =
        Vector(spread(m.positions.toSeq.map(_.x)), spread(m.positions.toSeq.map(_.y)))
          .filter(_ > 0.0)
      spreads.minOption match
        case None    => Left(GeometryError.NonPositiveSigma(0.0))
        case Some(s) => Sigma.of[U](s * math.pow(n.toDouble, -1.0 / 6.0) * factor)

  /** The conventional foveal bandwidth: one degree. */
  def foveal: Either[GeometryError, Sigma[Unit2D.Deg]] = Sigma.deg(1.0)

  /** Robust spread: the smaller of the standard deviation and a scaled IQR, so
    * a few outlying fixations do not widen the kernel for the whole map.
    */
  private def spread(xs: Seq[Double]): Double =
    val n    = xs.length
    val mean = xs.sum / n
    val sd   = math.sqrt(xs.map(x => (x - mean) * (x - mean)).sum / (n - 1))
    val a    = xs.sorted
    val iqr  = a((3 * n) / 4) - a(n / 4)
    if iqr > 0.0 then math.min(sd, iqr / 1.349) else sd

end Bandwidth
