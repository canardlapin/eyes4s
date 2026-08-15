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

package eyes4s.kernel

/** Failures in the occupancy layer. */
enum SurfaceError derives CanEqual:
  case LengthMismatch(expected: Int, actual: Int)
  case NegativeWeight(index: Int, value: Double)
  case NegativeValue(index: Int, value: Double)
  case NonFiniteValue(index: Int, value: Double)
  case DegenerateTotal(total: Double)
  case GridMismatch(left: GridId, right: GridId)
  case GridIdentityConflict(id: GridId, left: GridSpec, right: GridSpec)
  case EmptyCollection(operation: String)

  def message: String = this match
    case LengthMismatch(e, a) =>
      s"Expected $e values, got $a."
    case NegativeWeight(i, v) =>
      s"Weights must be finite and non-negative; index $i was $v."
    case NegativeValue(i, v) =>
      s"This surface must be non-negative; index $i was $v. " +
        "A signed map is a Signed, not an Intensity or a Mass."
    case NonFiniteValue(i, v) =>
      s"Surface values must be finite; index $i was $v."
    case DegenerateTotal(t) =>
      s"Cannot normalise: total mass is $t. A surface with no mass has no " +
        "probability interpretation, and scaling it would invent one."
    case GridMismatch(l, r) =>
      s"Cannot combine surfaces on different grids: '$l' and '$r'. " +
        "Resample one onto the other's grid first."
    case GridIdentityConflict(id, left, right) =>
      s"Grid identity '$id' has conflicting specifications: " +
        s"left=${left.render}; right=${right.render}. " +
        "This is corrupt discretisation metadata, not an ordinary grid mismatch."
    case EmptyCollection(op) =>
      s"$op needs at least one surface."

/** The base of a logarithm, so that an entropy states its units. */
enum LogBase derives CanEqual:
  case E
  case Two

  def value: Double = this match
    case E   => math.E
    case Two => 2.0

  def unitName: String = this match
    case E   => "nats"
    case Two => "bits"

/** An entropy, carrying the base it was computed in. */
final case class Entropy(value: Double, base: LogBase) derives CanEqual:
  def render: String = f"$value%.4f ${base.unitName}"

/** Scalar values on a grid.
  *
  * ==Three types, because there are three things==
  *
  * `eyesim` has one. Its `Ops.eye_density` defines `+` as a mean and `/` as a
  * log-ratio, returns all three results tagged as densities, and drops the
  * bandwidth on every operation. A signed difference map is therefore accepted
  * by `fixation_entropy` as if it were a probability mass, and silently returns
  * a number that means nothing.
  *
  * Splitting the type makes that a compile error rather than a wrong answer:
  *
  *   - [[Mass]] is non-negative and sums to one. A probability over cells.
  *   - [[Intensity]] is non-negative with arbitrary scale. What an estimator
  *     produces before anyone decides to normalise.
  *   - [[Signed]] is any real. Differences, log-ratios, contrast maps.
  *
  * `entropy` is defined on `Mass` alone, so asking a difference map for its
  * entropy does not typecheck.
  *
  * ==Equality is by provenance, not by value==
  *
  * These deliberately are not case classes. Structural equality over a hundred
  * thousand doubles is expensive, rarely what a caller means, and would be
  * quietly wrong here anyway since `IArray` compares by reference. Two surfaces
  * are the same computation when their [[Provenance]] digests agree; two
  * surfaces are numerically close when a tolerance says so, which is a question
  * for a law suite rather than for `==`.
  */
sealed trait Surface[U <: Unit2D]:

  def grid: Grid[U]
  def values: IArray[Double]
  def provenance: Provenance

  final def size: Int          = values.length
  final def at(i: Int): Double = values(i)

  final def sum: Double =
    var s = 0.0
    var i = 0
    while i < values.length do
      s += values(i)
      i += 1
    s

  /** Value at a position, or `None` outside the grid. */
  final def sampleAt(p: Pt[U]): Option[Double] = grid.indexOf(p).map(values.apply)

  def render: String

end Surface

/** Non-negative, arbitrary scale: what an estimator produces. */
final class Intensity[U <: Unit2D] private[kernel] (
    val grid: Grid[U],
    val values: IArray[Double],
    val provenance: Provenance
) extends Surface[U]:
  def render: String = f"intensity(${grid.id}, sum=$sum%.4g)"

/** Non-negative and summing to one: a probability over cells.
  *
  * Note "over cells", not "a density". The values are cell probabilities, so
  * they depend on the grid's resolution. Multiply by [[Grid.cellArea]] to reach
  * a density per unit area.
  */
final class Mass[U <: Unit2D] private[kernel] (
    val grid: Grid[U],
    val values: IArray[Double],
    val provenance: Provenance
) extends Surface[U]:
  def render: String = s"mass(${grid.id})"

/** Any real value: differences, log-ratios, contrasts. */
final class Signed[U <: Unit2D] private[kernel] (
    val grid: Grid[U],
    val values: IArray[Double],
    val provenance: Provenance
) extends Surface[U]:
  def render: String = f"signed(${grid.id}, sum=$sum%.4g)"

object Surface:

  private def checkNonNegative(values: IArray[Double]): Either[SurfaceError, Unit] =
    var i = 0
    while i < values.length do
      val v = values(i)
      if !v.isFinite then return Left(SurfaceError.NonFiniteValue(i, v))
      if v < 0.0 then return Left(SurfaceError.NegativeValue(i, v))
      i += 1
    Right(())

  private def checkFinite(values: IArray[Double]): Either[SurfaceError, Unit] =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then return Left(SurfaceError.NonFiniteValue(i, values(i)))
      i += 1
    Right(())

  private def sized[U <: Unit2D](
      g: Grid[U],
      values: IArray[Double]
  ): Either[SurfaceError, Unit] =
    if values.length == g.size then Right(())
    else Left(SurfaceError.LengthMismatch(g.size, values.length))

  def intensity[U <: Unit2D](
      g: Grid[U],
      values: IArray[Double],
      provenance: Provenance
  ): Either[SurfaceError, Intensity[U]] =
    for
      _ <- sized(g, values)
      _ <- checkNonNegative(values)
    yield new Intensity(g, values, provenance)

  def signed[U <: Unit2D](
      g: Grid[U],
      values: IArray[Double],
      provenance: Provenance
  ): Either[SurfaceError, Signed[U]] =
    for
      _ <- sized(g, values)
      _ <- checkFinite(values)
    yield new Signed(g, values, provenance)

  /** Direct construction of a mass, for values already known to be a
    * distribution. Prefer [[Intensity.normalised]], which cannot be wrong.
    */
  def mass[U <: Unit2D](
      g: Grid[U],
      values: IArray[Double],
      provenance: Provenance,
      tolerance: Double = 1e-9
  ): Either[SurfaceError, Mass[U]] =
    for
      _ <- sized(g, values)
      _ <- checkNonNegative(values)
      total = values.sum
      m <-
        if math.abs(total - 1.0) <= tolerance then Right(new Mass(g, values, provenance))
        else Left(SurfaceError.DegenerateTotal(total))
    yield m

end Surface

extension [U <: Unit2D](i: Intensity[U])

  /** The **only** route from an unnormalised estimate to a probability mass.
    *
    * `eyesim` sum-normalises at roughly ten sites across four files with four
    * different epsilon guards, and nothing records whether a given map has
    * already been normalised -- so a normalised map and an unnormalised one are
    * routinely compared without complaint. Here there is one gate, and passing
    * through it is written into the provenance.
    */
  def normalised: Either[SurfaceError, Mass[U]] =
    val t = i.sum
    if !t.isFinite || t <= 0.0 then Left(SurfaceError.DegenerateTotal(t))
    else
      Right(
        new Mass(
          i.grid,
          IArray.tabulate(i.size)(k => i.values(k) / t),
          i.provenance.andThen(Provenance.Step.text("normalise", "of", "surface"))
        )
      )

  def scaled(k: Double): Either[SurfaceError, Intensity[U]] =
    Surface.intensity(
      i.grid,
      IArray.tabulate(i.size)(j => i.values(j) * k),
      i.provenance.andThen(Provenance.Step.num("scale", "by", k))
    )

extension [U <: Unit2D](m: Mass[U])

  /** Cell-wise difference. Signed, and typed as such. */
  def difference(that: Mass[U]): Either[SurfaceError, Signed[U]] =
    Agreement
      .grids(m.grid, that.grid)
      .flatMap { g =>
        Surface.signed(
          g,
          IArray.tabulate(m.size)(i => m.values(i) - that.values(i)),
          Provenance(
            ContentHash.combine(m.provenance.digest, that.provenance.digest),
            Vector(Provenance.Step("difference"))
          )
        )
      }

  /** Cell-wise log ratio, named for what it is.
    *
    * `eyesim` spells this `/`, which reads as division and is not. Cells where
    * either map has no mass are `None`-free here only because they are mapped
    * to zero explicitly; a log-ratio at a cell with no data is not defined and
    * pretending otherwise produces the infinities its `zapsmall` then hides.
    */
  def logRatio(that: Mass[U], floor: Double = 1e-12): Either[SurfaceError, Signed[U]] =
    Agreement
      .grids(m.grid, that.grid)
      .flatMap { g =>
        Surface.signed(
          g,
          IArray.tabulate(m.size) { i =>
            val a = math.max(m.values(i), floor)
            val b = math.max(that.values(i), floor)
            math.log(a / b)
          },
          Provenance(
            ContentHash.combine(m.provenance.digest, that.provenance.digest),
            Vector(Provenance.Step.num("logRatio", "floor", floor))
          )
        )
      }

  /** Shannon entropy over cells. Defined on a mass, and only on a mass.
    *
    * Cells with no mass contribute nothing, which is the limit of `p log p` as
    * `p` tends to zero, not a special case.
    */
  def entropy(base: LogBase = LogBase.E): Entropy =
    val lb = math.log(base.value)
    var h  = 0.0
    var i  = 0
    while i < m.size do
      val p = m.values(i)
      if p > 0.0 then h -= p * math.log(p) / lb
      i += 1
    Entropy(h, base)

  /** Entropy as a fraction of the maximum achievable on this grid.
    *
    * The normaliser is the cell count, so this compares across grids of
    * different resolution -- which raw entropy does not.
    */
  def relativeEntropy(base: LogBase = LogBase.E): Double =
    val maxH = math.log(m.size.toDouble) / math.log(base.value)
    if maxH <= 0.0 then 0.0 else entropy(base).value / maxH

object Mass:

  /** The mean of several masses, which is itself a mass.
    *
    * `eyesim` has no k-way average at all; its two-way average is spelled `+`
    * and its result is not marked as an average of anything.
    */
  def mean[U <: Unit2D](ms: Seq[Mass[U]]): Either[SurfaceError, Mass[U]] =
    if ms.isEmpty then Left(SurfaceError.EmptyCollection("Mass.mean"))
    else
      Agreement.allGrids(ms.map(_.grid)).flatMap {
        case None    => Left(SurfaceError.EmptyCollection("Mass.mean"))
        case Some(g) =>
          val n   = ms.length.toDouble
          val acc = Array.fill(g.size)(0.0)
          ms.foreach { m =>
            var i = 0
            while i < g.size do
              acc(i) += m.values(i) / n
              i += 1
          }
          Surface.mass(
            g,
            IArray.unsafeFromArray(acc),
            Provenance(
              ContentHash.combineAll(ms.map(_.provenance.digest)),
              Vector(Provenance.Step.num("mean", "n", ms.length.toDouble))
            ),
            tolerance = 1e-9
          )
      }

  /** A weighted mean, for unequal group sizes. */
  def weightedMean[U <: Unit2D](
      ms: Seq[(Double, Mass[U])]
  ): Either[SurfaceError, Mass[U]] =
    if ms.isEmpty then Left(SurfaceError.EmptyCollection("Mass.weightedMean"))
    else
      val totalW = ms.map(_._1).sum
      if !totalW.isFinite || totalW <= 0.0 then Left(SurfaceError.DegenerateTotal(totalW))
      else
        Agreement.allGrids(ms.map(_._2.grid)).flatMap {
          case None    => Left(SurfaceError.EmptyCollection("Mass.weightedMean"))
          case Some(g) =>
            val acc = Array.fill(g.size)(0.0)
            ms.foreach { case (w, m) =>
              var i = 0
              while i < g.size do
                acc(i) += m.values(i) * (w / totalW)
                i += 1
            }
            Surface.mass(
              g,
              IArray.unsafeFromArray(acc),
              Provenance(
                ContentHash.combineAll(ms.map(_._2.provenance.digest)),
                Vector(Provenance.Step.num("weightedMean", "n", ms.length.toDouble))
              ),
              tolerance = 1e-9
            )
        }

end Mass
