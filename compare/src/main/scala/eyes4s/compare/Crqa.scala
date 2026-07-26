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

package eyes4s.compare

import eyes4s.core.Scanpath
import eyes4s.kernel.*

/** Invalid CRQA parameters.
  *
  * Each case retains the rejected operand so an application can identify the
  * field that failed without parsing a message.
  */
enum CrqaParameterError derives CanEqual:
  case NonPositiveEmbeddingDimension(value: Int)
  case NonPositiveEmbeddingDelay(value: Int)
  case NonPositiveLineMinimum(value: Int)
  case InvalidTargetRecurrenceRate(value: Double)

  def message: String = this match
    case NonPositiveEmbeddingDimension(value) =>
      s"Embedding dimension must be positive, got $value."
    case NonPositiveEmbeddingDelay(value) =>
      s"Embedding delay must be positive, got $value."
    case NonPositiveLineMinimum(value) =>
      s"A CRQA line-length minimum must be positive, got $value."
    case InvalidTargetRecurrenceRate(value) =>
      s"Target recurrence rate must be finite and in (0, 1], got $value."

/** Number of two-dimensional positions in each delay-embedded state. */
opaque type EmbeddingDimension = Int

object EmbeddingDimension:
  val one: EmbeddingDimension = 1

  def of(value: Int): Either[CrqaParameterError, EmbeddingDimension] =
    if value > 0 then Right(value)
    else Left(CrqaParameterError.NonPositiveEmbeddingDimension(value))

  extension (dimension: EmbeddingDimension) def value: Int = dimension

/** Lag, in fixation positions, between adjacent members of an embedding. */
opaque type EmbeddingDelay = Int

object EmbeddingDelay:
  val one: EmbeddingDelay = 1

  def of(value: Int): Either[CrqaParameterError, EmbeddingDelay] =
    if value > 0 then Right(value)
    else Left(CrqaParameterError.NonPositiveEmbeddingDelay(value))

  extension (delay: EmbeddingDelay) def value: Int = delay

/** Smallest maximal line that contributes to a line-based statistic. */
opaque type LineLengthMinimum = Int

object LineLengthMinimum:
  val two: LineLengthMinimum = 2

  def of(value: Int): Either[CrqaParameterError, LineLengthMinimum] =
    if value > 0 then Right(value)
    else Left(CrqaParameterError.NonPositiveLineMinimum(value))

  extension (minimum: LineLengthMinimum) def value: Int = minimum

/** Requested recurrence density for data-derived radius selection.
  *
  * The selected inclusive radius yields at least this density. It can yield
  * more when several embedded distances tie at the selected quantile.
  */
opaque type TargetRecurrenceRate = Double

object TargetRecurrenceRate:
  def of(value: Double): Either[CrqaParameterError, TargetRecurrenceRate] =
    if value.isFinite && value > 0.0 && value <= 1.0 then Right(value)
    else Left(CrqaParameterError.InvalidTargetRecurrenceRate(value))

  extension (rate: TargetRecurrenceRate) def value: Double = rate

/** How the recurrence threshold is obtained.
  *
  * The boundary convention is part of each case name. A distance exactly equal
  * to the selected radius is recurrent.
  */
sealed trait RadiusSelection[U <: Unit2D] derives CanEqual

object RadiusSelection:
  final case class FixedInclusive[U <: Unit2D](maximum: Distance[U]) extends RadiusSelection[U]

  /** Select the empirical distance quantile whose inclusive recurrence matrix
    * has at least the requested density.
    */
  final case class TargetAtLeast[U <: Unit2D](rate: TargetRecurrenceRate)
      extends RadiusSelection[U]

/** Which time axis supplies vertical lines for LAM and TT.
  *
  * A cross-recurrence matrix need not be square, so its vertical and horizontal
  * line statistics need not agree. The orientation is therefore data, not an
  * undocumented array convention.
  */
enum LaminarAxis derives CanEqual:
  case AlongLeft, AlongRight

/** Fully validated CRQA parameters. */
final case class CrqaConfig[U <: Unit2D](
    embeddingDimension: EmbeddingDimension,
    embeddingDelay: EmbeddingDelay,
    radiusSelection: RadiusSelection[U],
    minimumDiagonalLine: LineLengthMinimum,
    minimumLaminarLine: LineLengthMinimum,
    laminarAxis: LaminarAxis
) derives CanEqual

object CrqaConfig:
  def standard[U <: Unit2D](radius: Distance[U]): CrqaConfig[U] =
    CrqaConfig(
      EmbeddingDimension.one,
      EmbeddingDelay.one,
      RadiusSelection.FixedInclusive(radius),
      LineLengthMinimum.two,
      LineLengthMinimum.two,
      LaminarAxis.AlongLeft
    )

enum CrqaOperand derives CanEqual:
  case LeftPath, RightPath

/** Recoverable failures during CRQA. */
enum CrqaError derives CanEqual:
  case Geometry(underlying: GeometryError)
  case TooShort(
      operand: CrqaOperand,
      observedFixations: Int,
      requiredFixations: Long,
      embeddingDimension: Int,
      embeddingDelay: Int
  )
  case MatrixTooLarge(
      leftStates: Int,
      rightStates: Int,
      requiredCells: Long,
      maximumCells: Long
  )
  case NonFinitePosition(operand: CrqaOperand, index: Int, x: Double, y: Double)
  case NonFiniteEmbeddedDistance(leftState: Int, rightState: Int, value: Double)
  case SelectedRadius(underlying: GeometryError)

  def message: String = this match
    case Geometry(error)                                         => error.message
    case TooShort(operand, observed, required, dimension, delay) =>
      s"$operand has $observed fixations, but embedding dimension $dimension " +
        s"with delay $delay requires at least $required."
    case MatrixTooLarge(left, right, required, maximum) =>
      s"CRQA for $left left states and $right right states requires $required " +
        s"matrix cells, above the platform maximum of $maximum."
    case NonFinitePosition(operand, index, x, y) =>
      s"$operand fixation $index has a non-finite position ($x, $y)."
    case NonFiniteEmbeddedDistance(left, right, value) =>
      s"Embedded states left[$left] and right[$right] have non-finite distance $value."
    case SelectedRadius(error) =>
      s"The selected recurrence radius was invalid: ${error.message}"

/** A rectangular, row-major recurrence matrix.
  *
  * Rows follow the left embedded path and columns follow the right. Public
  * lookup is total: an invalid coordinate returns `None`.
  */
final class RecurrenceMatrix private[compare] (
    val rowCount: Int,
    val columnCount: Int,
    private val cells: IArray[Boolean]
):
  def get(row: Int, column: Int): Option[Boolean] =
    Option.when(row >= 0 && row < rowCount && column >= 0 && column < columnCount)(
      cells(row * columnCount + column)
    )

  def recurrentCount: Int =
    var count = 0
    var i     = 0
    while i < cells.length do
      if cells(i) then count += 1
      i += 1
    count

  def toRows: Vector[Vector[Boolean]] =
    Vector.tabulate(rowCount)(row =>
      Vector.tabulate(columnCount)(column => recurrent(row, column))
    )

  def render: String =
    s"recurrence-matrix(${rowCount}x$columnCount, recurrent=$recurrentCount)"

  private[compare] def recurrent(row: Int, column: Int): Boolean =
    cells(row * columnCount + column)

end RecurrenceMatrix

/** The standard recurrence-quantification statistics.
  *
  * RR, DET, and LAM are proportions in `[0, 1]`. `diagonalEntropyNats` names
  * its logarithm convention. TT is a mean line length in embedded time steps,
  * and `longestDiagonal` is L_max in the same steps.
  */
final case class CrqaMetrics private[compare] (
    recurrenceRate: Double,
    determinism: Double,
    laminarity: Double,
    diagonalEntropyNats: Double,
    trappingTime: Double,
    longestDiagonal: Int
) derives CanEqual

/** CRQA output, including the evidence from which the summary was computed. */
final case class CrqaResult[U <: Unit2D] private[compare] (
    matrix: RecurrenceMatrix,
    selectedRadius: Distance[U],
    config: CrqaConfig[U],
    metrics: CrqaMetrics
)

/** Cross-recurrence quantification analysis.
  *
  * The implementation follows the line-histogram definitions in Marwan,
  * Romano, Thiel & Kurths (2007): recurrence density is computed from an
  * inclusive epsilon neighbourhood; DET and entropy use maximal diagonal
  * lines; LAM and TT use maximal lines on the configured laminar axis.
  *
  * Embedding and delay are applied before any distance is measured. Embedded
  * states use the Euclidean norm over all `2 * dimension` coordinates. A fixed
  * recurrence density is implemented by selecting a quantile of that same
  * distance matrix, so it is not a decorative parameter.
  */
object Crqa:

  def analyse[U <: Unit2D](
      left: Scanpath[U],
      right: Scanpath[U],
      config: CrqaConfig[U]
  ): Either[CrqaError, CrqaResult[U]] =
    for
      _ <- Agreement.frames(left.frame, right.frame).left.map(CrqaError.Geometry.apply)
      leftEmbedded  <- embed(left, config, CrqaOperand.LeftPath)
      rightEmbedded <- embed(right, config, CrqaOperand.RightPath)
      _             <- matrixCapacity(leftEmbedded.length, rightEmbedded.length)
      distances     <- distanceMatrix(leftEmbedded, rightEmbedded)
      radius        <- selectRadius(config.radiusSelection, distances)
      matrix = recurrenceMatrix(
        leftEmbedded.length,
        rightEmbedded.length,
        distances,
        radius
      )
      metrics = quantify(matrix, config)
    yield CrqaResult(matrix, radius, config, metrics)

  private def embed[U <: Unit2D](
      path: Scanpath[U],
      config: CrqaConfig[U],
      operand: CrqaOperand
  ): Either[CrqaError, Vector[Vector[Pt[U]]]] =
    val bad = (0 until path.n).find(index => !path.fixations(index).centre.isFinite)
    bad match
      case Some(index) =>
        val point = path.fixations(index).centre
        Left(CrqaError.NonFinitePosition(operand, index, point.x, point.y))
      case None =>
        val dimension = config.embeddingDimension.value
        val delay     = config.embeddingDelay.value
        val required  = 1L + (dimension.toLong - 1L) * delay.toLong
        if path.n.toLong < required then
          Left(CrqaError.TooShort(operand, path.n, required, dimension, delay))
        else
          val embeddedStates = path.n - required.toInt + 1
          Right(
            Vector.tabulate(embeddedStates)(start =>
              Vector.tabulate(dimension)(offset =>
                path.fixations(start + offset * delay).centre
              )
            )
          )

  private def matrixCapacity(
      leftStates: Int,
      rightStates: Int
  ): Either[CrqaError, Unit] =
    val required = leftStates.toLong * rightStates.toLong
    Either.cond(
      required <= Int.MaxValue.toLong,
      (),
      CrqaError.MatrixTooLarge(
        leftStates,
        rightStates,
        required,
        Int.MaxValue.toLong
      )
    )

  private def distanceMatrix[U <: Unit2D](
      left: Vector[Vector[Pt[U]]],
      right: Vector[Vector[Pt[U]]]
  ): Either[CrqaError, IArray[Double]] =
    val values                     = Array.ofDim[Double](left.length * right.length)
    var failure: Option[CrqaError] = None
    var i                          = 0
    while i < left.length && failure.isEmpty do
      var j = 0
      while j < right.length && failure.isEmpty do
        var distance = 0.0
        var k        = 0
        while k < left(i).length do
          val dx = left(i)(k).x - right(j)(k).x
          val dy = left(i)(k).y - right(j)(k).y
          distance = math.hypot(distance, math.hypot(dx, dy))
          k += 1
        if !distance.isFinite then
          failure = Some(CrqaError.NonFiniteEmbeddedDistance(i, j, distance))
        else values(i * right.length + j) = distance
        j += 1
      i += 1
    failure.toLeft(IArray.from(values))

  private def selectRadius[U <: Unit2D](
      selection: RadiusSelection[U],
      distances: IArray[Double]
  ): Either[CrqaError, Distance[U]] =
    selection match
      case fixed: RadiusSelection.FixedInclusive[U] => Right(fixed.maximum)
      case target: RadiusSelection.TargetAtLeast[U] =>
        val sorted = IArray.genericWrapArray(distances).toArray.sorted
        val index  = math.max(0, math.ceil(target.rate.value * sorted.length).toInt - 1)
        Distance.of[U](sorted(index)).left.map(CrqaError.SelectedRadius.apply)

  private def recurrenceMatrix[U <: Unit2D](
      rows: Int,
      columns: Int,
      distances: IArray[Double],
      radius: Distance[U]
  ): RecurrenceMatrix =
    val cells = IArray.tabulate(distances.length)(index => distances(index) <= radius.value)
    new RecurrenceMatrix(rows, columns, cells)

  private def quantify[U <: Unit2D](
      matrix: RecurrenceMatrix,
      config: CrqaConfig[U]
  ): CrqaMetrics =
    val recurrent = matrix.recurrentCount
    val all       = matrix.rowCount * matrix.columnCount
    val diagonals = diagonalLines(matrix).filter(_ >= config.minimumDiagonalLine.value)
    val laminar   = laminarLines(matrix, config.laminarAxis)
      .filter(_ >= config.minimumLaminarLine.value)

    val diagonalPoints = diagonals.sum
    val laminarPoints  = laminar.sum

    CrqaMetrics(
      recurrenceRate = ratio(recurrent, all),
      determinism = ratio(diagonalPoints, recurrent),
      laminarity = ratio(laminarPoints, recurrent),
      diagonalEntropyNats = entropyNats(diagonals),
      trappingTime =
        if laminar.isEmpty then 0.0 else laminar.sum.toDouble / laminar.length.toDouble,
      longestDiagonal = diagonals.maxOption.getOrElse(0)
    )

  private def diagonalLines(matrix: RecurrenceMatrix): Vector[Int] =
    val lengths = Vector.newBuilder[Int]
    var row     = 0
    while row < matrix.rowCount do
      var column = 0
      while column < matrix.columnCount do
        val starts =
          matrix.recurrent(row, column) &&
            (row == 0 || column == 0 || !matrix.recurrent(row - 1, column - 1))
        if starts then
          var length = 0
          var i      = row
          var j      = column
          while i < matrix.rowCount &&
            j < matrix.columnCount &&
            matrix.recurrent(i, j)
          do
            length += 1
            i += 1
            j += 1
          lengths += length
        column += 1
      row += 1
    lengths.result()

  private def laminarLines(
      matrix: RecurrenceMatrix,
      axis: LaminarAxis
  ): Vector[Int] =
    axis match
      case LaminarAxis.AlongLeft =>
        axisLines(
          matrix.rowCount,
          matrix.columnCount,
          (row, column) => matrix.recurrent(row, column)
        )
      case LaminarAxis.AlongRight =>
        axisLines(
          matrix.columnCount,
          matrix.rowCount,
          (column, row) => matrix.recurrent(row, column)
        )

  /** Maximal true lines along the first coordinate supplied by `recurrent`. */
  private def axisLines(
      along: Int,
      across: Int,
      recurrent: (Int, Int) => Boolean
  ): Vector[Int] =
    val lengths     = Vector.newBuilder[Int]
    var acrossIndex = 0
    while acrossIndex < across do
      var alongIndex = 0
      while alongIndex < along do
        val starts =
          recurrent(alongIndex, acrossIndex) &&
            (alongIndex == 0 || !recurrent(alongIndex - 1, acrossIndex))
        if starts then
          var length = 0
          var i      = alongIndex
          while i < along && recurrent(i, acrossIndex) do
            length += 1
            i += 1
          lengths += length
        alongIndex += 1
      acrossIndex += 1
    lengths.result()

  private def ratio(numerator: Int, denominator: Int): Double =
    if denominator == 0 then 0.0 else numerator.toDouble / denominator.toDouble

  private def entropyNats(lengths: Vector[Int]): Double =
    if lengths.isEmpty then 0.0
    else
      val total = lengths.length.toDouble
      lengths
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .valuesIterator
        .map { count =>
          val probability = count.toDouble / total
          -probability * math.log(probability)
        }
        .sum

end Crqa
