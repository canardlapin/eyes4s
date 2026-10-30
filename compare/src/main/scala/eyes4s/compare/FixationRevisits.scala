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

/** Refusals name the invalid parameter or both operands of a failed calculation. */
enum RevisitError derives CanEqual:
  case NonPositiveRadius(value: Double)
  case InvalidLagRange(minimum: Int, maximum: Option[Int])
  case InvalidMaximumWork(value: Long)
  case Frames(error: GeometryError)
  case NonFiniteReferencePoint(index: Int, x: Double, y: Double)
  case NonFiniteReferenceMass(total: Double)
  case NonFiniteDistance(fixationIndex: Int, otherIndex: Int, reference: Boolean, value: Double)
  case WorkLimit(fixations: Int, referencePoints: Int, required: BigInt, maximum: Long)

  def message: String = this match
    case NonPositiveRadius(v)    => s"Revisit open radius=$v must be positive and finite."
    case InvalidLagRange(lo, hi) =>
      s"Revisit lag minimum=$lo must be positive and maximum=$hi must be at least minimum."
    case InvalidMaximumWork(v)            => s"Revisit maximumWork=$v must be positive."
    case Frames(e)                        => e.message
    case NonFiniteReferencePoint(i, x, y) =>
      s"Revisit reference[$i]=($x,$y) must be finite."
    case NonFiniteReferenceMass(total) => s"Revisit reference total mass=$total must be finite."
    case NonFiniteDistance(i, j, reference, value) =>
      s"Revisit distance from fixation[$i] to ${
          if reference then "reference" else "fixation"
        }[$j] is nonfinite: $value."
    case WorkLimit(n, m, required, maximum) =>
      s"Revisit fixations=$n, referencePoints=$m require at most $required work units, exceeding maximum=$maximum."

/** The incoming step, not a claim of exit from a previously visited region. */
enum RevisitDeparture derives CanEqual:
  case Unrestricted, IncomingStepAtLeastRadius

/** A strictly positive, exclusive radius and inclusive bounds in fixation positions.
  * `None` is unlimited lookback. Work units bound distance evaluations and reference
  * point visits before any working arrays are allocated; the bound is conservative
  * because an observed non-departure may skip a whole row.
  */
final class RevisitConfig[U <: Unit2D] private (
    val openRadius: Distance[U],
    val minimumLag: Int,
    val maximumLag: Option[Int],
    val departure: RevisitDeparture,
    val distance: FixationGroundDistance,
    val maximumWork: Long
)

object RevisitConfig:
  def of[U <: Unit2D](
      openRadius: Distance[U],
      minimumLag: Int = 2,
      maximumLag: Option[Int] = None,
      departure: RevisitDeparture = RevisitDeparture.IncomingStepAtLeastRadius,
      distance: FixationGroundDistance = FixationGroundDistance.Euclidean,
      maximumWork: Long = 50000000L
  ): Either[RevisitError, RevisitConfig[U]] =
    if !openRadius.value.isFinite || openRadius.value <= 0 then
      Left(RevisitError.NonPositiveRadius(openRadius.value))
    else if minimumLag < 1 || maximumLag.exists(_ < minimumLag) then
      Left(RevisitError.InvalidLagRange(minimumLag, maximumLag))
    else if maximumWork <= 0 then Left(RevisitError.InvalidMaximumWork(maximumWork))
    else
      Right(
        new RevisitConfig(openRadius, minimumLag, maximumLag, departure, distance, maximumWork)
      )

enum RevisitEligibility derives CanEqual:
  case InsufficientHistory, IncomingStepTooShort, Eligible

enum RevisitUnavailable derives CanEqual:
  case EmptyReference, ZeroReferenceMass, NoDepartureSupport

/** A probability conditional on this row's observed history and arrival rule.
  * The retained masses are in the supplied reference measure's original units.
  */
sealed trait RevisitExpectation

object RevisitExpectation:
  case object NotRequested                                 extends RevisitExpectation
  case object NotAnOpportunity                             extends RevisitExpectation
  final case class Unavailable(reason: RevisitUnavailable) extends RevisitExpectation
  final class Available private[compare] (
      val matchedMass: Double,
      val eligibleMass: Double,
      val eligiblePoints: Int
  ) extends RevisitExpectation:
    def probability: Double = matchedMass / eligibleMass

/** Most recent qualifying fixation, using zero-based input positions. */
final class RevisitMatch[U <: Unit2D] private[compare] (
    val index: Int,
    val lag: Int,
    val distance: Distance[U]
)

final class RevisitRow[U <: Unit2D] private[compare] (
    val index: Int,
    val onset: Instant,
    val eligibility: RevisitEligibility,
    val nearestEligibleDistance: Option[Distance[U]],
    val matched: Option[RevisitMatch[U]],
    val expectation: RevisitExpectation
):
  def opportunity: Boolean = eligibility == RevisitEligibility.Eligible
  def revisit: Boolean     = matched.nonEmpty

sealed trait RevisitExpectedSummary

object RevisitExpectedSummary:
  case object NotRequested    extends RevisitExpectedSummary
  case object NoOpportunities extends RevisitExpectedSummary
  final class Incomplete private[compare] (val unavailableIndices: Vector[Int])
      extends RevisitExpectedSummary
  final class Available private[compare] (
      val expectedRevisits: Double,
      val expectedRate: Double,
      val excessRevisits: Double,
      val excessRate: Double
  ) extends RevisitExpectedSummary

final class RevisitSummary private[compare] (
    val fixationCount: Int,
    val opportunities: Int,
    val revisits: Int,
    val rate: Option[Double],
    val expectation: RevisitExpectedSummary
)

/** Keeps the actual inputs, geometry, reference provenance and every fixation row.
  * A missing expectation never becomes zero or silently reduces the denominator.
  */
final class RevisitResult[U <: Unit2D] private[compare] (
    val scanpath: Scanpath[U],
    val config: RevisitConfig[U],
    val reference: Option[PointMeasure[U]],
    val rows: Vector[RevisitRow[U]],
    val summary: RevisitSummary
)

object FixationRevisits:
  /** Exact strict-radius return scoring over a validated, ordered scanpath.
    *
    * Reference weights define the spatial baseline explicitly: uniform masses
    * reproduce fixation-weighted pooling; duration masses ask a different question.
    * Subject/item pooling and episode exclusions belong to the caller's design.
    *
    * Rolling integer coverage counts implement the union of eligible historical
    * neighbourhoods. For n fixations, m reference positions and history size L,
    * time is O(nL + nm) and auxiliary storage is O(m), plus O(n) returned rows.
    * No pairwise distance matrix is constructed. Eligible masses are summed directly
    * in reference order, avoiding cancellation from total-minus-excluded mass.
    */
  def analyze[U <: Unit2D](
      path: Scanpath[U],
      config: RevisitConfig[U],
      reference: Option[PointMeasure[U]] = None
  ): Either[RevisitError, RevisitResult[U]] =
    val count = reference.fold(0)(_.size)
    for
      _ <- reference.fold[Either[RevisitError, Unit]](Right(()))(r =>
        Agreement.frames(path.frame, r.frame).left.map(RevisitError.Frames.apply).map(_ => ())
      )
      _      <- admitWork(path.n, count, config)
      total  <- reference.fold[Either[RevisitError, Double]](Right(0.0))(validateReference)
      result <- new Runner(path, config, reference, total).run()
    yield result

  private def admitWork[U <: Unit2D](
      n: Int,
      m: Int,
      c: RevisitConfig[U]
  ): Either[RevisitError, Unit] =
    val additions   = BigInt(math.max(0, n - c.minimumLag))
    val width       = c.maximumLag.fold(additions)(hi => BigInt(hi) - c.minimumLag + 1)
    val ramp        = additions.min(width)
    val comparisons = ramp * (ramp + 1) / 2 + (additions - ramp) * width
    val removals    = c.maximumLag.fold(BigInt(0))(hi => (BigInt(n) - hi - 1).max(BigInt(0)))
    val departure   =
      if c.departure == RevisitDeparture.IncomingStepAtLeastRadius then additions else BigInt(0)
    val work =
      comparisons + departure + BigInt(m) * (1 + additions + removals + additions + departure)
    Either.cond(work <= c.maximumWork, (), RevisitError.WorkLimit(n, m, work, c.maximumWork))

  private def validateReference[U <: Unit2D](r: PointMeasure[U]): Either[RevisitError, Double] =
    val total                         = new Sum
    var failure: Option[RevisitError] = None
    var i                             = 0
    while i < r.size && failure.isEmpty do
      val p = r.positions(i)
      if !p.x.isFinite || !p.y.isFinite then
        failure = Some(RevisitError.NonFiniteReferencePoint(i, p.x, p.y))
      else total.add(r.weights(i))
      i += 1
    failure match
      case Some(e) => Left(e)
      case None    =>
        Either.cond(
          total.value.isFinite,
          total.value,
          RevisitError.NonFiniteReferenceMass(total.value)
        )

  /** Neumaier summation in the input order, including original (unscaled) masses. */
  private final class Sum:
    private var sum              = 0.0
    private var correction       = 0.0
    def add(value: Double): Unit =
      val next = sum + value
      correction += (if math.abs(sum) >= math.abs(value) then (sum - next) + value
                     else (value - next) + sum)
      sum = next
    def value: Double = sum + correction

  private final class Runner[U <: Unit2D](
      path: Scanpath[U],
      c: RevisitConfig[U],
      reference: Option[PointMeasure[U]],
      total: Double
  ):
    private val radius                        = c.openRadius.value
    private val coverage                      = Array.fill(reference.fold(0)(_.size))(0)
    private var failure: Option[RevisitError] = None

    private def distance(i: Int, j: Int, other: Pt[U], isReference: Boolean): Double =
      val point = path.fixations(i).centre
      val dx    = math.abs(point.x - other.x)
      val dy    = math.abs(point.y - other.y)
      val d     = c.distance match
        case FixationGroundDistance.Euclidean => FdlibmHypot(dx, dy)
        case FixationGroundDistance.Manhattan => dx + dy
      if !d.isFinite then failure = Some(RevisitError.NonFiniteDistance(i, j, isReference, d))
      d

    private def updateCoverage(index: Int, delta: Int): Unit = reference.foreach { r =>
      var j = 0
      while j < r.size && failure.isEmpty do
        if distance(index, j, r.positions(j), true) < radius then coverage(j) += delta
        j += 1
    }

    private def expected(i: Int, eligible: Boolean): RevisitExpectation = reference match
      case None                 => RevisitExpectation.NotRequested
      case Some(_) if !eligible => RevisitExpectation.NotAnOpportunity
      case Some(r) if r.isEmpty =>
        RevisitExpectation.Unavailable(RevisitUnavailable.EmptyReference)
      case Some(_) if total == 0.0 =>
        RevisitExpectation.Unavailable(RevisitUnavailable.ZeroReferenceMass)
      case Some(r) =>
        val numerator   = new Sum
        val denominator = new Sum
        var points      = 0
        var j           = 0
        while j < r.size && failure.isEmpty do
          val allowed = c.departure == RevisitDeparture.Unrestricted ||
            distance(i - 1, j, r.positions(j), true) >= radius
          if allowed then
            points += 1
            denominator.add(r.weights(j))
            if coverage(j) > 0 then numerator.add(r.weights(j))
          j += 1
        if denominator.value > 0.0 then
          new RevisitExpectation.Available(numerator.value, denominator.value, points)
        else RevisitExpectation.Unavailable(RevisitUnavailable.NoDepartureSupport)

    def run(): Either[RevisitError, RevisitResult[U]] =
      val rows = Vector.newBuilder[RevisitRow[U]]
      var i    = 0
      while i < path.n && failure.isEmpty do
        val hi = i - c.minimumLag
        val lo = c.maximumLag.fold(0)(max => math.max(0, i - max))
        if hi >= 0 then updateCoverage(hi, 1)
        c.maximumLag.foreach { max =>
          // Compare before subtraction: Int.MaxValue is a valid unlimited-in-practice bound.
          if i > max then updateCoverage(i - max - 1, -1)
        }
        val eligibility =
          if hi < 0 then RevisitEligibility.InsufficientHistory
          else if c.departure == RevisitDeparture.IncomingStepAtLeastRadius &&
            distance(i, i - 1, path.fixations(i - 1).centre, false) < radius
          then RevisitEligibility.IncomingStepTooShort
          else RevisitEligibility.Eligible
        var nearest: Option[Distance[U]]     = None
        var matched: Option[RevisitMatch[U]] = None
        if eligibility == RevisitEligibility.Eligible then
          var nearestValue = Double.PositiveInfinity
          var matchedIndex = -1
          var matchedValue = 0.0
          var j            = lo
          while j <= hi && failure.isEmpty do
            val d = distance(i, j, path.fixations(j).centre, false)
            if failure.isEmpty then
              nearestValue = math.min(nearestValue, d)
              if d < radius then
                matchedIndex = j
                matchedValue = d
            j += 1
          if failure.isEmpty then
            // Eligibility guarantees a nonempty history and distance() checked every value.
            nearest = Distance.of[U](nearestValue).toOption
            if matchedIndex >= 0 then
              matched = Distance
                .of[U](matchedValue)
                .toOption
                .map(new RevisitMatch(matchedIndex, i - matchedIndex, _))
        val expectation = expected(i, eligibility == RevisitEligibility.Eligible)
        rows += new RevisitRow(
          i,
          path.fixations(i).span.onset,
          eligibility,
          nearest,
          matched,
          expectation
        )
        i += 1
      failure match
        case Some(e) => Left(e)
        case None    =>
          val built = rows.result()
          Right(
            new RevisitResult(path, c, reference, built, summarize(built, reference.nonEmpty))
          )

  private def summarize[U <: Unit2D](
      rows: Vector[RevisitRow[U]],
      hasReference: Boolean
  ): RevisitSummary =
    val opportunities = rows.count(_.opportunity)
    val revisits      = rows.count(_.revisit)
    val rate          = Option.when(opportunities > 0)(revisits.toDouble / opportunities)
    val expectation   =
      if !hasReference then RevisitExpectedSummary.NotRequested
      else if opportunities == 0 then RevisitExpectedSummary.NoOpportunities
      else
        val unavailable = rows.collect {
          case row
              if row.opportunity && !row.expectation
                .isInstanceOf[RevisitExpectation.Available] =>
            row.index
        }
        if unavailable.nonEmpty then new RevisitExpectedSummary.Incomplete(unavailable)
        else
          val sum = new Sum
          rows.foreach { row =>
            row.expectation match
              case e: RevisitExpectation.Available => sum.add(e.probability)
              case _                               => ()
          }
          val expected = sum.value
          new RevisitExpectedSummary.Available(
            expected,
            expected / opportunities,
            revisits - expected,
            revisits.toDouble / opportunities - expected / opportunities
          )
    new RevisitSummary(rows.size, opportunities, revisits, rate, expectation)
