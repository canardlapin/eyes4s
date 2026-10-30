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

import eyes4s.compare.*
import eyes4s.core.Scanpath
import eyes4s.kernel.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Independent exhaustive predicates for bounded, finite inputs. This oracle never
  * maintains rolling coverage or subtracts reference mass. Generators should include
  * coincident points, exact boundaries, finite lag windows and unequal/zero weights.
  */
trait FixationRevisitLaws extends Laws:
  def analysis[U <: Unit2D](
      inputs: Gen[(Scanpath[U], RevisitConfig[U], Option[PointMeasure[U]])],
      tolerance: Tolerance
  ): RuleSet = new DefaultRuleSet(
    "fixationRevisits",
    None,
    "cardinality, order and observed denominator" -> forAll(inputs) { (p, c, ref) =>
      FixationRevisits.analyze(p, c, ref).exists { result =>
        val rows = result.rows
        val opp  = rows.count(_.opportunity)
        val rev  = rows.count(_.revisit)
        rows.size == p.n && rows.map(_.index) == (0 until p.n).toVector &&
        rows.map(_.onset) == p.fixations.toVector.map(_.span.onset) &&
        result.summary.fixationCount == p.n && result.summary.opportunities == opp &&
        result.summary.revisits == rev && rev <= opp &&
        same(result.summary.rate, Option.when(opp > 0)(rev.toDouble / opp), tolerance)
      }
    },
    "strict radius, lag bounds, arrival and most recent match" -> forAll(inputs) {
      (p, c, ref) =>
        FixationRevisits
          .analyze(p, c, ref)
          .exists(_.rows.forall { row =>
            val i       = row.index
            val history = eligible(i, c)
            val state   =
              if history.isEmpty then RevisitEligibility.InsufficientHistory
              else if c.departure == RevisitDeparture.IncomingStepAtLeastRadius &&
                distance(
                  p.fixations(i).centre,
                  p.fixations(i - 1).centre,
                  c
                ) < c.openRadius.value
              then RevisitEligibility.IncomingStepTooShort
              else RevisitEligibility.Eligible
            val distances =
              history.map(j => j -> distance(p.fixations(i).centre, p.fixations(j).centre, c))
            val near = distances.filter(_._2 < c.openRadius.value).lastOption
            row.eligibility == state &&
            (if state != RevisitEligibility.Eligible then
               row.matched.isEmpty && row.nearestEligibleDistance.isEmpty
             else
               same(
                 row.nearestEligibleDistance.map(_.value),
                 distances.map(_._2).minOption,
                 tolerance
               ) &&
               row.matched.map(_.index) == near.map(_._1) &&
               row.matched.map(_.lag) == near.map(hit => i - hit._1) &&
               same(row.matched.map(_.distance.value), near.map(_._2), tolerance))
          })
    },
    "reference union, weights and conditional support" -> forAll(inputs) { (p, c, ref) =>
      FixationRevisits
        .analyze(p, c, ref)
        .exists(_.rows.forall { row =>
          ref match
            case None => row.expectation == RevisitExpectation.NotRequested
            case Some(_) if !row.opportunity =>
              row.expectation == RevisitExpectation.NotAnOpportunity
            case Some(r) if r.isEmpty =>
              row.expectation == RevisitExpectation
                .Unavailable(RevisitUnavailable.EmptyReference)
            case Some(r) if r.weights.toVector.sum == 0 =>
              row.expectation == RevisitExpectation
                .Unavailable(RevisitUnavailable.ZeroReferenceMass)
            case Some(r) =>
              val allowed = r.positions.indices.filter(k =>
                c.departure == RevisitDeparture.Unrestricted ||
                  distance(
                    p.fixations(row.index - 1).centre,
                    r.positions(k),
                    c
                  ) >= c.openRadius.value
              )
              val hits = allowed.filter(k =>
                eligible(row.index, c).exists(j =>
                  distance(p.fixations(j).centre, r.positions(k), c) < c.openRadius.value
                )
              )
              val denominator = allowed.map(r.weights(_)).sum
              val numerator   = hits.map(r.weights(_)).sum
              if denominator == 0 then
                row.expectation == RevisitExpectation
                  .Unavailable(RevisitUnavailable.NoDepartureSupport)
              else
                row.expectation match
                  case e: RevisitExpectation.Available =>
                    e.eligiblePoints == allowed.size &&
                    tolerance.approxEquals(e.eligibleMass, denominator) &&
                    tolerance.approxEquals(e.matchedMass, numerator) &&
                    tolerance.approxEquals(e.probability, numerator / denominator) &&
                    e.probability >= 0 && e.probability <= 1
                  case _ => false
        })
    },
    "expected totals retain incomplete opportunities" -> forAll(inputs) { (p, c, ref) =>
      FixationRevisits.analyze(p, c, ref).exists { result =>
        val s       = result.summary
        val opp     = result.rows.filter(_.opportunity)
        val missing =
          opp.filterNot(_.expectation.isInstanceOf[RevisitExpectation.Available]).map(_.index)
        if ref.isEmpty then s.expectation == RevisitExpectedSummary.NotRequested
        else if opp.isEmpty then s.expectation == RevisitExpectedSummary.NoOpportunities
        else if missing.nonEmpty then
          s.expectation match
            case e: RevisitExpectedSummary.Incomplete => e.unavailableIndices == missing
            case _                                    => false
        else
          val total = opp
            .map(_.expectation)
            .collect { case e: RevisitExpectation.Available => e.probability }
            .sum
          s.expectation match
            case e: RevisitExpectedSummary.Available =>
              tolerance.approxEquals(e.expectedRevisits, total) &&
              tolerance.approxEquals(e.expectedRate, total / opp.size) &&
              tolerance.approxEquals(e.excessRevisits, s.revisits - total) &&
              tolerance
                .approxEquals(e.excessRate, s.revisits.toDouble / opp.size - total / opp.size)
            case _ => false
      }
    }
  )

  private def eligible[U <: Unit2D](i: Int, c: RevisitConfig[U]): Vector[Int] =
    (0 until i).filter(j => i - j >= c.minimumLag && c.maximumLag.forall(i - j <= _)).toVector

  private def distance[U <: Unit2D](a: Pt[U], b: Pt[U], c: RevisitConfig[U]): Double =
    val dx = math.abs(a.x - b.x)
    val dy = math.abs(a.y - b.y)
    c.distance match
      case FixationGroundDistance.Euclidean => math.sqrt(dx * dx + dy * dy)
      case FixationGroundDistance.Manhattan => dx + dy

  private def same(a: Option[Double], b: Option[Double], tolerance: Tolerance): Boolean =
    (a, b) match
      case (None, None)       => true
      case (Some(x), Some(y)) => tolerance.approxEquals(x, y)
      case _                  => false

object FixationRevisitLaws extends FixationRevisitLaws
