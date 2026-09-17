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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Sample-time AOI accounting, checked against geometry and the supplied support ledger.
  *
  * Generators should include background, all exclusion reasons, overlapping areas
  * (including three-way overlap), and censored temporal support. RejectOverlap
  * belongs here only for inputs where assignment succeeds. Rejection diagnostics
  * are covered by AoiSuite. Integer microseconds are compared exactly using BigInt
  * sums; only dwell proportions use the caller's named tolerance.
  *
  * These laws qualify the shipped static AOI API, not the deferred entity-trace
  * or pairwise-overlap reporting APIs (bead aoi-set).
  */
trait AoiLaws extends Laws:
  def accounting[U <: Unit2D](
      assignments: Gen[AoiAssignment[U]],
      tolerance: Tolerance
  ): RuleSet =
    new SimpleRuleSet(
      "aoi.accounting",
      "one membership per source sample" -> forAll(assignments) { a =>
        a.size == a.recording.size && a.support.size == a.recording.size &&
        a.toVector.zip(a.recording.samples.toVector).forall { (actual, sample) =>
          sample.gaze match
            case Gaze.Blink() =>
              actual == SampleMembership.Excluded(ExclusionReason.Blink)
            case Gaze.Lost() =>
              actual == SampleMembership.Excluded(ExclusionReason.SignalLoss)
            case Gaze.OffScreen(_) =>
              actual == SampleMembership.Excluded(ExclusionReason.OffSurface)
            case Gaze.Tracked(_, _) =>
              val ids = expectedIds(a, sample)
              actual match
                case SampleMembership.Areas(found) => ids.nonEmpty && found == ids
                case SampleMembership.Background   => ids.isEmpty
                case _                             => false
        }
      },
      "represented support is conserved" -> forAll(assignments) { a =>
        val r     = a.report
        val parts =
          Vector(r.aoiUnionTime, r.backgroundTime, r.excludedTime, r.policyCensoredTime)
        parts.forall(!_.isNegative) &&
        parts.map(s => BigInt(s.toMicros)).sum == BigInt(a.support.representedTime.toMicros) &&
        r.policyCensoredTime == a.support.censoredTime
      },
      "union background exclusions and overlap have independent totals" -> forAll(assignments) {
        a =>
          val rows  = weighted(a)
          val union = rows.collect {
            case (sample, dt) if expectedIds(a, sample).nonEmpty => dt
          }.sum
          val background = rows.collect {
            case (sample @ Sample(_, Gaze.Tracked(_, _), _), dt)
                if expectedIds(a, sample).isEmpty =>
              dt
          }.sum
          val excluded = rows.collect {
            case (Sample(_, gaze, _), dt) if !gaze.isInstanceOf[Gaze.Tracked[?]] => dt
          }.sum
          val duplicated = rows.map { (sample, dt) =>
            dt * math.max(0, expectedIds(a, sample).size - 1)
          }.sum
          val r = a.report
          BigInt(r.aoiUnionTime.toMicros) == union &&
          BigInt(r.backgroundTime.toMicros) == background &&
          BigInt(r.excludedTime.toMicros) == excluded &&
          BigInt(r.duplicatedAoiTime.toMicros) == duplicated
      },
      "per-area dwell and policy partition agree" -> forAll(assignments) { a =>
        val measured = a.measure
        val rows     = weighted(a)
        measured.policy == a.policy && measured.report == a.report &&
        measured.areas.map(_.id) == a.aoiSet.ids &&
        measured.areas.forall { metric =>
          val expected = rows.collect {
            case (sample, dt) if expectedIds(a, sample).contains(metric.id) => dt
          }.sum
          BigInt(metric.dwell.toMicros) == expected
        } &&
        measured.areas.map(m => BigInt(m.dwell.toMicros)).sum ==
          BigInt(a.report.aoiUnionTime.toMicros) + BigInt(a.report.duplicatedAoiTime.toMicros)
      },
      "dwell proportions use analysable union plus background" -> forAll(assignments) { a =>
        val denominator = weighted(a).collect { case (Sample(_, Gaze.Tracked(_, _), _), dt) =>
          dt
        }.sum
        a.measure.areas.forall { metric =>
          if denominator == 0 then metric.dwellProportion.isEmpty
          else
            val dwell = weighted(a).collect {
              case (sample, dt) if expectedIds(a, sample).contains(metric.id) => dt
            }.sum
            metric.dwellProportion.exists(p =>
              p.isFinite && tolerance.approxEquals(p, dwell.toDouble / denominator.toDouble)
            )
        }
      }
    )

  private def weighted[U <: Unit2D](a: AoiAssignment[U]): Vector[(Sample[U], BigInt)] =
    a.recording.samples.toVector.zip(a.support.toVector.map(s => BigInt(s.toMicros)))

  private def expectedIds[U <: Unit2D](a: AoiAssignment[U], sample: Sample[U]): Vector[AoiId] =
    sample.gaze match
      case Gaze.Tracked(point, _) =>
        val containing = a.aoiSet.areas.filter(_.region.contains(point))
        a.policy match
          case MembershipPolicy.Multiple => containing.map(_.id)
          case MembershipPolicy.ExclusiveByPriority | MembershipPolicy.RejectOverlap =>
            containing.take(1).map(_.id)
          case MembershipPolicy.SmallestContaining(resolution) =>
            Grid.over(a.aoiSet.frame, resolution.nx, resolution.ny).toOption.toVector.flatMap {
              grid =>
                containing.sortBy(_.region.area(grid)).take(1).map(_.id)
            }
      case _ => Vector.empty

object AoiLaws extends AoiLaws
