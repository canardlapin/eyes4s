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
import eyes4s.kernel.*

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Fixation-time AOI accounting and visits against independently reclassified source
  * geometry. Generators must include positive gaps, background/off-surface fixations,
  * overlapping areas and all-contained paths. RejectOverlap inputs must succeed.
  * Durations and counts are exact; only proportions use the caller's named tolerance.
  */
trait FixationAoiLaws extends Laws:
  def accounting[U <: Unit2D](
      assignments: Gen[FixationAoiAssignment[U]],
      tolerance: Tolerance
  ): RuleSet =
    new DefaultRuleSet(
      "fixationAoiAccounting",
      None,
      "cardinality and centre membership agree with geometry" -> forAll(assignments) { a =>
        a.size == a.scanpath.n && a.toVector.zipWithIndex.forall { (membership, index) =>
          val point = a.scanpath.fixations(index).centre
          membership match
            case FixationMembership.OffSurface => !a.aoiSet.frame.contains(point)
            case FixationMembership.Background =>
              a.aoiSet.frame.contains(point) && expectedIds(a, index).isEmpty
            case FixationMembership.Areas(ids) =>
              ids.nonEmpty && ids == expectedIds(a, index)
        }
      },
      "fixation time and inter-fixation time partition the extent" -> forAll(assignments) { a =>
        val fixations  = a.scanpath.fixations.toVector
        val durations  = fixations.map(f => BigInt(f.duration.toMicros))
        val ids        = fixations.indices.map(expectedIds(a, _)).toVector
        val union      = durations.zip(ids).collect { case (d, hit) if hit.nonEmpty => d }.sum
        val background = durations.zipWithIndex.collect {
          case (d, i) if a.aoiSet.frame.contains(fixations(i).centre) && ids(i).isEmpty => d
        }.sum
        val offSurface = durations.zipWithIndex.collect {
          case (d, i) if !a.aoiSet.frame.contains(fixations(i).centre) => d
        }.sum
        val duplicated = durations.zip(ids).map((d, hit) => d * math.max(0, hit.size - 1)).sum
        val extent     = BigInt(a.scanpath.last.span.offset.toMicros) -
          BigInt(a.scanpath.first.span.onset.toMicros)
        val report = a.report
        a.accountingHolds &&
        BigInt(report.aoiUnionTime.toMicros) == union &&
        BigInt(report.backgroundTime.toMicros) == background &&
        BigInt(report.offSurfaceTime.toMicros) == offSurface &&
        BigInt(report.duplicatedAoiTime.toMicros) == duplicated &&
        BigInt(report.fixationTime.toMicros) == durations.sum &&
        BigInt(report.interFixationTime.toMicros) == extent - durations.sum &&
        BigInt(report.accountedTime.toMicros) == extent
      },
      "dwell and fixation counts exclude non-fixation time" -> forAll(assignments) { a =>
        val metrics = a.measure
        metrics.policy == a.policy && (metrics.report eq a.report) &&
        metrics.areas.map(_.id) == a.aoiSet.ids && metrics.areas.forall { metric =>
          val contained =
            a.scanpath.fixations.indices.filter(i => expectedIds(a, i).contains(metric.id))
          val dwell = contained.map(i => BigInt(a.scanpath.fixations(i).duration.toMicros)).sum
          BigInt(metric.dwell.toMicros) == dwell && metric.fixationCount == contained.size
        } && metrics.areas.map(m => BigInt(m.dwell.toMicros)).sum ==
          BigInt(a.report.aoiUnionTime.toMicros) + BigInt(a.report.duplicatedAoiTime.toMicros)
      },
      "visits follow consecutive fixation membership regardless of gaps" -> forAll(
        assignments
      ) { a =>
        a.measure.areas.forall { metric =>
          val present =
            a.scanpath.fixations.indices.map(i => expectedIds(a, i).contains(metric.id))
          val starts = present.indices.count(i => present(i) && (i == 0 || !present(i - 1)))
          metric.visitCount == starts && metric.entryCount == starts &&
          metric.visitCount <= metric.fixationCount &&
          (!present.forall(identity) || metric.visitCount == 1)
        }
      },
      "first entry follows the first contained fixation onset" -> forAll(assignments) { a =>
        a.measure.areas.forall { metric =>
          val first =
            a.scanpath.fixations.indices.find(i => expectedIds(a, i).contains(metric.id))
          metric.firstEntryLatency == first.map(i =>
            a.scanpath.first.span.onset.until(a.scanpath.fixations(i).span.onset)
          )
        }
      },
      "proportions use on-surface fixation time including background" -> forAll(assignments) {
        a =>
          val denominator = a.scanpath.fixations.toVector.collect {
            case f if a.aoiSet.frame.contains(f.centre) => BigInt(f.duration.toMicros)
          }.sum
          a.measure.areas.forall { metric =>
            if denominator == 0 then metric.dwellProportion.isEmpty
            else
              val dwell = a.scanpath.fixations.indices.collect {
                case i if expectedIds(a, i).contains(metric.id) =>
                  BigInt(a.scanpath.fixations(i).duration.toMicros)
              }.sum
              metric.dwellProportion.exists(p =>
                p.isFinite && tolerance.approxEquals(p, dwell.toDouble / denominator.toDouble)
              )
          }
      },
      "transitions follow adjacent fixations and preserve ordered pairs" -> forAll(
        assignments
      ) { a =>
        val ids   = a.scanpath.fixations.indices.map(expectedIds(a, _)).toVector
        val edges = ids.zip(ids.drop(1)).flatMap { (from, to) =>
          if from == to then Vector.empty
          else for left <- from; right <- to if left != right yield (left, right)
        }
        val expected = a.aoiSet.ids.flatMap { from =>
          a.aoiSet.ids.collect {
            case to if from != to && edges.count(_ == (from, to)) > 0 =>
              (from, to, edges.count(_ == (from, to)))
          }
        }
        a.measure.transitions.map(t => (t.from, t.to, t.count)) == expected
      }
    )

  private def expectedIds[U <: Unit2D](a: FixationAoiAssignment[U], index: Int): Vector[AoiId] =
    val point = a.scanpath.fixations(index).centre
    if !a.aoiSet.frame.contains(point) then Vector.empty
    else
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

object FixationAoiLaws extends FixationAoiLaws
