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
import eyes4s.kernel.Unit2D.Px

/** Consumes an independently generated exact oracle, without running Python or R. */
class FixationAoiConformanceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame               = get(Frame.screen("cr7-fixation-oracle", 100, 100))
  private val clock               = ClockId("cr7-fixation-oracle")
  private val proportionTolerance = Tolerance(absolute = 1e-15, relative = 1e-14)

  FixationAoiReference.cases.foreach { c =>
    test(c.name) {
      val areas = get(
        AoiSet.of(
          c.areas.map(a =>
            get(
              Aoi.of(
                a.id,
                a.label,
                frame,
                get(Region.rect(Pt[Px](a.xMin, a.yMin), Pt[Px](a.xMax, a.yMax)))
              )
            )
          )
        )
      )
      val path = get(
        Scanpath.of(
          frame,
          clock,
          IArray.from(
            c.fixes.map(f =>
              get(
                Event.Fixation.withoutDispersion(
                  get(
                    Interval
                      .of(clock, Instant.micros(f.onset), Instant.micros(f.onset + f.duration))
                  ),
                  Pt[Px](f.x, f.y),
                  1
                )
              )
            )
          )
        )
      )
      val policy = c.policy match
        case "multiple"  => MembershipPolicy.Multiple
        case "exclusive" => MembershipPolicy.ExclusiveByPriority
        case "smallest"  =>
          MembershipPolicy.SmallestContaining(get(AoiResolution.of(c.nx, c.ny)))
        case "reject" => MembershipPolicy.RejectOverlap
        case other    => fail(s"unknown fixture policy $other")
      val result = areas.assign(path, policy)
      c.expected match
        case None =>
          result match
            case Left(AoiError.ObservedFixationOverlap(id, index, x, y, ids)) =>
              assertEquals(id, frame.id)
              assertEquals(Some(index), c.overlapIndex)
              assertEquals((x, y), (c.fixes(index).x, c.fixes(index).y))
              assertEquals(ids.map(_.value), c.overlapAreas)
            case other => fail(s"unexpected result $other")
        case Some(expected) =>
          val assignment = get(result)
          assertEquals(assignment.size, c.fixes.size)
          assertEquals(
            assignment.toVector.map {
              case FixationMembership.Areas(ids) => "areas"      -> ids.map(_.value)
              case FixationMembership.Background => "background" -> Vector.empty[String]
              case FixationMembership.OffSurface => "offSurface" -> Vector.empty[String]
            },
            expected.memberships
          )
          val report = assignment.report
          assertEquals(
            Vector(
              report.aoiUnionTime,
              report.backgroundTime,
              report.offSurfaceTime,
              report.interFixationTime,
              report.duplicatedAoiTime
            ).map(_.toMicros),
            expected.report
          )
          assert(assignment.accountingHolds)
          val measurements = assignment.measure
          assertEquals(measurements.areas.size, expected.metrics.size)
          measurements.areas.zip(expected.metrics).foreach { (actual, wanted) =>
            assertEquals(actual.id.value, wanted.id)
            assertEquals(actual.dwell.toMicros, wanted.dwell)
            assertEquals(actual.firstEntryLatency.map(_.toMicros), wanted.firstEntry)
            assertEquals(actual.fixationCount, wanted.fixationCount)
            assertEquals(actual.entryCount, wanted.entryCount)
            assertEquals(actual.visitCount, wanted.visitCount)
            assertEquals(actual.dwellProportion.isDefined, wanted.proportion.isDefined)
            actual.dwellProportion.zip(wanted.proportion).foreach { (a, b) =>
              assert(
                proportionTolerance.approxEquals(a, b),
                s"${wanted.id}: $a versus rational oracle $b"
              )
            }
          }
          assertEquals(
            measurements.transitions.map(t => (t.from.value, t.to.value, t.count)),
            expected.transitions
          )
    }
  }
