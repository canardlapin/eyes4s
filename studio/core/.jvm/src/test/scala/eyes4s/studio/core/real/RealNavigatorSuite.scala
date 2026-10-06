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

package eyes4s.studio.core.real

import cats.effect.IO
import eyes4s.results.{ReportNavigation, Role}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingId, ReportingSpec, ReportingWeight}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.headless.NativeReads
import eyes4s.studio.core.navigation.{ReportRef, UsedByRole}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class RealNavigatorSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private val scale                             = get(ScaleIndex.of(2))
  private val page                              = get(PageRequest.of(0, 100))
  private val spec                              = get(
    ReportingSpec.of(
      get(ReportingId.of("native-navigation-overall")),
      "Native navigation",
      None,
      Vector.empty,
      None,
      ReportingWeight.ParticipantMeans
    )
  )

  test("native report membership and the full source chain agree with library navigation") {
    RealStudyBackend
      .resource[IO](document, RealBackendConformanceSuite.golden)
      .use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          for
            _    <- reads.result(StoryMoments.run7).map(get)
            _    <- reads.report(StoryMoments.run7, spec, scale.value).map(get)
            held <- backend.held(StoryMoments.run7).map(get)
            native = get(RealReports.native(StoryMoments.run7, spec, scale.value, held))
            _     <- backend.navigator.synchronizeReporting(document.reporting)
            _     <- backend.navigator.synchronizeReporting(document.reporting)
            cells <- reads.navigator.cells(StoryMoments.run7, spec.id, scale, page).map(get)
            cell = cells.entries.headOption.getOrElse(fail("no cell"))
            participants <- reads.navigator.participants(cell, page).map(get)
            first = participants.entries.headOption.getOrElse(fail("no participant"))
            queries <- reads.navigator.queries(first, page).map(get)
            query = queries.entries.headOption.getOrElse(fail("no query"))
            pairs <- reads.navigator.pairs(query, PairDesign.Matched, page).map(get)
            pair = pairs.entries.headOption.getOrElse(fail("no pair"))
            maps      <- reads.navigator.maps(pair).map(get)
            fixations <- reads.navigator.fixations(maps.query, page).map(get)
            fixation = fixations.entries.headOption.getOrElse(fail("no fixation"))
            record     <- reads.navigator.record(fixation).map(get)
            used       <- reads.navigator.usedByCounts(maps.query).map(get)
            usingPairs <- reads.navigator.usedBy(maps.query, UsedByRole.AsQuery, page).map(get)
            matchedRoleCell = ReportRef.Cell(
              cell.run,
              cell.reporting,
              cell.scale,
              cell.group,
              ReportRole.Matched
            )
            matchedParticipants <- reads.navigator.participants(matchedRoleCell, page).map(get)
          yield
            val nativeCell = ReportNavigation
              .cells(native)
              .find(_.role == Role.Difference)
              .getOrElse(fail("no native D cell"))
            val nativeParticipants = get(ReportNavigation.participants(native, nativeCell))
            assertEquals(
              participants.entries.map(_.participant),
              nativeParticipants.map(_.participant)
            )
            val nativeQueries = get(
              ReportNavigation
                .queries(native, nativeParticipants.head, held.prepared.plan.layout)
            )
            val expected = nativeQueries.collect {
              case eyes4s.plan.ResultRef.ContrastRow(_, key) =>
                StudioRef.QueryContrast(StoryMoments.run7, scale, RealResults.key(key))
            }
            assertEquals(queries.entries, expected)
            val nativeMatched = ReportNavigation
              .cells(native)
              .find(_.role == Role.Matched)
              .getOrElse(fail("no M cell"))
            assertEquals(
              matchedParticipants.entries.map(_.participant),
              get(ReportNavigation.participants(native, nativeMatched)).map(_.participant)
            )
            assert(matchedParticipants.entries.forall(_.cell.role == ReportRole.Matched))
            assert(usingPairs.entries.contains(pair))
            assert(used.asQuery > 0)
            record match
              case StudioRef.SourceRecord(trial, Some(index), _, number) =>
                val core = held.prepared.admitted.input.trials.rows
                  .find(row => RealResults.key(row.key) == trial)
                  .getOrElse(fail("no source trial"))
                  .key
                val provenance = get(
                  eyes4s.plan.CoordinateProvenance.of(
                    held.prepared.plan,
                    held.prepared.admitted.input,
                    Some(held.prepared.admitted.evidence)
                  )
                )
                val source = get(
                  eyes4s.plan.ResultNavigation.record(
                    provenance,
                    eyes4s.plan
                      .FixationRef(core, get(eyes4s.plan.ScanpathPosition.of(index.value - 1)))
                  )
                )
                assertEquals(number.value, source.value)
              case other => fail(s"unexpected source $other")
        }
      }
      .timeout(40.seconds)
  }
