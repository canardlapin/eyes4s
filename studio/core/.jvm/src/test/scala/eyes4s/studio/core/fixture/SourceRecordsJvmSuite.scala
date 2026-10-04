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

package eyes4s.studio.core.fixture

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

/** The protocol 1.7 source records (S6.4) checked against fixtures/studio-golden's
  * own text (`GoldenCsv`, JVM test scope): every one of fixations.csv's 11,520
  * records is served, in file order, with its verbatim line and its cells, and
  * every record of an admitted scanpath names the fixation, and carries the
  * placement, that the trial view (protocol 1.6) gives it.
  */
class SourceRecordsJvmSuite extends CatsEffectSuite:

  override val munitIOTimeout: Duration = 120.seconds

  private val rev4 = AnalysisRevision(4)

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  private val lines: Vector[String] = GoldenCsv.fixations.linesIterator.toVector.drop(1)

  private def all(b: FakeStudyBackend[IO]): IO[Vector[SourceRecordRow]] =
    (1 to lines.size by SourceRecordPage.Limit).toVector
      .traverse(from =>
        b.sourceRecords(rev4, from, SourceRecordPage.Limit)
          .map(_.fold(e => fail(e.message), _.rows))
      )
      .map(_.flatten)

  test("every record of fixations.csv, in order, verbatim and as its cells state") {
    fake.flatMap(all).map { rows =>
      assertEquals(lines.size, 11520)
      assertEquals(rows.map(_.record), (1 to 11520).toVector)
      assertEquals(rows.map(_.line), lines)
      val cells = lines.map(_.split(",", -1))
      assertEquals(
        rows.map(r =>
          (
            r.trial.participant,
            r.trial.phase.label,
            r.trial.trial,
            r.trial.occurrence.toString,
            r.ordinal.map(_.toString),
            r.screen.map(p => (p.x, p.y)),
            r.onsetMs,
            r.durationMs,
            r.samples
          )
        ),
        cells.map(c =>
          (
            c(0),
            c(1),
            c(2),
            c(3),
            Some(c(4)),
            Some((c(5).toDouble, c(6).toDouble)),
            Some(c(7).toDouble),
            Some(c(8).toDouble),
            Some(c(9).toInt)
          )
        )
      )
      // The image frame is fixtures/studio-golden's (448, 156), 1024 × 768.
      assertEquals(
        rows.map(_.image.map(i => (i.at.x, i.at.y, i.insideImage))),
        cells.map { c =>
          val (x, y) = (c(5).toDouble, c(6).toDouble)
          Some((x - 448.0, y - 156.0, x >= 448 && x < 1472 && y >= 156 && y < 924))
        }
      )
    }
  }

  test("each admitted record names its fixation and the trial view's placement") {
    for
      b    <- fake
      rows <- all(b)
      placed = rows.filter(_.fixation.isDefined)
      views <- placed
        .map(_.trial)
        .distinct
        .traverse(t => b.trialFixations(rev4, t).map(_.fold(e => fail(e.message), identity)))
    yield
      val byRecord = views.flatMap(_.fixations).map(f => f.record -> f).toMap
      assertEquals(placed.size, byRecord.size)
      placed.foreach { r =>
        val f = byRecord.getOrElse(r.record, fail(s"record ${r.record} is in no trial view"))
        assertEquals((r.fixation, r.placement), (Some(f.ref), Some(f.placement)))
      }
      // A record no trial view holds names no fixation.
      assertEquals(rows.size - placed.size, rows.count(_.placement.isEmpty))
  }
