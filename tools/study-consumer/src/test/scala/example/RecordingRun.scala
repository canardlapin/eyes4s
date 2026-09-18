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

package example

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.fs2.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.laws.RecordingResultEquivalence
import eyes4s.plan.*

/** One recording route through the journey, as an application runs it: the
  * plan built from the input's evidence, its runner and the steps the suites
  * and the fresh reader share. A refusal here is a broken fixture.
  */
final class RecordingCase[P](val route: RecordingRoute[P]):
  import JourneySetup.{get, utf8}
  import RecordingFixtures.*

  type Outcome = RecordingOutcome[P]

  val runner = RecordingExecution[IO]

  lazy val plan: RecordingPlan[P] =
    get(route.plan(input, angular, gap, areas))

  /** The pure run the streamed and reloaded runs must reproduce. */
  lazy val pure: RecordingAnalysis[P] = get(plan.run(recording))

  lazy val id: RecordingRunId = RecordingRunId.of(plan, quanta)

  /** Bit-for-bit identity of two analyses of this route. */
  def same(a: RecordingAnalysis[P], b: RecordingAnalysis[P]): Boolean =
    RecordingJourney.fingerprint(a) == RecordingJourney.fingerprint(b) &&
      RecordingResultEquivalence.same(a, b)

  def completed(outcome: Outcome): RecordingAnalysis[P] = outcome match
    case RunOutcome.Completed(_, _, result) => result
    case other => throw new IllegalStateException(s"expected a completed run: $other")

  def events: IO[Vector[RecordingEvent[P]]] =
    runner.events(plan, recording, quanta).compile.toVector

  /** Start a run, park it before step `completed + 1` and cancel it there. */
  def cancelAfter(completed: Int): IO[(Outcome, Vector[RecordingProgress])] =
    for
      started <- Ref[IO].of(0)
      reached <- Deferred[IO, Unit]
      parked  <- Deferred[IO, Unit]
      between = started.updateAndGet(_ + 1).flatMap { n =>
        if n == completed + 1 then reached.complete(()) >> parked.get else IO.unit
      }
      result <- runner.start(plan, recording, quanta, between).use { run =>
        reached.get >> run.cancel >> (run.outcome, run.progress.compile.toVector).tupled
      }
    yield result

  /** A completed streamed run and its saved manifest. */
  def saved: IO[(RecordingAnalysis[P], SavedManifest)] =
    runner.start(plan, recording, quanta).use(_.outcome).map { outcome =>
      val analysis = completed(outcome)
      analysis -> get(RecordingJourney.save(route, input, plan, analysis))
    }

  /** The application's storage: a private copy of every file, by name. */
  def storage(saved: SavedManifest): Map[String, IArray[Byte]] =
    SavedRun
      .files(saved)
      .map((name, bytes) => name -> IArray.tabulate(bytes.length)(bytes(_)))
      .toMap

  /** A reader that starts from the stored bytes and fresh registrations. */
  def reload(
      files: Map[String, IArray[Byte]]
  ): Either[JourneyError, RecordingJourney.Reloaded[P]] =
    val address = get(ByteDigest.parse(utf8(files(SavedRun.addressFile))))
    RecordingJourney.resolve(route, address, SavedRun.source(files.get))

  /** The canonical archive digest of an analysis under this route. */
  def archived(analysis: RecordingAnalysis[P]): ByteDigest =
    get(StoredArtifact.recordingResult("rerun", route.results, analysis)).entry.sha256

  /** The oracle's view of an analysis: kind, onset and exclusive offset in
    * microseconds, and the places in degrees.
    */
  def detected(analysis: RecordingAnalysis[P]): Vector[RecordingFixtures.Expected] =
    analysis.detection.eventSeries.events.map {
      case f: Event.Fixation[Deg] =>
        Expected.Fixation(f.span.onset.toMicros, f.span.offset.toMicros, f.centre.x, f.centre.y)
      case s: Event.Saccade[Deg] =>
        Expected.Saccade(s.span.onset.toMicros, s.span.offset.toMicros, s.from.x, s.to.x)
      case other => throw new IllegalStateException(s"unexpected event $other")
    }

  /** Whether an analysis matches the oracle: kinds and spans exactly, places
    * within `tolerance` degrees (the angular warp is trigonometric).
    */
  def matchesOracle(analysis: RecordingAnalysis[P], tolerance: Double): Boolean =
    val found = detected(analysis)
    found.size == expected.size && found.zip(expected).forall {
      case (Expected.Fixation(a, b, x, y), Expected.Fixation(c, d, u, v)) =>
        a == c && b == d && math.abs(x - u) <= tolerance && math.abs(y - v) <= tolerance
      case (Expected.Saccade(a, b, x, y), Expected.Saccade(c, d, u, v)) =>
        a == c && b == d && math.abs(x - u) <= tolerance && math.abs(y - v) <= tolerance
      case _ => false
    } && analysis.detection.eventSeries.support.map(r => (r.from, r.until)) == support

/** The consumer's two recording routes through the journey. */
object RecordingRun:
  val ivt: RecordingCase[IvtParameters]    = RecordingCase(RecordingFixtures.ivt)
  val lab: RecordingCase[LabIvtParameters] = RecordingCase(RecordingFixtures.lab)
