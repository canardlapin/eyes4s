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

package eyes4s.studio.core.session

import cats.effect.{IO, Outcome, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{CommandJournal, CommandSamples}
import eyes4s.studio.core.document.DocumentSamples.t2

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

/** Cancelling a session operation at every store mutation it makes (S2.4a
  * review): the store's writes and the session state recording them must
  * stay together, so a later perform never repeats a journal sequence number
  * and a later save never swaps against a stale manifest digest. Run against
  * every [[ProjectStore]].
  */
abstract class SessionCancellationConformance extends SessionConformance:

  private val base  = withoutJobs(t2)
  private val edits = CommandSamples.session

  /** Run `op`, pause its store at mutation `step` (before or after the store
    * performs it), cancel it there, then let the store go on.
    */
  private def cancelledAt[A](faulty: FaultyStore, step: Int, fault: Fault)(
      op: IO[A]
  ): IO[Outcome[IO, Throwable, A]] =
    for
      paused    <- faulty.pause(step, fault)
      fiber     <- op.start
      _         <- paused.reached.get.timeout(10.seconds)
      canceling <- fiber.cancel.start
      // Let the cancellation request reach the fiber before it resumes.
      _       <- IO.sleep(20.millis)
      _       <- paused.gate.complete(())
      _       <- canceling.join
      outcome <- fiber.join
      _       <- faulty.reset
    yield outcome

  private def text(bytes: IArray[Byte]): String = String(Array.from(bytes), UTF_8)

  private val faults = Vector(Fault.Before, Fault.After)

  for fault <- faults do
    test(
      s"a perform cancelled ${fault.toString.toLowerCase} its journal append repeats no sequence number"
    ) {
      bundle.use { views =>
        for
          faulty  <- views.flatMap(FaultyStore.over)
          session <- created(faulty, t2)
          _       <- performAll(session, edits.take(2))
          _       <- cancelledAt(faulty, 1, fault)(session.perform(edits(2)))
          next    <- ok(session.perform(edits(3)))
          live    <- session.document
          journal <- ok(faulty.readSidecar(Sidecar.Journal))
          _       <- ok(session.close)
        yield
          assertEquals(next.journal, JournalWrite.Appended(1))
          val replay = CommandJournal.replay(base, text(journal))
          assertEquals(replay.map(_.entries), Right(edits.take(4)))
          assertEquals(replay.map(_.history.document), Right(live))
      }
    }

  /** The mutations one save of the edited document makes. */
  private val saveSteps: IO[Int] =
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- performAll(session, edits)
        _       <- faulty.reset
        _       <- ok(session.save)
        steps   <- faulty.mutations
      yield steps.size
    }

  for fault <- faults do
    test(
      s"a save cancelled ${fault.toString.toLowerCase} each step leaves no stale manifest digest"
    ) {
      saveSteps.flatMap { n =>
        (1 to n).toVector.traverse_ { k =>
          bundle.use { views =>
            val clue = s"cancel ${fault.toString.toLowerCase} step $k of $n"
            for
              faulty  <- views.flatMap(FaultyStore.over)
              session <- created(faulty, t2)
              _       <- performAll(session, edits)
              after   <- session.document
              _       <- cancelledAt(faulty, k, fault)(session.save)
              again   <- session.save
              _       <- ok(session.close)
              opened  <- reopen(views)
              doc     <- opened.session.document
              _       <- ok(opened.session.close)
            yield
              // The cancelled save completed and was recorded: the next one
              // finds the manifest it expects and has nothing to swap.
              assertEquals(again.map(_.swapped), Right(false), clue)
              assertEquals(opened.report.journal, JournalFinding.Clean, clue)
              assertEquals(doc, after, clue)
          }
        }
      }
    }

  for
    fault <- faults
    step  <- Vector(1, 2)
  do
    test(
      s"a decline cancelled ${fault.toString.toLowerCase} archive step $step still answers the recovery"
    ) {
      bundle.use { views =>
        for
          faulty  <- views.flatMap(FaultyStore.over)
          session <- created(faulty, t2)
          _       <- performAll(session, edits.take(2))
          _       <- die(faulty)
          other   <- views.flatMap(FaultyStore.over)
          opened  <- ok(ProjectSession.open(other, bob))
          digest  <- opened.report.journal match
            case JournalFinding.Pending(r) => IO.pure(r.journal)
            case j                         => IO.raiseError(AssertionError(j.toString))
          _       <- cancelledAt(other, step, fault)(opened.session.decline(digest))
          pending <- opened.session.pending
          edit    <- opened.session.perform(edits.head)
          _       <- ok(opened.session.close)
          again   <- reopen(views)
          _       <- ok(again.session.close)
        yield
          assertEquals(pending, None)
          assert(edit.isRight, edit)
          // The declined journal is archived, and the edit after it journaled.
          again.report.journal match
            case JournalFinding.Pending(Recovery.Offered(offer)) =>
              assertEquals(offer.restores.map(_.entry), Vector(edits.head))
            case j => fail(s"unexpected journal $j")
      }
    }

/** The in-memory store passes the cancellation suite. */
class SessionCancellationSuite extends SessionCancellationConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)
