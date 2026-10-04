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

package eyes4s.studio.core.preview

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.execution.ExecutionService
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment}
import munit.CatsEffectSuite
import fs2.Stream

class PreviewPagingSuite extends CatsEffectSuite:
  private def budget(n: Int): PreviewBudget =
    PreviewBudget.of(n).fold(e => fail(e.message), identity)

  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private def progress(completed: Int, total: Int): PreviewProgress =
    right(PreviewProgress.of(completed, total))

  private def ok[E, A](value: IO[Either[E, A]]): IO[A] =
    value.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.toString)), IO.pure))

  private def events(
      stream: fs2.Stream[IO, Either[BackendError, PreviewEvent]]
  ): IO[Vector[PreviewEvent]] =
    stream
      .evalMap(_.fold(e => IO.raiseError(new AssertionError(e.message)), IO.pure))
      .compile
      .toVector

  test("fixture preview pages one participant at a time and reports the golden counts") {
    for
      fake  <- FakeStudyBackend.create[IO](StoryMoment.T2)
      first <- events(fake.previewCounting(AnalysisRevision(5), budget(1)))
      id = first
        .collectFirst { case PreviewEvent.Initial(value, _, _) => value }
        .getOrElse(fail("no initial"))
      rest <- events(fake.continuePreview(id, budget(23)))
    yield
      assertEquals(first.size, 2)
      assertEquals(
        first.collect { case PreviewEvent.Counting(_, p) => p },
        Vector(progress(1, 24))
      )
      val ready = rest
        .collectFirst { case PreviewEvent.Ready(value) => value }
        .getOrElse(fail("not ready"))
      assertEquals(ready.candidates.candidatePairsPerScale, 230400L)
      assertEquals(ready.counts, right(PreviewCounts.of(8969L, 44845L, 457, 9, 0)))
      assertEquals(
        rest.collect { case PreviewEvent.Counting(_, p) => p }.last,
        progress(24, 24)
      )
  }

  test("a partly counted preview cannot submit, and a changed ready receipt is refused") {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      a    <- events(fake.previewCounting(AnalysisRevision(5), budget(1)))
      aId = a
        .collectFirst { case PreviewEvent.Initial(value, _, _) => value }
        .getOrElse(fail("no initial"))
      b <- events(fake.previewCounting(AnalysisRevision(5), budget(24)))
      receipt = b
        .collectFirst { case PreviewEvent.Ready(value) => value }
        .getOrElse(fail("no ready"))
      incomplete <- fake.submitPreview(receipt.copy(id = aId))
      tampered   <- fake.submitPreview(
        receipt.copy(counts =
          right(
            PreviewCounts.of(
              receipt.counts.eligiblePairsPerScale,
              receipt.counts.eligiblePairs,
              receipt.counts.eligibleQueries,
              receipt.counts.unmatchedQueries,
              1
            )
          )
        )
      )
    yield
      assertEquals(incomplete.left.map(_.code), Left("studio-backend.preview-not-ready"))
      assertEquals(tampered.left.map(_.code), Left("studio-backend.tampered-preview"))
  }

  test("only the exact ready handle can start its retained snapshot") {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      page <- events(fake.previewCounting(AnalysisRevision(5), budget(24)))
      ready = page
        .collectFirst { case PreviewEvent.Ready(value) => value }
        .getOrElse(fail("not ready"))
      tampered = ready.copy(counts =
        right(
          PreviewCounts.of(
            1L,
            ready.counts.eligiblePairs,
            ready.counts.eligibleQueries,
            ready.counts.unmatchedQueries,
            ready.counts.ambiguousMatches
          )
        )
      )
      refused <- fake.submitPreview(tampered)
      status  <- ok(fake.submitPreview(ready))
    yield
      assertEquals(refused.left.map(_.code), Left("studio-backend.tampered-preview"))
      assertEquals(
        (status.revision, status.dataset),
        (ready.stamp.revision, ready.stamp.dataset)
      )
  }

  test("preview frames and requests round-trip through the text transport") {
    val request = Envelope(
      RequestId(77L),
      BackendRequest.PreviewCounting(AnalysisRevision(5), budget(1))
    )
    val frame = Envelope(
      RequestId(77L),
      ServerFrame.Preview(
        PreviewEvent.Counting(PreviewId(4L), progress(1, 24))
      )
    )
    assertEquals(WireFormat.parse[BackendRequest](WireFormat.line(request)), Right(request))
    assertEquals(WireFormat.parse[ServerFrame](WireFormat.line(frame)), Right(frame))
  }

  test("unknown revisions and handles are stream refusals") {
    for
      fake     <- FakeStudyBackend.create[IO](StoryMoment.T2)
      revision <- fake.previewCounting(AnalysisRevision(99), budget(1)).compile.toVector
      handle   <- fake.continuePreview(PreviewId(99L), budget(1)).compile.toVector
    yield
      assertEquals(
        revision.map(_.left.map(_.code)),
        Vector(Left("studio-backend.unknown-revision"))
      )
      assertEquals(
        handle.map(_.left.map(_.code)),
        Vector(Left("studio-backend.unknown-preview"))
      )
  }

  test("preview budget is a positive bounded work request") {
    assertEquals(
      PreviewBudget.of(0).left.map(_.message),
      Left("Preview budget 0 is outside 1 to 4096 participants.")
    )
    assertEquals(
      PreviewBudget.of(4097).left.map(_.message),
      Left("Preview budget 4097 is outside 1 to 4096 participants.")
    )
  }

  test("in-process and loopback transports resume bounded preview pages") {
    def exercise(transport: FakeStudyBackend[IO] => BackendTransport[IO]): IO[Unit] =
      for
        fake   <- FakeStudyBackend.create[IO](StoryMoment.T2)
        remote <- RemoteStudyBackend[IO](transport(fake))
        opened <- remote
          .previewCounting(AnalysisRevision(5), budget(1))
          .take(2)
          .compile
          .toVector
        id = opened
          .collectFirst { case Right(PreviewEvent.Initial(value, _, _)) => value }
          .getOrElse(fail("no initial"))
        completed <- events(remote.continuePreview(id, budget(23)))
      yield assert(completed.exists(_.isInstanceOf[PreviewEvent.Ready]), completed)
    exercise(BackendTransport.inProcess(_)) >>
      exercise(fake => BackendTransport.loopback(SidecarServer.serve(fake)))
  }

  test("remote preview closes its exchange on early stop, refusal, and completion") {
    for
      fake     <- FakeStudyBackend.create[IO](StoryMoment.T2)
      acquired <- Ref.of[IO, Int](0)
      released <- Ref.of[IO, Int](0)
      inner                           = BackendTransport.inProcess(fake)
      transport: BackendTransport[IO] = request =>
        Stream
          .bracket(acquired.update(_ + 1))(_ => released.update(_ + 1))
          .flatMap(_ => inner.exchange(request))
      remote <- RemoteStudyBackend[IO](transport)
      _      <- remote.previewCounting(AnalysisRevision(5), budget(1)).take(1).compile.drain
      _      <- remote.previewCounting(AnalysisRevision(99), budget(1)).compile.drain
      _      <- remote.previewCounting(AnalysisRevision(5), budget(24)).compile.drain
      opened <- acquired.get
      closed <- released.get
    yield assertEquals((opened, closed), (3, 3))
  }

  test("cancelling a waiting remote preview finalizes its exchange") {
    for
      entered  <- Deferred[IO, Unit]
      released <- Ref.of[IO, Int](0)
      transport: BackendTransport[IO] = request =>
        Stream.bracket(IO.unit)(_ => released.update(_ + 1)).flatMap { _ =>
          Stream.emit(
            Envelope(request.id, ServerFrame.Response(BackendResponse.PreviewAccepted))
          ) ++
            Stream.exec(entered.complete(()).void) ++ Stream.never[IO]
        }
      remote <- RemoteStudyBackend[IO](transport)
      fiber  <- remote.previewCounting(AnalysisRevision(5), budget(1)).compile.drain.start
      _      <- entered.get >> fiber.cancel
      closed <- released.get
    yield assertEquals(closed, 1)
  }

  test("a remote preview requires an acceptance or refusal before event frames") {
    Vector(
      Vector.empty,
      Vector(ServerFrame.Response(BackendResponse.PreviewAccepted)),
      Vector(ServerFrame.Preview(PreviewEvent.Counting(PreviewId(1), progress(1, 24))))
    ).traverse_ { frames =>
      val transport: BackendTransport[IO] =
        request => Stream.emits(frames.map(Envelope(request.id, _)))
      for
        remote <- RemoteStudyBackend[IO](transport)
        result <- remote.previewCounting(AnalysisRevision(5), budget(1)).compile.drain.attempt
      yield assert(result.left.exists(_.isInstanceOf[TransportFailure]), result)
    }
  }

  test("ExecutionService forwards the exact ready receipt to the retained snapshot") {
    (for
      fake    <- cats.effect.Resource.eval(FakeStudyBackend.create[IO](StoryMoment.T2))
      service <- ExecutionService.resource[IO](fake)
    yield (fake, service)).use { (fake, service) =>
      for
        page <- events(fake.previewCounting(AnalysisRevision(5), budget(24)))
        ready = page
          .collectFirst { case PreviewEvent.Ready(value) => value }
          .getOrElse(fail("no ready"))
        job <- service
          .submitPreview(ready)
          .flatMap(_.fold(e => IO.raiseError(new AssertionError(e.message)), IO.pure))
        outcome <- ok(fake.complete(job.id))
      yield assertEquals(
        outcome.progress.map(_.totals.totalPairs),
        Some(ProgressTotal.Exact(44845L))
      )
    }
  }

  test("preview counts refuse negatives and over-complete progress, naming the operand") {
    assertEquals(
      PreviewProgress.of(25, 24),
      Left(PreviewError.BeyondTotal("completedParticipants", 25, 24))
    )
    assertEquals(
      PreviewProgress.of(-1, 24),
      Left(PreviewError.Negative("completedParticipants", -1))
    )
    assertEquals(
      PreviewProgress.of(0, -1),
      Left(PreviewError.Negative("totalParticipants", -1))
    )
    assertEquals(
      PreviewCandidates.of(480, 480, 24, -1L, 480, 14, None),
      Left(PreviewError.Negative("candidatePairsPerScale", -1L))
    )
    assertEquals(
      PreviewCandidates.of(480, -2, 24, 1L, 480, 14, None),
      Left(PreviewError.Negative("referenceTrials", -2L))
    )
    assertEquals(
      PreviewCounts.of(8969L, 44845L, 457, 9, -1),
      Left(PreviewError.Negative("ambiguousMatches", -1L))
    )
    assertEquals(
      PreviewCounts.of(-8969L, 44845L, 457, 9, 0),
      Left(PreviewError.Negative("eligiblePairsPerScale", -8969L))
    )
    assertEquals(
      PreviewCandidates.of(480, 480, 24, 1L, 480, 481, None),
      Left(PreviewError.BeyondTotal("queriesNotAdmitted", 481, 480))
    )
    assertEquals(
      PreviewCandidates.of(480, 480, 24, 1L, 480, 14, Some(-1)),
      Left(PreviewError.Negative("byDesignQueries", -1L))
    )
    assertEquals(
      PreviewCounts.of(8969L, 44845L, -457, 9, 0),
      Left(PreviewError.Negative("eligibleQueries", -457L))
    )
    assertEquals(ParticipantCount.of(-3), Left(PreviewError.Negative("participant count", -3L)))
    assertEquals(
      PreviewError.BeyondTotal("completedParticipants", 25, 24).message,
      "Preview completedParticipants reports 25, beyond its total 24."
    )
  }

  test("preview decoders refuse what the constructors refuse") {
    import io.circe.parser.decode
    assert(
      decode[PreviewProgress]("""{"completedParticipants":25,"totalParticipants":24}""").isLeft
    )
    assert(
      decode[PreviewProgress]("""{"completedParticipants":-1,"totalParticipants":24}""").isLeft
    )
    assert(
      decode[PreviewCandidates](
        """{"focalTrials":-1,"referenceTrials":480,"participants":24,"candidatePairsPerScale":1,""" +
          """"requestedQueries":480,"queriesNotAdmitted":14,"byDesignQueries":null}"""
      ).isLeft
    )
    assert(
      decode[PreviewCounts](
        """{"eligiblePairsPerScale":1,"eligiblePairs":1,"eligibleQueries":1,""" +
          """"unmatchedQueries":-9,"ambiguousMatches":0}"""
      ).isLeft
    )
    assert(
      decode[BackendError](
        """{"PreviewNotReady":{"preview":1,"completedParticipants":-3,"totalParticipants":24}}"""
      ).isLeft
    )
    assertEquals(
      decode[PreviewProgress]("""{"completedParticipants":3,"totalParticipants":24}"""),
      Right(progress(3, 24))
    )
  }

  test("progress advances one participant at a time and stops when complete") {
    val start =
      PreviewProgress.start(right(PreviewCandidates.of(480, 480, 2, 1L, 480, 14, None)))
    assertEquals(start, progress(0, 2))
    assertEquals(start.advance, Some(progress(1, 2)))
    assertEquals(start.advance.flatMap(_.advance), Some(progress(2, 2)))
    assert(progress(2, 2).isComplete)
    assertEquals(progress(2, 2).advance, None)
    assertEquals(progress(1, 2).completed, right(ParticipantCount.of(1)))
    assertEquals(progress(1, 2).total, right(ParticipantCount.of(2)))
    assertEquals(
      PreviewProgress.start(right(PreviewCandidates.of(0, 0, 0, 0L, 0, 0, None))).advance,
      None
    )
  }
