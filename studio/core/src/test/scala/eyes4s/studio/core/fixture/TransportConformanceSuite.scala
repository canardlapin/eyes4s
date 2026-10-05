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
import fs2.Stream

import scala.concurrent.duration.*

/** TransportConformanceSuite (S0.9): the whole [[BackendConformanceSuite]]
  * run on a [[RemoteStudyBackend]] over a transport, plus frame-for-frame
  * agreement of the transport with [[StudyBackend.handle]] in process. Every
  * [[BackendTransport]] a host adds extends it.
  */
abstract class TransportConformanceSuite extends BackendConformanceSuite:

  /** The transport under test, to `backend`. */
  def transport(backend: StudyBackend[IO]): BackendTransport[IO]

  def subject: IO[BackendConformanceSuite.Subject] =
    FakeBackendConformanceSuite.subject(fake => RemoteStudyBackend[IO](transport(fake)))

  protected def page(offset: Int, size: Int): PageRequest =
    PageRequest.of(offset, size).fold(e => fail(e.message), identity)

  protected def fresh: IO[FakeStudyBackend[IO]] = FakeStudyBackend.create[IO](StoryMoment.T2)

  /** Requests of every kind that answer without a running job. */
  protected def requests(fake: StudyBackend[IO]): IO[Vector[Envelope[BackendRequest]]] =
    fake.queries(RunId(7), page(0, 1)).map { rows =>
      val row     = rows.toOption.get.rows.head
      val address = ResultAddress.ContrastRow(1, row.query)
      Vector(
        BackendRequest.Admission(DatasetRevision(3)),
        BackendRequest.Ledger(DatasetRevision(3), page(900, 100)),
        BackendRequest.Preview(AnalysisRevision(5)),
        BackendRequest.PreviewRows(AnalysisRevision(5), page(0, 5)),
        BackendRequest.Runs,
        BackendRequest.Jobs,
        BackendRequest.Job(JobId(9999)),
        BackendRequest.Outcome(JobId(9999)),
        BackendRequest.Cancel(JobId(9999)),
        BackendRequest.Result(RunId(7)),
        BackendRequest.Queries(RunId(7), page(10, 10)),
        BackendRequest.Inspect(RunId(7), address),
        BackendRequest.ProvenanceOf(RunId(7), address),
        BackendRequest.Result(RunId(9999)),
        BackendRequest.Subscribe(JobId(9999)),
        BackendRequest.Unsubscribe(RequestId(12345))
      ).zipWithIndex.map((r, i) => Envelope(RequestId(i.toLong), r))
    }

  test("every request's frames over the transport equal the in-process frames") {
    for
      template <- fresh
      sent     <- requests(template)
      direct   <- sent.traverse(r => fresh.flatMap(StudyBackend.handle(_)(r).compile.toVector))
      carried  <- sent.traverse(r => fresh.flatMap(transport(_).exchange(r).compile.toVector))
    yield
      assertEquals(carried, direct)
      assert(direct.forall(_.size == 1), direct)
  }

  test("the remote backend answers each method as the backend itself") {
    def both[A](call: StudyBackend[IO] => IO[A]): IO[(A, A)] =
      for
        remote <- fresh.flatMap(fake => RemoteStudyBackend[IO](transport(fake)))
        via    <- call(remote)
        itself <- fresh.flatMap(call)
      yield (via, itself)
    val calls = Vector[StudyBackend[IO] => IO[Any]](
      _.admission(DatasetRevision(3)),
      _.admission(DatasetRevision(9999)),
      _.preview(AnalysisRevision(5)),
      _.runs,
      _.jobs,
      _.result(RunId(7)),
      _.outcome(JobId(9999)),
      _.submit(AnalysisRevision(5)),
      b => b.submit(AnalysisRevision(5)) >> b.submit(AnalysisRevision(5)),
      b => b.submit(AnalysisRevision(5)).flatMap(s => b.cancel(s.toOption.get.job)),
      _.subscribe(JobId(9999)).map(_.void)
    )
    calls.traverse(both(_)).map(_.foreach((via, itself) => assertEquals(via, itself)))
  }

/** The in-process transport: [[StudyBackend.handle]] with no encoding. */
class InProcessTransportSuite extends TransportConformanceSuite:
  def transport(backend: StudyBackend[IO]): BackendTransport[IO] =
    BackendTransport.inProcess(backend)

  /** A transport whose every frame is rewritten by `f`. */
  private def rewriting(
      backend: StudyBackend[IO]
  )(f: Envelope[ServerFrame] => Envelope[ServerFrame]): BackendTransport[IO] =
    request => StudyBackend.handle(backend)(request).map(f)

  private def failure(io: IO[Any]): IO[TransportError] =
    io.attempt.map {
      case Left(TransportFailure(e)) => e
      case other                     => fail(s"expected a TransportFailure, got $other")
    }

  test("a frame answering another request is a transport defect, named") {
    for
      fake   <- fresh
      remote <- RemoteStudyBackend[IO](rewriting(fake)(_.copy(id = RequestId(41))))
      error  <- failure(remote.runs)
    yield assertEquals(error, TransportError.WrongRequest(RequestId(0), RequestId(41)))
  }

  test("a second response to one request is a transport defect, named") {
    val twice: BackendTransport[IO] => BackendTransport[IO] =
      t => request => t.exchange(request).flatMap(f => Stream(f, f))
    for
      fake   <- fresh
      remote <- RemoteStudyBackend[IO](twice(BackendTransport.inProcess(fake)))
      runs   <- fake.runs
      error  <- failure(remote.runs)
    yield assertEquals(
      error,
      TransportError.Unexpected(
        BackendRequest.Runs,
        ServerFrame.Response(BackendResponse.Runs(runs))
      )
    )
  }

  test("a frame of another minor version is a transport defect, named") {
    val older = ProtocolVersion(1, 2)
    for
      fake   <- fresh
      remote <- RemoteStudyBackend[IO](rewriting(fake)(_.copy(version = older)))
      error  <- failure(remote.jobs)
    yield assertEquals(error, TransportError.Incompatible(older, ProtocolVersion.Current))
  }

  test("a frame of another major version is a transport defect, named") {
    val future = ProtocolVersion(2, 0)
    for
      fake   <- fresh
      remote <- RemoteStudyBackend[IO](rewriting(fake)(_.copy(version = future)))
      error  <- failure(remote.jobs)
    yield assertEquals(error, TransportError.Incompatible(future, ProtocolVersion.Current))
  }

  test("a refusal is a value where the protocol allows one and a defect where it does not") {
    val refused = BackendError.UnsupportedVersion(ProtocolVersion(1, 0), ProtocolVersion(1, 0))
    val refuse  = (e: Envelope[ServerFrame]) =>
      e.copy(body = ServerFrame.Response(BackendResponse.Refused(refused)))
    for
      fake      <- fresh
      remote    <- RemoteStudyBackend[IO](rewriting(fake)(refuse))
      admission <- remote.admission(DatasetRevision(3))
      runs      <- failure(remote.runs)
    yield
      assertEquals(admission, Left(refused))
      assertEquals(runs, TransportError.Refusal(BackendRequest.Runs, refused))
  }

  test("a mismatched or missing response is a transport defect, named") {
    val jobs = (e: Envelope[ServerFrame]) =>
      e.copy(body = ServerFrame.Response(BackendResponse.Jobs(Vector.empty)))
    for
      fake       <- fresh
      mismatched <- RemoteStudyBackend[IO](rewriting(fake)(jobs))
      wrong      <- failure(mismatched.runs)
      silent     <- RemoteStudyBackend[IO](_ => Stream.empty)
      missing    <- failure(silent.runs)
    yield
      assertEquals(
        wrong,
        TransportError.Unexpected(
          BackendRequest.Runs,
          ServerFrame.Response(BackendResponse.Jobs(Vector.empty))
        )
      )
      assertEquals(missing, TransportError.Unanswered(BackendRequest.Runs))
  }

/** The loopback IPC transport: every exchange is encoded to NDJSON bytes,
  * served by [[SidecarServer.serve]] and decoded again.
  */
class LoopbackTransportSuite extends TransportConformanceSuite:
  def transport(backend: StudyBackend[IO]): BackendTransport[IO] =
    BackendTransport.loopback(SidecarServer.serve(backend))

  // --- Exact versions (bd-01M3JH3492J21SKMYYNZM93118) ----------------------------------

  /** One raw line through the sidecar, its answers decoded. */
  private def served(line: String): IO[Vector[Envelope[ServerFrame]]] =
    fresh.flatMap(fake =>
      Stream
        .emit(line + "\n")
        .through(fs2.text.utf8.encode)
        .through(SidecarServer.serve(fake))
        .through(WireFormat.decode[IO, ServerFrame]())
        .compile
        .toVector
    )

  private val v12 = ProtocolVersion(1, 2)

  test("a 1.2 client's request is refused as UnsupportedVersion, naming both versions") {
    // A 1.2 Admission request decodes at 1.9, and a 1.2 body may not: both are
    // refused by version, under the request's id, never as malformed.
    val decodable =
      """{"version":{"major":1,"minor":2},"id":7,"body":{"Admission":{"dataset":3}}}"""
    val undecodable =
      """{"version":{"major":1,"minor":2},"id":8,"body":{"Retired":{"inventoryTrials":960}}}"""
    for
      a <- served(decodable)
      b <- served(undecodable)
    yield
      val refusal = (id: Long) =>
        Vector(
          Envelope(
            RequestId(id),
            ServerFrame.Response(
              BackendResponse.Refused(
                BackendError.UnsupportedVersion(v12, ProtocolVersion.Current)
              )
            )
          )
        )
      assertEquals(a, refusal(7))
      assertEquals(b, refusal(8))
  }

  test("a 1.2 backend's frame fails the client as Incompatible, naming both, not Malformed") {
    // A 1.2 admission summary: inventoryTrials/absent, which 1.3 replaced.
    val old =
      """{"version":{"major":1,"minor":2},"id":0,"body":{"Response":{"response":""" +
        """{"Admission":{"summary":{"dataset":3,"inventoryTrials":960,"absent":6}}}}}}"""
    val client = Stream
      .emit(old + "\n")
      .through(fs2.text.utf8.encode)
      .through(WireFormat.decode[IO, ServerFrame]())
      .compile
      .toVector
    client.attempt.map {
      case Left(TransportFailure(e)) =>
        assertEquals(e, TransportError.Incompatible(v12, ProtocolVersion.Current))
      case other => fail(s"expected Incompatible, got $other")
    }
  }

  test("a line of another version with no readable id ends the connection, named") {
    val line = """{"version":{"major":1,"minor":2},"body":{"Runs":{}}}"""
    served(line).attempt.map {
      case Left(TransportFailure(TransportError.Unidentifiable(excerpt, reason))) =>
        assertEquals(excerpt, line)
        assert(reason.contains("1.2"), reason)
      case other => fail(s"expected Unidentifiable, got $other")
    }
  }

  test("a line with no version but a readable id is refused as malformed under that id") {
    val line = """{"id":9,"body":{"Runs":{}}}"""
    served(line).map(frames =>
      frames.map(_.body) match
        case Vector(
              ServerFrame.Response(BackendResponse.Refused(BackendError.Malformed(e, _)))
            ) =>
          assertEquals(e, line)
          assertEquals(frames.map(_.id), Vector(RequestId(9)))
        case other => fail(s"expected Malformed, got $other")
    )
  }

  test("a newer minor is refused alike: versions are exact, not ordered") {
    val newer = ProtocolVersion.Current.copy(minor = ProtocolVersion.Current.minor + 1)
    val line  =
      s"""{"version":{"major":${newer.major},"minor":${newer.minor}},"id":3,"body":{"Runs":{}}}"""
    for frames <- served(line)
    yield assertEquals(
      frames.map(_.body),
      Vector(
        ServerFrame.Response(
          BackendResponse.Refused(
            BackendError.UnsupportedVersion(newer, ProtocolVersion.Current)
          )
        )
      )
    )
  }

  test("one connection serves many requests, each frame under its request's id") {
    for
      fake   <- fresh
      status <- fake.submit(AnalysisRevision(5)).map(_.toOption.get)
      _      <- fake.cancel(status.job)
      sent   <- requests(fake).map(
        _ :+ Envelope(RequestId(99), BackendRequest.Subscribe(status.job))
      )
      direct <- sent.flatTraverse(StudyBackend.handle(fake)(_).compile.toVector)
      served <- Stream
        .emits(sent)
        .through(WireFormat.encode[IO, BackendRequest])
        .chunkLimit(1)
        .unchunks
        .through(SidecarServer.serve(fake))
        .chunkLimit(3)
        .unchunks
        .through(WireFormat.decode[IO, ServerFrame]())
        .compile
        .toVector
    yield
      assertEquals(served.groupBy(_.id), direct.groupBy(_.id))
      assertEquals(served.map(_.id).distinct.size, sent.size)
  }

  test("a live subscription does not hold up the requests after it on its connection") {
    FakeBackendConformanceSuite.subject(IO.pure).flatMap { s =>
      for
        status <- s.backend.submit(s.draft).map(_.toOption.get)
        sent = Vector(
          Envelope(RequestId(1), BackendRequest.Subscribe(status.job)),
          Envelope(RequestId(2), BackendRequest.Runs)
        )
        frames <- Stream
          .emits(sent)
          .through(WireFormat.encode[IO, BackendRequest])
          .through(SidecarServer.serve(s.backend))
          .through(WireFormat.decode[IO, ServerFrame]())
          // The job finishes only once Runs is answered, so a server that
          // served requests one after another would never end.
          .evalTap(f => if f.id == RequestId(2) then s.finish(status.job) else IO.unit)
          .compile
          .toVector
          .timeout(30.seconds)
      yield
        assertEquals(frames.count(_.id == RequestId(2)), 1)
        frames.filter(_.id == RequestId(1)).last.body match
          case ServerFrame.Event(JobEvent.Finished(o)) => assertEquals(o.job, status.job)
          case other                                   => fail(s"last frame is $other")
    }
  }

  test("each envelope is exactly one line of the wire format") {
    for
      fake   <- fresh
      sent   <- requests(fake)
      frames <- sent.flatTraverse(StudyBackend.handle(fake)(_).compile.toVector)
    yield
      (sent.map(WireFormat.line(_)) ++ frames.map(WireFormat.line(_))).foreach { line =>
        assert(line.endsWith("\n") && line.count(_ == '\n') == 1, line)
      }
      frames.foreach(f =>
        assertEquals(WireFormat.parse[ServerFrame](WireFormat.line(f)), Right(f))
      )
  }

  /** The frames `lines` get from one connection to a fresh fake, or its failure. */
  private def serveLines(
      lines: Vector[String],
      limit: Int = WireFormat.MaxLineLength
  ): IO[Either[Throwable, Vector[Envelope[ServerFrame]]]] =
    fresh.flatMap { fake =>
      Stream
        .emits(lines)
        .through(fs2.text.utf8.encode)
        .through(SidecarServer.serve(fake, limit))
        .through(WireFormat.decode[IO, ServerFrame]())
        .compile
        .toVector
        .attempt
    }

  test("a malformed line with a readable id is refused under that id; the connection goes on") {
    // The current version, so the body is what is wrong (another version is
    // refused by version first).
    val v   = ProtocolVersion.Current
    val bad =
      s"""{"version":{"major":${v.major},"minor":${v.minor}},"id":5,"body":{"Runz":{}}}""" + "\n"
    val runs = WireFormat.line(Envelope(RequestId(6), BackendRequest.Runs: BackendRequest))
    serveLines(Vector(bad, runs)).map {
      case Right(frames) =>
        assertEquals(frames.map(_.id).sortBy(_.value), Vector(RequestId(5), RequestId(6)))
        frames.find(_.id == RequestId(5)).map(_.body) match
          case Some(
                ServerFrame.Response(
                  BackendResponse.Refused(BackendError.Malformed(excerpt, _))
                )
              ) =>
            assertEquals(excerpt, bad.stripSuffix("\n"))
          case other => fail(s"expected a Malformed refusal, got $other")
      case Left(e) => fail(s"the connection failed: $e")
    }
  }

  test(
    "a line without a readable id ends the connection, saying so, and quotes only its start"
  ) {
    val long = "{\"not\":\"" + ("x" * 1000) + "\"}"
    serveLines(Vector(long + "\n")).map {
      case Left(TransportFailure(e @ TransportError.Unidentifiable(excerpt, _))) =>
        assertEquals(excerpt, long.take(WireFormat.ExcerptLength) + "…")
        assert(e.message.contains("connection ends"), e.message)
      case other => fail(s"expected Unidentifiable, got $other")
    }
  }

  test("a line longer than the limit ends the connection with LineTooLong") {
    val runs = WireFormat.line(Envelope(RequestId(1), BackendRequest.Runs: BackendRequest))
    serveLines(Vector(runs, "x" * 200 + "\n"), limit = 64).map {
      case Left(TransportFailure(TransportError.LineTooLong(limit))) => assertEquals(limit, 64)
      case other => fail(s"expected LineTooLong, got $other")
    }
  }

  test(
    "a Subscribe reusing a live subscription's id is refused by name; the live one goes on"
  ) {
    FakeBackendConformanceSuite.subject(IO.pure).flatMap { s =>
      for
        status <- s.backend.submit(s.draft).map(_.toOption.get)
        sent = Vector(
          Envelope(RequestId(1), BackendRequest.Subscribe(status.job)),
          Envelope(RequestId(1), BackendRequest.Subscribe(status.job)),
          Envelope(RequestId(2), BackendRequest.Unsubscribe(RequestId(1)))
        )
        frames <- Stream
          .emits(sent)
          .through(WireFormat.encode[IO, BackendRequest])
          .through(SidecarServer.serve(s.backend))
          .through(WireFormat.decode[IO, ServerFrame]())
          .compile
          .toVector
          .timeout(30.seconds)
      yield
        val refused = BackendError.DuplicateSubscription(RequestId(1))
        assertEquals(
          frames.filter(_.body.isInstanceOf[ServerFrame.Response]).map(_.body).toSet,
          Set[ServerFrame](
            ServerFrame.Response(BackendResponse.Refused(refused)),
            // The first subscription kept its entry, so the Unsubscribe found
            // and ended it (and the connection could end at all).
            ServerFrame.Response(BackendResponse.Unsubscribed(RequestId(1), true))
          )
        )
        assert(refused.message.contains("Request 1"), refused.message)
    }
  }

  test(
    "Unsubscribe ends a live subscription; its response follows the subscription's last frame"
  ) {
    FakeBackendConformanceSuite.subject(IO.pure).flatMap { s =>
      for
        status <- s.backend.submit(s.draft).map(_.toOption.get)
        sent = Vector(
          Envelope(RequestId(1), BackendRequest.Subscribe(status.job)),
          Envelope(RequestId(2), BackendRequest.Unsubscribe(RequestId(1))),
          Envelope(RequestId(3), BackendRequest.Unsubscribe(RequestId(1))),
          Envelope(RequestId(4), BackendRequest.Unsubscribe(RequestId(77)))
        )
        // The job never finishes: only the Unsubscribe can end request 1.
        frames <- Stream
          .emits(sent.take(2))
          .through(WireFormat.encode[IO, BackendRequest])
          .through(SidecarServer.serve(s.backend))
          .through(WireFormat.decode[IO, ServerFrame]())
          .compile
          .toVector
          .timeout(30.seconds)
        later <- Stream
          .emits(sent.drop(2))
          .through(WireFormat.encode[IO, BackendRequest])
          .through(SidecarServer.serve(s.backend))
          .through(WireFormat.decode[IO, ServerFrame]())
          .compile
          .toVector
        outcome <- s.backend.outcome(status.job)
      yield
        assertEquals(
          frames.last,
          Envelope(
            RequestId(2),
            ServerFrame.Response(BackendResponse.Unsubscribed(RequestId(1), true)): ServerFrame
          )
        )
        assert(
          frames.init
            .forall(f => f.id == RequestId(1) && f.body.isInstanceOf[ServerFrame.Event]),
          frames
        )
        assertEquals(outcome, Right(None))
        assertEquals(
          later.map(_.body).toSet,
          Set[ServerFrame](
            ServerFrame.Response(BackendResponse.Unsubscribed(RequestId(1), false)),
            ServerFrame.Response(BackendResponse.Unsubscribed(RequestId(77), false))
          )
        )
    }
  }
