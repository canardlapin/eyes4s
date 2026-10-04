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

package eyes4s.studio.core.backend

import cats.Functor
import cats.effect.{Concurrent, Deferred, Ref}
import cats.syntax.all.*
import eyes4s.studio.core.preview.PreviewEvent
import fs2.{Pipe, Pull, RaiseThrowable, Stream, text}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder}

/** How a client reaches a [[StudyBackend]] (DESIGN_SPEC section 13, S0.9):
  * one enveloped request in, the frames answering it out, each carrying the
  * request's id. [[BackendTransport.inProcess]] calls the backend directly;
  * [[BackendTransport.loopback]] goes through the sidecar's wire format and
  * server ([[WireFormat]], [[SidecarServer]]). [[RemoteStudyBackend]] turns
  * any transport back into a `StudyBackend`, and `TransportConformanceSuite`
  * requires every transport to answer as the backend does in process.
  */
trait BackendTransport[F[_]]:
  def exchange(request: Envelope[BackendRequest]): Stream[F, Envelope[ServerFrame]]

object BackendTransport:

  /** The backend in the same process: [[StudyBackend.handle]], no encoding. */
  def inProcess[F[_]: Functor](backend: StudyBackend[F]): BackendTransport[F] =
    request => StudyBackend.handle(backend)(request)

  /** Each exchange is one connection to `server`, a byte pipe speaking the
    * [[WireFormat]]: [[SidecarServer.serve]] in a test, or a process's
    * stdin/stdout or a WebSocket in a host.
    */
  def loopback[F[_]: RaiseThrowable](server: Pipe[F, Byte, Byte]): BackendTransport[F] =
    request =>
      Stream
        .emit(request)
        .through(WireFormat.encode[F, BackendRequest])
        .through(server)
        .through(WireFormat.decode[F, ServerFrame]())

/** Why a transport could not carry an exchange: a defect of the transport or
  * of the peer, never a refusal (which is a [[BackendError]]). Every case
  * names its operands.
  */
enum TransportError derives CanEqual:
  /** A line that is not an envelope of the expected kind; `excerpt` is its
    * start, at most [[WireFormat.ExcerptLength]] characters.
    */
  case Malformed(excerpt: String, reason: String)

  /** A request line from which not even the request id can be read. The
    * server cannot answer it, so it ends the connection.
    */
  case Unidentifiable(excerpt: String, reason: String)

  /** A line longer than `limit` characters; it is not buffered further and
    * the connection ends.
    */
  case LineTooLong(limit: Int)

  /** A frame answering another request than the one sent. */
  case WrongRequest(expected: RequestId, found: RequestId)

  /** A frame from a peer speaking another major version. */
  case Incompatible(found: ProtocolVersion, supported: ProtocolVersion)

  /** A request that ended without its one response. */
  case Unanswered(request: BackendRequest)

  /** A frame that does not answer the request (or a second response). */
  case Unexpected(request: BackendRequest, frame: ServerFrame)

  /** A refusal of a request the protocol answers unconditionally. */
  case Refusal(request: BackendRequest, error: BackendError)

  def message: String = this match
    case Malformed(excerpt, reason)      => s"Not a protocol frame ($reason): $excerpt"
    case Unidentifiable(excerpt, reason) =>
      s"A request line without a readable id ($reason), so the connection ends: $excerpt"
    case LineTooLong(limit) =>
      s"A line is longer than $limit characters, so the connection ends."
    case WrongRequest(e, f) => s"Expected a frame for request ${e.value}, got ${f.value}."
    case Incompatible(f, s) =>
      s"The peer speaks protocol ${f.render}; this client speaks ${s.render}."
    case Unanswered(request)    => s"No response to $request."
    case Unexpected(request, f) => s"$f does not answer $request."
    case Refusal(request, e)    => s"$request cannot be refused, but was: ${e.message}"

/** A transport defect raised in the effect. */
final case class TransportFailure(error: TransportError) extends RuntimeException(error.message)

/** The sidecar's wire format (docs/studio/PORTING.md): one JSON envelope per
  * line, UTF-8, `\n`-terminated (NDJSON). JSON text escapes newlines inside
  * strings, so a line is exactly one envelope. Over a WebSocket, each text
  * message is one line without its terminator.
  */
object WireFormat:

  /** The longest line either side accepts, in characters (16 MiB). */
  val MaxLineLength: Int = 16 * 1024 * 1024

  // Preserve numeric text on Scala.js, too: native JSON.parse can round a
  // fractional number into a valid integer before a count decoder sees it.
  private val parser = io.circe.jawn.JawnParser(MaxLineLength)

  /** How much of a bad line an error quotes. */
  val ExcerptLength: Int = 200

  def excerpt(line: String): String =
    if line.length <= ExcerptLength then line else line.take(ExcerptLength) + "…"

  def line[A: Encoder](envelope: Envelope[A]): String = envelope.asJson.noSpaces + "\n"

  def parse[A: Decoder](line: String): Either[TransportError, Envelope[A]] =
    parser
      .decode[Envelope[A]](line)
      .leftMap(e => TransportError.Malformed(excerpt(line), e.getMessage))

  /** The request id of a line that is JSON with an `id`, whatever its body. */
  def requestId(line: String): Option[RequestId] =
    parser
      .parse(line)
      .toOption
      .flatMap(_.hcursor.downField("id").as[RequestId].toOption)

  def encode[F[_], A: Encoder]: Pipe[F, Envelope[A], Byte] =
    _.map(line(_)).through(text.utf8.encode)

  /** Bytes in any chunking to non-empty lines; a line longer than `limit`
    * fails the stream with [[TransportError.LineTooLong]].
    */
  def lines[F[_]: RaiseThrowable](limit: Int): Pipe[F, Byte, String] =
    _.through(text.utf8.decode)
      .through(text.linesLimited(limit))
      .handleErrorWith {
        case e: text.LineTooLongException =>
          Stream.raiseError(TransportFailure(TransportError.LineTooLong(e.max)))
        case e => Stream.raiseError(e)
      }
      .filter(_.nonEmpty)
      // linesLimited bounds only an unterminated tail; a terminated line
      // longer than the limit is refused here.
      .flatMap(l =>
        if l.length > limit then
          Stream.raiseError(TransportFailure(TransportError.LineTooLong(limit)))
        else Stream.emit(l)
      )

  /** Bytes to envelopes; a malformed or overlong line fails the stream. */
  def decode[F[_]: RaiseThrowable, A: Decoder](
      limit: Int = MaxLineLength
  ): Pipe[F, Byte, Envelope[A]] =
    _.through(lines(limit))
      .flatMap(l => parse[A](l).fold(e => Stream.raiseError(TransportFailure(e)), Stream.emit))

/** The server end of the IPC sidecar: a JVM process serving a backend to a
  * shell in another runtime (Electron, Tauri, a browser).
  */
object SidecarServer:

  private final case class Live[F[_]](stop: Deferred[F, Unit], done: Deferred[F, Unit])

  /** Requests in, frames out, in the [[WireFormat]], on one connection.
    *
    *  - Requests are served concurrently, so a subscription does not hold up
    *    the requests after it; frames of different requests interleave, and
    *    each carries its request's id.
    *  - `Unsubscribe(id)` ends subscription `id`; its `Unsubscribed` response
    *    follows the subscription's last frame.
    *  - A `Subscribe` whose id is already a live subscription on this
    *    connection is refused with [[BackendError.DuplicateSubscription]];
    *    the live one goes on.
    *  - A malformed line whose id can be read is refused under that id with
    *    [[BackendError.Malformed]]; one without a readable id, or longer than
    *    `limit`, ends the connection with a [[TransportFailure]].
    */
  def serve[F[_]: Concurrent](
      backend: StudyBackend[F],
      limit: Int = WireFormat.MaxLineLength
  ): Pipe[F, Byte, Byte] = in =>
    Stream.eval(Ref.of[F, Map[RequestId, Live[F]]](Map.empty)).flatMap { live =>
      def frame(id: RequestId, response: BackendResponse): Envelope[ServerFrame] =
        Envelope(id, ServerFrame.Response(response))

      def subscription(request: Envelope[BackendRequest]): F[Stream[F, Envelope[ServerFrame]]] =
        (Deferred[F, Unit], Deferred[F, Unit]).tupled.flatMap { (stop, done) =>
          val mine = Live(stop, done)
          live
            .modify { current =>
              if current.contains(request.id) then (current, false)
              else (current.updated(request.id, mine), true)
            }
            .map {
              case false =>
                Stream.emit(
                  frame(
                    request.id,
                    BackendResponse.Refused(BackendError.DuplicateSubscription(request.id))
                  )
                )
              case true =>
                StudyBackend
                  .handle(backend)(request)
                  .interruptWhen(stop.get.map(_.asRight[Throwable]))
                  // Remove only this subscription's own entry.
                  .onFinalize(
                    live.update(m =>
                      if m.get(request.id).contains(mine) then m - request.id else m
                    ) >>
                      done.complete(()).void
                  )
            }
        }

      def unsubscribe(id: RequestId, target: RequestId): Stream[F, Envelope[ServerFrame]] =
        Stream.eval(live.get.map(_.get(target))).flatMap {
          case Some(l) =>
            Stream
              .eval(l.stop.complete(()) >> l.done.get)
              .as(frame(id, BackendResponse.Unsubscribed(target, true)))
          case None => Stream.emit(frame(id, BackendResponse.Unsubscribed(target, false)))
        }

      def route(line: String): F[Stream[F, Envelope[ServerFrame]]] =
        WireFormat.parse[BackendRequest](line) match
          case Right(request) if request.version.major != ProtocolVersion.Current.major =>
            Concurrent[F].pure(StudyBackend.handle(backend)(request))
          case Right(request @ Envelope(_, _, BackendRequest.Subscribe(_))) =>
            subscription(request)
          case Right(Envelope(_, id, BackendRequest.Unsubscribe(target))) =>
            Concurrent[F].pure(unsubscribe(id, target))
          case Right(request) =>
            Concurrent[F].pure(StudyBackend.handle(backend)(request))
          case Left(TransportError.Malformed(excerpt, why)) =>
            WireFormat.requestId(line) match
              case Some(id) =>
                Concurrent[F].pure(
                  Stream.emit(
                    frame(id, BackendResponse.Refused(BackendError.Malformed(excerpt, why)))
                  )
                )
              case None =>
                Concurrent[F].raiseError(
                  TransportFailure(TransportError.Unidentifiable(excerpt, why))
                )
          case Left(other) => Concurrent[F].raiseError(TransportFailure(other))

      in.through(WireFormat.lines(limit))
        .evalMap(route)
        .parJoinUnbounded
        .through(WireFormat.encode[F, ServerFrame])
    }

/** A [[StudyBackend]] reached through a [[BackendTransport]]: the client side
  * of the protocol. Refusals come back as values, as in process; a transport
  * defect fails the effect with a [[TransportFailure]].
  *
  * `subscribe` first asks for the job, so an unknown job is refused before a
  * stream is returned; the subscription itself starts when the stream runs.
  */
object RemoteStudyBackend:

  def apply[F[_]: Concurrent](transport: BackendTransport[F]): F[StudyBackend[F]] =
    Ref.of[F, Long](0L).map(new Remote(transport, _))

  private final class Remote[F[_]: Concurrent](
      transport: BackendTransport[F],
      ids: Ref[F, Long]
  ) extends StudyBackend[F]:
    import BackendRequest as Q
    import BackendResponse as A

    private def failure[V](error: TransportError): F[V] =
      Concurrent[F].raiseError(TransportFailure(error))

    private def frames(request: BackendRequest): Stream[F, ServerFrame] =
      Stream.eval(ids.getAndUpdate(_ + 1).map(RequestId(_))).flatMap { id =>
        transport.exchange(Envelope(id, request)).evalMap { e =>
          if e.id != id then failure(TransportError.WrongRequest(id, e.id))
          else if e.version.major != ProtocolVersion.Current.major then
            failure(TransportError.Incompatible(e.version, ProtocolVersion.Current))
          else Concurrent[F].pure(e.body)
        }
      }

    private def respond(request: BackendRequest): F[BackendResponse] =
      frames(request).compile.toVector.flatMap {
        case Vector(ServerFrame.Response(r)) => Concurrent[F].pure(r)
        case Vector()                        => failure(TransportError.Unanswered(request))
        case Vector(ServerFrame.Response(_), extra, _*) =>
          failure(TransportError.Unexpected(request, extra))
        case other => failure(TransportError.Unexpected(request, other.head))
      }

    private def ask[V](request: BackendRequest)(
        pick: PartialFunction[BackendResponse, V]
    ): F[Either[BackendError, V]] =
      respond(request).flatMap {
        case A.Refused(e)             => Concurrent[F].pure(Left(e))
        case r if pick.isDefinedAt(r) => Concurrent[F].pure(Right(pick(r)))
        case r => failure(TransportError.Unexpected(request, ServerFrame.Response(r)))
      }

    private def always[V](
        request: BackendRequest
    )(pick: PartialFunction[BackendResponse, V]): F[V] =
      ask(request)(pick).flatMap(
        _.fold(e => failure(TransportError.Refusal(request, e)), _.pure[F])
      )

    def admission(dataset: DatasetRevision): F[Either[BackendError, AdmissionSummary]] =
      ask(Q.Admission(dataset)) { case A.Admission(s) => s }

    def ledger(
        dataset: DatasetRevision,
        page: PageRequest
    ): F[Either[BackendError, LedgerPage]] =
      ask(Q.Ledger(dataset, page)) { case A.Ledger(p) => p }

    def preview(revision: AnalysisRevision): F[Either[BackendError, PreviewSummary]] =
      ask(Q.Preview(revision)) { case A.Preview(s) => s }

    def trialFixations(
        revision: AnalysisRevision,
        trial: TrialKey
    ): F[Either[BackendError, TrialFixations]] =
      ask(Q.TrialFixationsOf(revision, trial)) { case A.TrialFixationsOf(f) => f }

    def trialPreview(
        revision: AnalysisRevision,
        trial: TrialKey
    ): F[Either[BackendError, TrialPreview]] =
      ask(Q.TrialPreviewOf(revision, trial)) { case A.TrialPreviewOf(p) => p }

    def sourceRecords(
        revision: AnalysisRevision,
        from: Int,
        count: Int
    ): F[Either[BackendError, SourceRecordPage]] =
      ask(Q.SourceRecordsOf(revision, from, count)) { case A.SourceRecordsOf(p) => p }

    def previewRows(
        revision: AnalysisRevision,
        page: PageRequest
    ): F[Either[BackendError, PreviewPage]] =
      ask(Q.PreviewRows(revision, page)) { case A.PreviewRows(p) => p }

    private def previewEvents(
        request: BackendRequest
    ): Stream[F, Either[BackendError, PreviewEvent]] =
      def events(tail: Stream[F, ServerFrame]): Stream[F, Either[BackendError, PreviewEvent]] =
        tail
          .evalMap {
            case ServerFrame.Preview(event)             => Concurrent[F].pure(Right(event))
            case ServerFrame.Response(A.Refused(error)) => Concurrent[F].pure(Left(error))
            case other => failure(TransportError.Unexpected(request, other))
          }
          .takeThrough(_.isRight)
      frames(request).pull.uncons1.flatMap {
        case None => Pull.raiseError(TransportFailure(TransportError.Unanswered(request)))
        case Some((ServerFrame.Response(A.Refused(error)), _))     => Pull.output1(Left(error))
        case Some((ServerFrame.Response(A.PreviewAccepted), tail)) =>
          tail.pull.uncons1.flatMap {
            case None => Pull.raiseError(TransportFailure(TransportError.Unanswered(request)))
            case Some((head, rest)) => events(Stream.emit(head) ++ rest).pull.echo
          }
        case Some((other, _)) =>
          Pull.raiseError(TransportFailure(TransportError.Unexpected(request, other)))
      }.stream

    def previewCounting(
        revision: AnalysisRevision,
        budget: eyes4s.studio.core.preview.PreviewBudget
    ): Stream[F, Either[BackendError, eyes4s.studio.core.preview.PreviewEvent]] =
      previewEvents(Q.PreviewCounting(revision, budget))

    def continuePreview(
        preview: eyes4s.studio.core.preview.PreviewId,
        budget: eyes4s.studio.core.preview.PreviewBudget
    ): Stream[F, Either[BackendError, eyes4s.studio.core.preview.PreviewEvent]] =
      previewEvents(Q.ContinuePreview(preview, budget))

    def runs: F[Vector[RunSummary]] = always(Q.Runs) { case A.Runs(r) => r }

    def submit(revision: AnalysisRevision): F[Either[BackendError, JobStatus]] =
      ask(Q.Submit(revision)) { case A.Job(s) => s }

    def submitPreview(
        ready: eyes4s.studio.core.preview.PreviewReady
    ): F[Either[BackendError, JobStatus]] =
      ask(Q.SubmitPreview(ready)) { case A.Job(s) => s }

    def jobs: F[Vector[JobStatus]] = always(Q.Jobs) { case A.Jobs(j) => j }

    def job(id: JobId): F[Either[BackendError, JobStatus]] =
      ask(Q.Job(id)) { case A.Job(s) => s }

    def subscribe(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]] =
      job(id).map(_.map { _ =>
        frames(Q.Subscribe(id)).flatMap {
          case ServerFrame.Event(e) => Stream.emit(e)
          case other                =>
            Stream.raiseError[F](
              TransportFailure(TransportError.Unexpected(Q.Subscribe(id), other))
            )
        }
      })

    def cancel(id: JobId): F[Either[BackendError, JobStatus]] =
      ask(Q.Cancel(id)) { case A.Job(s) => s }

    def outcome(id: JobId): F[Either[BackendError, Option[JobOutcome]]] =
      ask(Q.Outcome(id)) { case A.Outcome(j, o) if j == id => o }

    def result(run: RunId): F[Either[BackendError, ResultSummary]] =
      ask(Q.Result(run)) { case A.Result(s) => s }

    def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]] =
      ask(Q.Queries(run, page)) { case A.Queries(p) => p }

    def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
      ask(Q.Inspect(run, address)) { case A.Inspected(i) => i }

    def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
      ask(Q.ProvenanceOf(run, address)) { case A.ProvenanceOf(p) => p }
