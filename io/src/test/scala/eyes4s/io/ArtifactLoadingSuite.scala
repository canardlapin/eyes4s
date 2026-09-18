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

package eyes4s.io

import cats.effect.kernel.Outcome
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref, Resource}
import eyes4s.codec.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** The shared effectful adapter over an in-memory `IO` source, on the JVM and
  * Scala.js: end-to-end resolution, cancellation in the middle of a read, and
  * failures raised inside the effect.
  */
class ArtifactLoadingSuite extends munit.FunSuite:
  import SavedGraphFixture.*

  private val saved = SavedGraphFixture.saved()

  /** Decoders that count their invocations and otherwise delegate. */
  private final class Counting extends ArtifactDecoders[StudyKey, Px]:
    var calls                       = 0
    private def tick[A](a: => A): A =
      calls += 1
      a
    def plan(d: Json)                                                = tick(decoders.plan(d))
    def input(d: Json)                                               = tick(decoders.input(d))
    def ledger(d: Json)                                              = tick(decoders.ledger(d))
    def result(d: Json)                                              = tick(decoders.result(d))
    def recording(d: Json, p: PayloadRef => Option[VerifiedPayload]) = tick(
      decoders.recording(d, p)
    )
    def recordingInput(d: Json) = tick(decoders.recordingInput(d))
    def temporalInput(
        d: Json,
        b: ArtifactRef[StudyInput[StudyKey, Px]] => Option[StudyInput[StudyKey, Px]]
    ) = tick(decoders.temporalInput(d, b))

  test("an in-memory effectful source resolves the saved graph") {
    ArtifactLoading
      .resolve(saved.address, request => IO(saved.source.read(request)), decoders)
      .map { outcome =>
        val resolved = outcome.fold(e => fail(s"$e"), identity)
        assertEquals(resolved.inputs.map(_._2.reference), Vector(input.reference))
        assertEquals(resolved.ledgers.map(_._2), Vector(ledger))
        assertEquals(
          resolved.results.map(_._2.encode),
          Vector(StudyResultCodecs.cosine[Px].codec.encode(result))
        )
      }
      .unsafeToFuture()
  }

  test("cancelling during a read runs the source's finalizers and decodes nothing") {
    val blocked = saved.manifest.entries(2).name
    val program = for
      started <- Deferred[IO, Unit]
      opened  <- Ref[IO].of(0)
      closed  <- Ref[IO].of(0)
      reads   <- Ref[IO].of(Vector.empty[ByteRequest])
      counted = new Counting
      source  = (request: ByteRequest) =>
        Resource.make(opened.update(_ + 1))(_ => closed.update(_ + 1)).use { _ =>
          reads.update(_ :+ request) *> (request match
            case ByteRequest.Entry(entry) if entry.name == blocked =>
              started.complete(()) *> IO.never
            case other => IO(saved.source.read(other)))
        }
      fiber   <- ArtifactLoading.resolve(saved.address, source, counted).start
      _       <- started.get
      _       <- fiber.cancel
      outcome <- fiber.join
      o       <- opened.get
      c       <- closed.get
      r       <- reads.get
      d = counted.calls
    yield
      assert(outcome match
        case Outcome.Canceled() => true
        case _                  => false)
      // The manifest and the entries up to and including the blocked one were opened.
      assertEquals(
        r,
        ByteRequest
          .Manifest(saved.address) +: saved.manifest.entries.take(3).map(ByteRequest.Entry(_))
      )
      assertEquals(o, 4)
      assertEquals(c, 4)
      assertEquals(d, 0)
    program.unsafeToFuture()
  }

  test("a read that fails inside the effect is reported as unreadable, naming the entry") {
    val failing = saved.manifest.entries(3).name
    ArtifactLoading
      .resolve(
        saved.address,
        {
          case ByteRequest.Entry(entry) if entry.name == failing =>
            IO.raiseError(new IllegalStateException("store offline"))
          case request => IO(saved.source.read(request))
        },
        decoders
      )
      .map(result =>
        assertEquals(
          result.left.map(_.toVector),
          Left(
            Vector(
              ResolveError.Unreadable(failing, "java.lang.IllegalStateException: store offline")
            )
          )
        )
      )
      .unsafeToFuture()
  }
