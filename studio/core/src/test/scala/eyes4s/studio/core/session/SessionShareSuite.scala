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

import cats.effect.{IO, Resource}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.DocumentSamples.t1
import eyes4s.studio.core.document.SourceRole

import java.nio.charset.StandardCharsets.UTF_8

/** A session copies its project as last saved into an empty bundle: an
  * export bundle's project snapshot (ticket S9.5).
  */
class SessionShareSuite extends SessionConformance:

  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  private val fixations = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,1,2\n")
  private val trials    = bytes("participant,phase,trial\nP01,Encoding,enc_01\n")
  private val document  = BundleSamples.withSources(withoutJobs(t1), fixations, trials)

  private val inputs: Vector[InputEntry] =
    Vector(SourceRole.Fixations -> fixations, SourceRole.Trials -> trials).map { (role, b) =>
      val s = document.datasets.last.sources.entries.find(_.role == role).get
      BundleSamples.right(
        InputEntry.of(
          InputKind.Source(role),
          s.path.value.split('/').last,
          s.bytes,
          b.length.toLong
        )
      )
    }

  private def saved: IO[ProjectSession[IO]] =
    for
      store <- InMemoryProjectStore.create[IO]
      // Listed at their true lengths, so importing them stores their bytes.
      session <- ok(
        ProjectSession.create(store, alice, document, SharingOptions.complete, inputs)
      )
      _ <- ok(
        session.importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixations)
      )
      _ <- ok(session.importInput(InputKind.Source(SourceRole.Trials), "trials.csv", trials))
      _ <- ok(session.save)
    yield session

  test("the copy opens as the same project, with its inputs") {
    for
      session <- saved
      copy    <- InMemoryProjectStore.create[IO]
      digest  <- ok(session.share(copy, SharingOptions.complete))
      _       <- ok(session.close)
      opened  <- ok(ProjectSession.open(copy, bob))
      doc     <- opened.session.document
      back    <- opened.session.readInput(
        document.datasets.last.sources.entries.find(_.role == SourceRole.Fixations).get
      )
      _ <- ok(opened.session.close)
    yield
      assertEquals(doc, withoutJobs(document))
      assertEquals(back.map(_.toVector), Right(fixations.toVector))
      assert(digest.hex.nonEmpty)
  }

  test("a copy into a bundle that is not empty is refused, and the copy is unlocked") {
    for
      session <- saved
      copy    <- InMemoryProjectStore.create[IO]
      _       <- ok(session.share(copy, SharingOptions.complete))
      again   <- session.share(copy, SharingOptions.complete)
      lock    <- copy.acquire(alice)
      _       <- ok(session.close)
    yield
      again match
        case Left(SessionError.Bundle(_, BundleError.TargetNotEmpty(true, _))) => ()
        case other => fail(s"expected the copy to be refused, got $other")
      assert(lock.isRight, lock)
  }
