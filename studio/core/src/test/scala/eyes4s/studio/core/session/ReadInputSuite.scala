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

/** A session reads a dataset source's bytes back from the bundle (the
  * column-mapping pane's re-map): an imported input reads back exactly, a
  * source whose bytes the bundle does not hold is a typed store error naming
  * the file.
  */
class ReadInputSuite extends SessionConformance:

  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  private val fixations = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,1,2\n")
  private val trials    = bytes("participant,phase,trial\nP01,Encoding,enc_01\n")
  private val document  = BundleSamples.withSources(withoutJobs(t1), fixations, trials)

  private def source(role: SourceRole) =
    document.datasets.last.sources.entries.find(_.role == role).get

  test("an imported source reads back byte for byte; one not imported is Missing") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, document)
        _       <- ok(
          session.importInput(
            InputKind.Source(SourceRole.Fixations),
            "fixations.csv",
            fixations
          )
        )
        read    <- session.readInput(source(SourceRole.Fixations))
        missing <- session.readInput(source(SourceRole.Trials))
        _       <- ok(session.close)
      yield
        assertEquals(read.map(_.toVector), Right(fixations.toVector))
        missing match
          case Left(SessionError.Store(operation, StoreError.Missing(path))) =>
            assertEquals(operation, "read inputs/trials.csv")
            assertEquals(path, ProjectBundle.inputPath(source(SourceRole.Trials)).toOption.get)
          case other => fail(s"expected a missing input, got $other")
    }
  }
