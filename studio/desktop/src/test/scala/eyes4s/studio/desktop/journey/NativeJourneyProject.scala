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

package eyes4s.studio.desktop.journey

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.FileProjectStore
import eyes4s.studio.desktop.runtime.SessionPort
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** An owned real project with actual source and image bytes for both routes. */
object NativeJourneyProject:
  import NativeCommandJourneyFixture.get
  private val owner = get(LockOwner.of("Native golden interaction journey"))

  def open(
      path: Path,
      document: StudioDocument,
      inputs: NativeCommandJourneyFixture.Inputs
  ): Resource[IO, SessionPort] =
    for
      store   <- Resource.eval(FileProjectStore.at[IO](path))
      entries <- Resource.eval(
        Resource.make(store.acquire(owner).map(get))(lock => store.release(lock).map(get)).use {
          lock =>
            for
              sources <- inputs.entries.traverse { (source, data) =>
                ProjectBundle
                  .importInput(
                    store,
                    lock,
                    InputKind.Source(source.role),
                    source.path.value.split('/').last,
                    IArray.from(data)
                  )
                  .map(get)
              }
              files <- IO.blocking {
                val folder = NativeCommandJourneyFixture.golden.resolve("stimuli")
                val stream = Files.list(folder)
                try
                  stream.iterator.asScala
                    .filter(p =>
                      Files.isRegularFile(p) && p.getFileName.toString.endsWith(".png")
                    )
                    .toVector
                    .sortBy(_.getFileName.toString)
                finally stream.close()
              }
              images <- files.traverse { file =>
                IO.blocking(IArray.from(Files.readAllBytes(file)))
                  .flatMap(data =>
                    ProjectBundle
                      .importInput(
                        store,
                        lock,
                        InputKind.StimulusImage,
                        file.getFileName.toString,
                        data
                      )
                      .map(get)
                  )
              }
            yield sources ++ images
        }
      )
      session <- Resource.make(
        ProjectSession.create(store, owner, document, SharingOptions.complete, entries).map(get)
      )(project => project.close.map(get))
      // The port owns its transport; the resource above owns the project session.
      port <- Resource.make(IO.blocking(SessionPort.start(session)))(port =>
        IO.blocking(port.close())
      )
    yield port
