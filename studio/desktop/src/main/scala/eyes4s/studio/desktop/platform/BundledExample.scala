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

package eyes4s.studio.desktop.platform

import cats.effect.IO
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.studio.core.bundle.*

/** The checked-in golden example is a classpath resource, independent of the
  * working directory. The source is loaded into a separate in-memory store
  * and only copies are ever opened for editing.
  */
object BundledExample:
  val resource: String = "eyes4s/studio/desktop/example.zip"

  def source: IO[Either[String, ProjectStore[IO]]] =
    val entries = IO
      .blocking {
        Option(getClass.getClassLoader.getResourceAsStream(resource))
          .toRight(s"Missing bundled example: $resource")
          .flatMap { input =>
            val zip = java.util.zip.ZipInputStream(input)
            try
              val files = Vector.newBuilder[(String, IArray[Byte])]
              var entry = zip.getNextEntry
              while entry != null do
                if !entry.isDirectory then
                  files += entry.getName -> IArray.unsafeFromArray(zip.readAllBytes())
                entry = zip.getNextEntry
              Right(files.result())
            finally zip.close()
          }
      }
      .handleError(e => Left(e.toString))
    (for
      files <- EitherT(entries)
      store <- EitherT.liftF[IO, String, ProjectStore[IO]](
        InMemoryProjectStore.create[IO].map(s => s: ProjectStore[IO])
      )
      owner <- EitherT.fromEither[IO](
        LockOwner.of("Bundled example reader").left.map(_.message)
      )
      lock <- EitherT(store.acquire(owner).map(_.left.map(_.message)))
      _    <- EitherT(
        files
          .traverse { (name, bytes) =>
            if name == ProjectStore.ManifestName then
              store.swapManifest(lock, None, bytes).map(_.left.map(_.message))
            else
              BundlePath.of(name) match
                case Left(error) => IO.pure(Left(error.message))
                case Right(path) => store.write(lock, path, bytes).map(_.left.map(_.message))
          }
          .map(_.sequence)
          .guarantee(store.release(lock).void)
      )
    yield store).value
