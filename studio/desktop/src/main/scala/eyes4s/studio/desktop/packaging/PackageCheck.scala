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

package eyes4s.studio.desktop.packaging

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.fixture.ExampleProject
import eyes4s.studio.desktop.StudioBuild
import eyes4s.studio.desktop.platform.{BundledExample, FileProjectStore, JavaFxFonts}
import io.circe.Json
import java.nio.file.{Files, Path}

/** A non-graphical check executed by the installed launcher and bundled JVM.
  * It verifies resources, copies the bundled project through the production
  * store and reopens it. It deliberately makes no native UI acceptance claim.
  */
object PackageCheck:
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] =
    value.flatMap(_.fold(e => IO.raiseError(new IllegalStateException(e.toString)), IO.pure))

  def check(output: Path): IO[Json] =
    for
      _ <- IO.blocking(Files.createDirectory(output))
      resources = JavaFxFonts.bundled.map(_.resource) :+
        "eyes4s/studio/desktop/notices/THIRD-PARTY-COMPONENTS.txt"
      _ <- IO.blocking {
        resources.foreach { name =>
          val stream = Option(getClass.getClassLoader.getResourceAsStream(name))
            .getOrElse(throw new IllegalStateException(s"Missing packaged resource: $name"))
          try require(stream.read() != -1, s"Empty packaged resource: $name")
          finally stream.close()
        }
      }
      source   <- ok(BundledExample.source)
      original <- ok(ProjectBundle.open(source))
      store    <- FileProjectStore.at[IO](output.resolve("memory-study.eyes"))
      owner    <- IO.fromEither(
        LockOwner.of("Package check").left.map(e => new IllegalStateException(e.message))
      )
      lock   <- ok(store.acquire(owner))
      copied <- ok(ExampleProject.openCopy(source, store, lock))
        .guarantee(store.release(lock).void)
      reopened <- ok(ProjectBundle.open(store))
      inputs   <- ProjectBundle.checkInputs(store, reopened.manifest)
      _        <- IO {
        require(reopened.document == original.document, "Reopened document changed")
        require(copied.project.document == reopened.document, "Copied document changed")
        require(reopened.science.verified, "Reopened science is not verified")
        require(
          inputs == reopened.manifest.inputs.map(InputStatus.Present(_)),
          "Packaged example has missing or changed inputs"
        )
      }
    yield Json.obj(
      "schema"   -> Json.fromInt(1),
      "status"   -> Json.fromString("passed"),
      "scope"    -> Json.fromString("bundled-runtime-resource-copy-reopen; no native UI claim"),
      "commit"   -> StudioBuild.commit.fold(Json.Null)(Json.fromString),
      "modified" -> Json.fromBoolean(StudioBuild.modified),
      "version"  -> Json.fromString(StudioBuild.version),
      "java_home"          -> Json.fromString(sys.props("java.home")),
      "java_version"       -> Json.fromString(sys.props("java.version")),
      "os_arch"            -> Json.fromString(sys.props("os.arch")),
      "inputs_verified"    -> Json.fromInt(inputs.size),
      "resources_verified" -> Json.fromInt(resources.size),
      "project"            -> Json.fromString(output.resolve("memory-study.eyes").toString)
    )

  def run(output: Path): Unit =
    val result = (for
      receipt <- check(output)
      _       <- IO.blocking(
        Files.writeString(output.resolve("package-check.json"), receipt.spaces2 + "\n")
      )
    yield receipt).attempt.unsafeRunSync()
    result match
      case Right(receipt) => println(receipt.spaces2)
      case Left(error)    =>
        System.err.println(s"Package check failed: ${error.getMessage}")
        sys.exit(1)
